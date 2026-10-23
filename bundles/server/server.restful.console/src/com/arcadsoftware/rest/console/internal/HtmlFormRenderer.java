package com.arcadsoftware.rest.console.internal;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.restlet.data.Language;

import com.arcadsoftware.rest.BasicHtmlRenderer;
import com.arcadsoftware.rest.console.Category;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleMessage;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.ConsoleSet;
import com.arcadsoftware.rest.console.SectionId;

/**
 * Renders a console form (a list of {@link ConsoleField}) as static HTML5: no JavaScript, no CSS.
 *
 * <p>
 * Every action button posts the whole form to {@code <sectionPath>/<actionId>} using {@code formaction}.
 */
final class HtmlFormRenderer extends BasicHtmlRenderer {

	private final String sectionPath;
	private final String backLabel;

	private HtmlFormRenderer(Language language, String sectionPath, String backLabel) {
		super(language);
		String p = (sectionPath == null) ? "" : sectionPath; //$NON-NLS-1$
		while (p.endsWith("/")) { //$NON-NLS-1$
			p = p.substring(0, p.length() - 1);
		}
		this.sectionPath = p;
		this.backLabel = backLabel;
	}

	/**
	 * Render a form.
	 *
	 * @param fields      the form.
	 * @param title       the page title (section label).
	 * @param sectionPath the path of the section resource (e.g. "/admin/console/mysection"), used to build the actions URL.
	 * @param language    the response language (may be null).
	 * @param backLabel   the localized label of the "back" button, displayed only if the form has no visible action.
	 *                    If null, no back button is ever displayed (use it when the page is the section page itself).
	 */
	static String render(List<ConsoleField> fields, String title, String sectionPath, Language language, String backLabel) {
		return new HtmlFormRenderer(language, sectionPath, backLabel).page(fields, title, language);
	}

	/**
	 * Render the page returned when an action succeeded without returning any form.
	 *
	 * @param title       the page title (section label).
	 * @param message     the localized success message.
	 * @param sectionPath the path of the section resource, target of the "back" button.
	 * @param language    the response language (may be null).
	 * @param backLabel   the localized label of the "back" button (required).
	 */
	static String renderSuccess(String title, String message, String sectionPath, Language language, String backLabel) {
		return new BasicHtmlRenderer(language)
				.head(title)
				.h1(title)
				.status(message)
				.button(backLabel, sectionPath)
				.render();
	}

	/**
	 * Render the list of sections as a simple list of links, one list per category.
	 *
	 * @param categories the categories, already sorted, each one containing its (ordered) sections.
	 * @param title      the page title.
	 * @param basePath   the path of the sections list resource (e.g. "/admin/console"): a section is linked at
	 *                   {@code <basePath>/<sectionId>}.
	 * @param language   the response language (may be null).
	 */
	static String renderSections(List<Category> categories, String title, String basePath, Language language) {
		HtmlFormRenderer r = (HtmlFormRenderer) new HtmlFormRenderer(language, basePath, null).head(title).h1(title);
		for (Category category : categories) {
			if ((category.getList() == null) || category.getList().isEmpty()) {
				continue;
			}
			r.h2(category.getLabel()).append("<ul>\n"); //$NON-NLS-1$
			for (SectionId section : category.getList()) {
				r.append("<li>").link(r.notEmpty(section.getLabel()) ? section.getLabel() : section.getId(),
						r.sectionPath + '/' + URLEncoder.encode(section.getId(), StandardCharsets.UTF_8).replace("+", "%20")); //$NON-NLS-1$ //$NON-NLS-2$
				if (r.notEmpty(section.getHelp())) {
					r.append(" <small>").append(r.multiline(section.getHelp())).append("</small>"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				r.append("</li>\n"); //$NON-NLS-1$
			}
			r.append("</ul>\n"); //$NON-NLS-1$
		}
		return r.render();
	}

	private String page(List<ConsoleField> fields, String title, Language language) {
		head(title);
		h1(title);
		// Without any visible action a form has no target: fields are then rendered outside of a form.
		boolean form = hasVisibleAction(fields);
		if (form) {
			form();
		}
		boolean inSet = false;
		for (ConsoleField field : fields) {
			if (field == null) {
				continue;
			}
			if (!(field instanceof ConsoleProperty)) {
				closeTable();
			}
			if (field instanceof ConsoleSet) {
				// A ConsoleSet has no children: it opens a group that lasts until the next set.
				// The "anchor" (partial replacement of a set client-side) is meaningless here: each response is a full page.
				if (inSet) {
					append("</fieldset>\n"); //$NON-NLS-1$
				}
				append("<fieldset>\n"); //$NON-NLS-1$
				if (field.getLabel() != null) {
					legend(field.getLabel());
				}
				small(field.getHelp());
				inSet = true;
			} else if (field instanceof ConsoleProperty) {
				property((ConsoleProperty) field);
			} else if (field instanceof ConsoleAction) {
				action((ConsoleAction) field);
			} else if (field instanceof ConsoleMessage) {
				message((ConsoleMessage) field);
			} else {
				// ConsoleText, and any unknown kind of field: display the label.
				if (field.getLabel() != null) {
					append("<p>").append(multiline(field.getLabel())); //$NON-NLS-1$
					if (notEmpty(field.getHelp())) {
						append("<br/>\n<small>").append(multiline(field.getHelp())).append("</small>"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					append("</p>\n"); //$NON-NLS-1$
				}
			}
		}
		closeTable();
		if (inSet) {
			append("</fieldset>\n"); //$NON-NLS-1$
		}
		if (form) {
			closeForm();
		}
		link(backLabel, sectionPath);
		return render();
	}
	
	private static boolean hasVisibleAction(List<ConsoleField> fields) {
		for (ConsoleField field : fields) {
			if ((field instanceof ConsoleAction) && !((ConsoleAction) field).isHidden() && (((ConsoleAction) field).getId() != null)) {
				return true;
			}
		}
		return false;
	}

	private void property(ConsoleProperty p) {
		String name = p.getId();
		if (name == null) {
			return;
		}
		String value = (p.getDefaultvalue() == null) ? "" : p.getDefaultvalue(); //$NON-NLS-1$
		if (p.isHidden()) {
			hidden(name, value);
			return;
		}
		table();
		String id = newId();
		append("<tr>\n<td><label for=\"").append(id).append("\">").append(escRich(p.getLabel())).append("</label></td>\n<td>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (p.isBoolean()) {
			// A checkbox is not submitted when unchecked: a true/false list is unambiguous for the server.
			String v = "true".equalsIgnoreCase(value) ? "true" : "false"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			select(id, name, BOOLEANS, v, p.isReadonly());
		} else if (p.isEnumeration()) {
			select(id, name, p.getList(), value, p.isReadonly());
		} else if (ConsoleProperty.TYPE_TEXT.equalsIgnoreCase(p.getType())) {
			append("<textarea id=\"").append(id).append("\" name=\"").append(esc(name)).append("\" rows=\"5\" cols=\"60\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			if (p.isReadonly()) {
				append(" readonly=\"readonly\""); //$NON-NLS-1$
			}
			// The first line feed after the opening tag is ignored by parsers: it protects a leading line feed of the value.
			append(">\n").append(esc(value)).append("</textarea>"); //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			String type = "text"; //$NON-NLS-1$
			String extra = ""; //$NON-NLS-1$
			if (p.isPassword()) {
				// The current password is never written in the page.
				type = "password"; //$NON-NLS-1$
				value = ""; //$NON-NLS-1$
			} else if (p.isInteger()) {
				type = "number"; //$NON-NLS-1$
				extra = " step=\"1\""; //$NON-NLS-1$
			} else if (p.isPositiveInteger()) {
				type = "number"; //$NON-NLS-1$
				extra = " step=\"1\" min=\"0\""; //$NON-NLS-1$
			} else if (p.isFloat()) {
				type = "number"; //$NON-NLS-1$
				extra = " step=\"any\""; //$NON-NLS-1$
			}
			// date, time, path and url stay "text": the server formats (ISO dates...) are not those of the browser inputs.
			append("<input id=\"").append(id).append("\" type=\"").append(type).append("\" name=\"").append(esc(name)) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					.append("\" value=\"").append(esc(value)).append('"').append(extra); //$NON-NLS-1$
			if (p.isReadonly()) {
				append(" readonly=\"readonly\""); //$NON-NLS-1$
			}
			append("/>");
		}
		if (notEmpty(p.getHelp())) {
			append("<br/>\n<small>").append(multiline(p.getHelp())).append("</small>"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		append("</td>\n</tr>\n"); //$NON-NLS-1$
	}

	private void action(ConsoleAction a) {
		// Hidden actions are reserved to the rich client (delayed actions, progressive loading).
		// The "code" (client side script) and "recall" delay are not supported.
		if (a.isHidden() || (a.getId() == null)) {
			return;
		}
		String label = notEmpty(a.getLabel()) ? a.getLabel() : a.getId();
		append("<p><button type=\"submit\" formmethod=\"post\" formaction=\"").append(esc(sectionPath)).append('/') //$NON-NLS-1$
				.append(esc(URLEncoder.encode(a.getId(), StandardCharsets.UTF_8).replace("+", "%20"))).append("\">") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				.append(escRich(label)).append("</button>"); //$NON-NLS-1$
		if (notEmpty(a.getHelp())) {
			append(" <small>").append(multiline(a.getHelp())).append("</small>"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		append("</p>\n"); //$NON-NLS-1$
	}

	private void message(ConsoleMessage m) {
		boolean alert = (m.getMessageType() == ConsoleMessage.ERROR) || (m.getMessageType() == ConsoleMessage.WARNING);
		append(alert ? "<p role=\"alert\">" : "<p>"); //$NON-NLS-1$ //$NON-NLS-2$
		if (notEmpty(m.getTitle())) {
			append("<strong>").append(escRich(m.getTitle())).append("</strong><br/>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		append(multiline(m.getLabel()));
		if (notEmpty(m.getHelp())) {
			append("<br/>\n<small>").append(multiline(m.getHelp())).append("</small>"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		append("</p>\n"); //$NON-NLS-1$
	}
}