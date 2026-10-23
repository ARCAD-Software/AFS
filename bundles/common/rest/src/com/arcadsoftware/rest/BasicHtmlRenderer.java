package com.arcadsoftware.rest;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Locale;

import org.restlet.data.Language;

/**
 * Basic utility class to generate simple HTML pages.
 * 
 * <p>
 * Resulting HTML code is compatible with application/xhtml and text/html.
 * 
 * @author ARCAD Software
 */
public class BasicHtmlRenderer {

	protected static final List<String> BOOLEANS = List.of("true", "false"); //$NON-NLS-1$ //$NON-NLS-2$

	private final StringBuilder sb = new StringBuilder(4096);
	private int counter;
	private boolean inTable;
	private boolean inLine;
	private boolean inTableBody;
	private boolean inForm;
	// Hidden inputs of properties encountered inside a table: written after it, since they have no row.
	private final StringBuilder pendingHidden = new StringBuilder();
	private final Language language;

	public BasicHtmlRenderer(Language language) {
		super();
		this.language = language;
	}

	/**
	 * Generate the HTML page.
	 * 
	 * @return non null HTML code.
	 */
	public String render() {
		if (sb.isEmpty()) {
			head(""); //$NON-NLS-1$
		} else {
			closeTable();
			closeForm();
		}
		sb.append("</body>\n</html>\n"); //$NON-NLS-1$
		return sb.toString();
	}

	/**
	 * Return the Language associated to this page.
	 * 
	 * @return
	 */
	public Language getLanguage() {
		return language;
	}
	
	/**
	 * Add the HTML Head, must be called just once (can only be applied before any other code.
	 * 
	 * @param title
	 * @return
	 */
	public BasicHtmlRenderer head(String title) {
		if (sb.isEmpty()) {
			sb.append("<!DOCTYPE html>\n<html xmlns=\"http://www.w3.org/1999/xhtml\""); //$NON-NLS-1$
			String lang = (language == null) ? null : language.getPrimaryTag();
			if ((lang != null) && !lang.isEmpty() && !"*".equals(lang)) { //$NON-NLS-1$
				sb.append(" lang=\"").append(esc(lang)).append("\" xml:lang=\"").append(esc(lang)).append('"'); //$NON-NLS-1$
			}
			sb.append(">\n<head>\n<meta charset=\"utf-8\"/>\n<title>").append(esc(title)).append("</title>\n</head>\n<body>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}

	/**
	 * Open a form.
	 * 
	 * @return
	 */
	public BasicHtmlRenderer form() {
		sb.append("<form method=\"post\" accept-charset=\"utf-8\">\n"); //$NON-NLS-1$
		inForm = true;
		return this;
	}
	
	/**
	 * Close a form.
	 * 
	 * <p>
	 * This method is optional if nothing need to be added after the form in the page.
	 * 
	 * @return
	 * @see #form()
	 */
	public BasicHtmlRenderer closeForm() {
		if (inForm) {
			sb.append("</form>\n"); //$NON-NLS-1$
			inForm = false;
		}
		return this;
	}
	
	/**
	 * Add a legend.
	 * 
	 * @param text
	 * @return
	 */
	public BasicHtmlRenderer legend(String text) {
		sb.append("<legend>").append(multiline(text)).append("</legend>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return this;
	}
	
	/**
	 * Add button to the page: a separate GET to the targetted path.
	 * 
	 * @param label
	 * @param path
	 * @return
	 */
	public BasicHtmlRenderer button(String label, String path) {
		if ((label == null) || (path == null)) {
			return this;
		}
		if (!inForm) {
			sb.append("<p><button type=\"submit\" formaction=\"") //$NON-NLS-1$
			.append(esc(path)).append("\" formmethod=\"get\">") //$NON-NLS-1$
			.append(escRich(label)).append("</button></p>\n"); //$NON-NLS-1$
		} else {
			sb.append("<form method=\"get\" action=\"").append(esc(path)).append("\">\n<p><button type=\"submit\">") //$NON-NLS-1$ //$NON-NLS-2$
					.append(escRich(label)).append("</button></p>\n</form>\n"); //$NON-NLS-1$
		}
		return this;
	}

	/**
	 * Add a link to another page or anchor.
	 * 
	 * @param label
	 * @param path
	 * @return
	 */
	public BasicHtmlRenderer link(String label, String path) {
		sb.append("<a href=\"").append(esc(path)) //$NON-NLS-1$
			.append("\">").append(escRich(label)).append("</a>"); //$NON-NLS-1$ //$NON-NLS-2$
		return this;
	}

	/**
	 * Add a select field.
	 * 
	 * @param id may be null.
	 * @param name
	 * @param options
	 * @param selected
	 * @param disabled
	 * @return
	 */
	public BasicHtmlRenderer select(String id, String name, List<String> options, String selected, boolean disabled) {
		if (id == null) {
			id = newId();
		}
		sb.append("<select id=\"").append(id).append("\" name=\"").append(esc(name)).append('"'); //$NON-NLS-1$ //$NON-NLS-2$
		if (disabled) {
			sb.append(" disabled=\"disabled\""); //$NON-NLS-1$
		}
		sb.append(">\n"); //$NON-NLS-1$
		boolean found = false;
		if (options != null) {
			for (String option : options) {
				boolean sel = (option != null) && option.equals(selected);
				found |= sel;
				option(option, sel);
			}
		}
		if (!found && notEmpty(selected)) {
			// Keep the current value even if it is not in the list, otherwise submitting would silently change it.
			option(selected, true);
		}
		sb.append("</select>"); //$NON-NLS-1$
		if (disabled) {
			// A disabled control is not submitted.
			hidden(name, selected);
		}
		return this;
	}
	
	public BasicHtmlRenderer option(String value, boolean selected) {
		sb.append("<option value=\"").append(esc(value)).append('"'); //$NON-NLS-1$
		if (selected) {
			sb.append(" selected=\"selected\""); //$NON-NLS-1$
		}
		sb.append('>').append(esc(value)).append("</option>\n"); //$NON-NLS-1$
		return this;
	}

	/**
	 * Add HTML code.
	 * 
	 * @param s
	 * @return
	 */
	public BasicHtmlRenderer append(String s) {
		sb.append(s);
		return this;
	}

	/**
	 * Add HTML character.
	 * 
	 * @param c
	 * @return
	 */
	public BasicHtmlRenderer append(char c) {
		sb.append(c);
		return this;
	}

	/**
	 * Add an boolean value.
	 * 
	 * @param b
	 * @return
	 */
	public BasicHtmlRenderer append(boolean b) {
		if (b) {
			sb.append(BOOLEANS.get(0));
		} else {
			sb.append(BOOLEANS.get(1));
		}
		return this;
	}

	/**
	 * Add an integer value.
	 * 
	 * @param i
	 * @return
	 */
	public BasicHtmlRenderer append(int i) {
		sb.append(i);
		return this;
	}

	/**
	 * Add a new paragraph text.
	 * 
	 * @param s
	 * @return
	 */
	public BasicHtmlRenderer text(String s) {
		sb.append("<p>").append(multiline(s)).append("</p>"); //$NON-NLS-1$ //$NON-NLS-2$
		return this;
	}

	public BasicHtmlRenderer status(String text) {
		sb.append("<p role=\"status\">").append(multiline(text)).append("</p>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return this;
	}
	
	public BasicHtmlRenderer h1(String text) {
		if (notEmpty(text)) {
			sb.append("<h1>").append(escRich(text)).append("</h1>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}

	public BasicHtmlRenderer h2(String text) {
		if (notEmpty(text)) {
			sb.append("<h2>").append(escRich(text)).append("</h2>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}

	public BasicHtmlRenderer h3(String text) {
		if (notEmpty(text)) {
			sb.append("<h3>").append(escRich(text)).append("</h3>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}
	
	public BasicHtmlRenderer hidden(String name, String value) {
		// Inside a table a bare input would be misplaced: it is written once the table is closed.
		(inTable ? pendingHidden : sb).append("<input type=\"hidden\" name=\"") //$NON-NLS-1$
				.append(esc(name)).append("\" value=\"") //$NON-NLS-1$
				.append(esc(value)).append("\"/>\n"); //$NON-NLS-1$
		return this;
	}
	
	/**
	 * Start an HTML Table.
	 * 
	 * @return
	 * @see #table(String)
	 * @see #closeTable()
	 * @see #header(String)
	 * @see #column(String)
	 * @see #endTableLine()
	 */
	public BasicHtmlRenderer table() {
		return table(null);
	}
	
	/**
	 * Start an HTML Table with a caption.
	 * 
	 * @param caption
	 * @return
	 * @see #table()
	 * @see #closeTable()
	 * @see #header(String)
	 * @see #column(String)
	 * @see #endTableLine()
	 */
	public BasicHtmlRenderer table(String caption) {
		if (!inTable) {
			sb.append("<table>\n"); //$NON-NLS-1$
			inTable = true;
			inLine = false;
			inTableBody = false;
			if (notEmpty(caption)) {
				sb.append("<caption>").append(escRich(caption)).append("</caption>");
			}
		}
		return this;
	}
	
	/**
	 * Add a new Header cell in the Table.
	 * 
	 * @param s
	 * @return
	 */
	public BasicHtmlRenderer header(String s) {
		if (inTable) { 
			if (!inTableBody && !inLine) {
				sb.append("<thead>\n"); //$NON-NLS-1$
			}
			if (!inLine) {
				sb.append("<tr>\n"); //$NON-NLS-1$
				inLine = true;
			}
			sb.append("<th>").append(s).append("</th>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}
	
	/**
	 * Add a new collumn cell in the table.
	 * 
	 * @param s
	 * @return
	 * @see #table()
	 */
	public BasicHtmlRenderer column(String s) {
		if (inTable) {
			if (!inTableBody) {
				if (inLine) {
					sb.append("</thead>\n"); //$NON-NLS-1$
					inLine = false;
				}
				sb.append("<tbody>\n"); //$NON-NLS-1$
				inTableBody = true;
			}
			if (!inLine) {
				inLine = true;
				sb.append("<tr>"); //$NON-NLS-1$
			}
			sb.append("<td>").append(s).append("</td>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}
	
	/**
	 * Terminate the current line in a Table.
	 * 
	 * @return
	 */
	public BasicHtmlRenderer endTableLine() {
		if (inTable && inLine) {
			sb.append("</tr>"); //$NON-NLS-1$
			inLine = false;
			if (!inTableBody) {
				sb.append("</thead>\n"); //$NON-NLS-1$
				sb.append("<tbody>\n"); //$NON-NLS-1$
				inTableBody = true;
			}
		}
		return this;
	}
	
	/** 
	 * Close the table of properties (if any), then write the hidden inputs met meanwhile.
	 * 
	 * <p>
	 * This method is optional if nothing need to be added after the table in the page.
	 * 
	 * @return
	 */
	public BasicHtmlRenderer closeTable() {
		if (inTable) {
			endTableLine();
			if (inTableBody) {
				sb.append("</tbody>\n"); //$NON-NLS-1$
				inTableBody = false;
			}
			sb.append("</table>\n"); //$NON-NLS-1$
			inTable = false;
		}
		if (pendingHidden.length() > 0) {
			sb.append(pendingHidden);
			pendingHidden.setLength(0);
		}
		return this;
	}
	
	/**
	 * Add a small paragraph.
	 * 
	 * @param text
	 * @return
	 */
	public BasicHtmlRenderer small(String text) {
		if (notEmpty(text)) {
			sb.append("<p><small>").append(multiline(text)).append("</small></p>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return this;
	}
	
	/**
	 * Generate a new and unique ID.
	 * 
	 * @return
	 */
	protected String newId() {
		return "f" + (++counter);
	}

	/**
	 * Return true is the string is not null and non empty (it may be blank).
	 * @param s
	 * @return
	 */
	protected boolean notEmpty(String s) {
		return (s != null) && !s.isEmpty();
	}

	/** 
	 * Escape a text for an HTML content or a quoted attribute.
	 * 
	 * @param s
	 * @return
	 */
	protected String esc(String s) {
		if (s == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder b = new StringBuilder(s.length() + 16);
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			switch (c) {
			case '&':
				b.append("&amp;"); //$NON-NLS-1$
				break;
			case '<':
				b.append("&lt;"); //$NON-NLS-1$
				break;
			case '>':
				b.append("&gt;"); //$NON-NLS-1$
				break;
			case '"':
				b.append("&quot;"); //$NON-NLS-1$
				break;
			case '\'':
				b.append("&#39;"); //$NON-NLS-1$
				break;
			default:
				b.append(c);
			}
		}
		return b.toString();
	}

	/**
	 * Escape a text for an HTML content, but keep a small whitelist of markup:
	 * <ul>
	 * <li>numeric character references: {@code &#160;} or {@code &#xA0;} (only if they designate a character valid in XML),
	 * <li>{@code <strong>...</strong>}, {@code <i>...</i>}, {@code <code>...</code>} (no attributes),
	 * <li>{@code <br/>}.
	 * </ul>
	 * Everything else ({@code <}, {@code >}, {@code &}, quotes, other tags, named entities such as {@code &nbsp;}) is escaped.
	 * Opening tags that are not closed are closed at the end, and a closing tag that does not match the last opened one is
	 * escaped, so the result is always well-formed (XML and HTML).
	 */
	protected String escRich(String s) {
		if (s == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder b = new StringBuilder(s.length() + 16);
		ArrayDeque<String> open = new ArrayDeque<>();
		int n = s.length();
		int i = 0;
		while (i < n) {
			char c = s.charAt(i);
			if (c == '&') {
				int end = numericReferenceEnd(s, i);
				if (end > 0) {
					b.append(s, i, end);
					i = end;
				} else {
					b.append("&amp;"); //$NON-NLS-1$
					i++;
				}
			} else if (c == '<') {
				int end = s.indexOf('>', i);
				String tag = (end < 0) ? null : s.substring(i + 1, end).toLowerCase(Locale.ROOT);
				if (tag == null) {
					b.append("&lt;"); //$NON-NLS-1$
					i++;
				} else if ("br/".equals(tag)) { //$NON-NLS-1$
					b.append("<br/>"); //$NON-NLS-1$
					i = end + 1;
				} else if ("strong".equals(tag) || "i".equals(tag) || "code".equals(tag)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					open.push(tag);
					b.append('<').append(tag).append('>');
					i = end + 1;
				} else if (tag.startsWith("/") && tag.substring(1).equals(open.peek())) { //$NON-NLS-1$
					open.pop();
					b.append('<').append(tag).append('>');
					i = end + 1;
				} else {
					b.append("&lt;"); //$NON-NLS-1$
					i++;
				}
			} else if (c == '>') {
				b.append("&gt;"); //$NON-NLS-1$
				i++;
			} else if (c == '"') {
				b.append("&quot;"); //$NON-NLS-1$
				i++;
			} else if (c == '\'') {
				b.append("&#39;"); //$NON-NLS-1$
				i++;
			} else {
				b.append(c);
				i++;
			}
		}
		while (!open.isEmpty()) {
			b.append("</").append(open.pop()).append('>'); //$NON-NLS-1$
		}
		return b.toString();
	}

	/**
	 * @return the index following a valid numeric character reference ({@code &#123;} or {@code &#x7B;}) starting at
	 *         {@code start}, or -1.
	 */
	protected int numericReferenceEnd(String s, int start) {
		int n = s.length();
		int i = start + 1;
		if ((i >= n) || (s.charAt(i) != '#')) {
			return -1;
		}
		i++;
		int radix = 10;
		if ((i < n) && ((s.charAt(i) == 'x') || (s.charAt(i) == 'X'))) {
			radix = 16;
			i++;
		}
		int digits = i;
		long code = 0;
		while ((i < n) && (Character.digit(s.charAt(i), radix) >= 0) && (i - digits < 7)) {
			code = code * radix + Character.digit(s.charAt(i), radix);
			i++;
		}
		if ((i == digits) || (i >= n) || (s.charAt(i) != ';')) {
			return -1;
		}
		// Only characters allowed by XML 1.0.
		boolean valid = (code == 0x9) || (code == 0xA) || (code == 0xD) || ((code >= 0x20) && (code <= 0xD7FF))
				|| ((code >= 0xE000) && (code <= 0xFFFD)) || ((code >= 0x10000) && (code <= 0x10FFFF));
		return valid ? i + 1 : -1;
	}

	/** 
	 * Escape a text (see {@link #escRich(String)}) and keep its line breaks. 
	 * 
	 */
	protected String multiline(String s) {
		return escRich(s).replace("\r\n", "\n").replace("\n", "<br/>\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

}
