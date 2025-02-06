/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     ARCAD Software - initial API and implementation
 *******************************************************************************/
package com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers;

import java.text.ParseException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.servers.model.IServer;
import com.arcadsoftware.afs.client.server.admin.common.Activator;
import com.arcadsoftware.afs.client.server.admin.common.core.model.Form;
import com.arcadsoftware.afs.client.server.admin.common.ui.IIconConsts;
import com.arcadsoftware.afs.client.server.admin.common.ui.actions.AbstractCopyToClipboardAction;
import com.arcadsoftware.afs.framework.ui.images.ImageManager;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleMessage;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.ConsoleSet;
import com.arcadsoftware.rest.console.ConsoleText;
import com.arcadsoftware.rest.console.SectionId;

public class SectionComposite extends Composite implements ICopyContentProvider {

	private class BooleanListener extends SelectionAdapter {

		public String id;

		public BooleanListener(final String id) {
			this.id = id;
		}

		@Override
		public void widgetSelected(final SelectionEvent e) {
			if (e.getSource() instanceof Button) {
				final Button b = (Button) e.getSource();
				actionManager.getValues().put(id, b.getSelection() ? "true" : "false"); //$NON-NLS-1$//$NON-NLS-2$
			}
		}
	}

	private class StringListener implements ModifyListener {

		public String id;

		public StringListener(final String id) {
			this.id = id;
		}

		@Override
		public void modifyText(final ModifyEvent e) {
			if (e.getSource() instanceof Text) {
				final Text t = (Text) e.getSource();
				actionManager.getValues().put(id, t.getText());
			} else if (e.getSource() instanceof Combo) {
				final Combo c = (Combo) e.getSource();
				actionManager.getValues().put(id, c.getText());
			}
		}
	}

	/**
	 * Time Selection Listener.
	 */
	private class TimeListener extends SelectionAdapter {

		public String id;
		public DateTime widget;

		public TimeListener(final String id, final DateTime widget) {
			this.id = id;
			this.widget = widget;
		}

		@Override
		public void widgetSelected(final SelectionEvent e) {
			final Calendar calendar = new GregorianCalendar();
			calendar.set(Calendar.HOUR_OF_DAY, widget.getHours());
			calendar.set(Calendar.MINUTE, widget.getMinutes());
			calendar.set(Calendar.SECOND, widget.getSeconds());
			final long t = calendar.getTimeInMillis();
			actionManager.getValues().put(id, String.valueOf(t));
		}
	}

	private final ActionManager actionManager;
	private Composite actionBarComposite;
	private Composite bodyComposite;
	private AbstractCopyToClipboardAction copyToClipboardAction;
	private Composite helpComposite;
	private SendbyMailAction sendbyMailAction;

	public SectionComposite(final Composite parent, final int style, final ActionManager actionManager) {
		super(parent, style);
		this.actionManager = actionManager;
		format();
		createAction();
	}

	private void addSpecialAction(final Menu menu, final IAction action, final String imageKey) {
		addSpecialAction(menu, action, ImageManager.getInstance().getImage(imageKey));
	}

	private void addSpecialAction(final Menu menu, final IAction action, final Image image) {
		final MenuItem item = new MenuItem(menu, SWT.PUSH);
		item.setText(action.getText());
		item.setImage(image);
		item.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						action.run();
					}
				});
	}

	private void createAction() {
		copyToClipboardAction = getCopyToClipboardAction();

		sendbyMailAction = new SendbyMailAction() {
			@Override
			public String getContent() {
				return sectionToString();
			}

			@Override
			public String getSection() {
				return actionManager.getSectionId().getId();
			}
		};
	}

	private void createActionMenuItem(final Menu menu, final ConsoleAction property) {
		final MenuItem item = new MenuItem(menu, SWT.PUSH);
		item.setText(property.getLabel());
		item.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						final ActionManager itemManager = actionManager.clone(true);
						itemManager.setAction(property);
						itemManager.execute();
					}
				});
	}

	private void createBoolean(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly) {
		final Button b = GuiFormatTools.createLabelledCheckbox(parent, property.getLabel(),
				defaultValue.equalsIgnoreCase("true")); //$NON-NLS-1$
		b.addSelectionListener(new BooleanListener(property.getId()));
		b.setEnabled(!readOnly);
		b.setToolTipText(property.getHelp());
	}

	private void createCombo(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly) {
		final String[] items = new String[property.getList().size()];
		property.getList().toArray(items);
		// recherche de la valeur par défaut
		final int index = property.getList().indexOf(defaultValue);
		final Combo combo = GuiFormatTools.createLabelledCombo(parent, property.getLabel(), readOnly, items, index);
		combo.addModifyListener(new StringListener(property.getId()));
		combo.setToolTipText(property.getHelp());
	}

	private void createDate(final Composite parent, final ConsoleProperty property, final String defaultValue) {
		final Text dateText = GuiFormatTools.createLabelledDate(parent, property.getLabel(), defaultValue);
		dateText.addModifyListener(new StringListener(property.getId()));
		dateText.setToolTipText(property.getHelp());
		final DateTime dateTime = new DateTime(parent, SWT.TIME);
		dateTime.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
		dateTime.setEnabled(!property.isReadonly());
		try {
			final GregorianCalendar date = new GregorianCalendar();
			date.setTime(ISODateFormater.toDate(defaultValue));
			dateTime.setDate(date.get(Calendar.YEAR), date.get(Calendar.MONTH), date.get(Calendar.DAY_OF_MONTH));
		} catch (final ParseException e) {
			Activator.getInstance().error(e.getLocalizedMessage(), e);
		}
	}

	private Group createGroup(final Composite parent, final ConsoleSet property) {
		return GuiFormatTools.createGroup(parent, property.getLabel());
	}

	private void createHelpPanel(final Composite parent, final String help) {
		GuiFormatTools.createLabel(parent, help, false, 3);
	}

	private void createLongString(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly) {
		GuiFormatTools.createLabel(parent, property.getLabel());

		final Text t = GuiFormatTools.createText(parent, defaultValue, true, 3);
		t.addModifyListener(new StringListener(property.getId()));
		t.setEditable(!readOnly);
		t.setToolTipText(property.getHelp());
	}

	private Group createMessage(final Composite parent, final ConsoleMessage property) {
		final Group group = GuiFormatTools.createGroup(parent, property.getTitle());
		GuiFormatTools.createLabel(group, property.getLabel(), true, 3);
		return group;
	}

	private void createNumber(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly) {
		createNumber(parent, property, defaultValue, readOnly, false);
	}

	private void createNumber(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly, final boolean isPositive) {
		final Text numberText = GuiFormatTools.createLabelledIntegerText(parent,
				property.getLabel(), Integer.valueOf(defaultValue), Integer.MAX_VALUE, null, isPositive);
		numberText.addModifyListener(new StringListener(property.getId()));
		numberText.setEnabled(!readOnly);
		numberText.setToolTipText(property.getHelp());
	}

	private void createPassword(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly) {
		createString(parent, property, defaultValue, readOnly, true);
	}

	private void createProperty(final Composite parent, final ConsoleProperty property) {
		String defaultValue = property.getDefaultvalue();
		if (defaultValue == null) {
			defaultValue = ""; //$NON-NLS-1$
		}
		actionManager.getValues().put(property.getId(), defaultValue);
		if (!property.isHidden()) {
			final boolean readOnly = property.isReadonly();
			String type = getType(property);
			if (type == null) {
				type = ConsoleProperty.TYPE_STRING;
			}
			if (type.equals(ConsoleProperty.TYPE_BOOLEAN)) {
				createBoolean(parent, property, defaultValue, readOnly);
			} else if (type.equals(ConsoleProperty.TYPE_ENUM)) {
				createCombo(parent, property, defaultValue, readOnly);
			} else if (type.equals(ConsoleProperty.TYPE_STRING)) {
				createString(parent, property, defaultValue, readOnly, property.isPassword());
			} else if (type.equals(ConsoleProperty.TYPE_TEXT)) {
				createLongString(parent, property, defaultValue, readOnly);
			} else if (type.equals(ConsoleProperty.TYPE_PASSWORD)) {
				createPassword(parent, property, defaultValue, readOnly);
			} else if (type.equals(ConsoleProperty.TYPE_INTEGER)) {
				createNumber(parent, property, defaultValue, readOnly);
			} else if (type.equals(ConsoleProperty.TYPE_DATE)) {
				createDate(parent, property, defaultValue);
			} else if (type.equals(ConsoleProperty.TYPE_TIME)) {
				createTime(parent, property, defaultValue);
			} else if (type.equals(ConsoleProperty.TYPE_POSITIVEINTEGER)) {
				createNumber(parent, property, defaultValue, readOnly, true);
			} // FIXME unknown type must be taken into account too !
		}
	}

	private void createString(final Composite parent, final ConsoleProperty property, final String defaultValue,
			final boolean readOnly, final boolean password) {
		final Text t = GuiFormatTools.createLabelledText(parent, property.getLabel());
		t.setText(defaultValue);
		if (password) {
			t.setEchoChar('*');
		}
		t.addModifyListener(new StringListener(property.getId()));
		t.setEditable(!readOnly);
		t.setToolTipText(property.getHelp());
	}

	private void createText(final Composite parent, final ConsoleText property) {
		GuiFormatTools.createLabel(parent, property.getLabel(), false, 3);
	}

	private void createTime(final Composite parent, final ConsoleProperty property, final String defaultValue) {
		final DateTime time = GuiFormatTools.createLabelledDateTime(parent, property.getLabel(), SWT.TIME);
		time.setToolTipText(property.getHelp());
		time.setEnabled(!property.isReadonly());
		final GregorianCalendar calendar = new GregorianCalendar();
		try {
			final Long timeVal = Long.parseLong(defaultValue);
			if (timeVal != null) {
				calendar.setTimeInMillis(timeVal);
			}
		} catch (final NumberFormatException e) {
			// do not catch
		}
		time.setTime(calendar.get(Calendar.HOUR_OF_DAY), calendar.get(Calendar.MINUTE), 0);
		time.addSelectionListener(new TimeListener(property.getId(), time));

	}

	public void createToolbar(final Composite parent, final Form form) {
		final ToolBar toolbar = new ToolBar(parent, SWT.FLAT | SWT.RIGHT);
		final ToolItem toolItem = new ToolItem(toolbar, SWT.DROP_DOWN);
		toolItem.setImage(ImageManager.getInstance().getImage(IIconConsts.ACTION));
		toolItem.setText(Activator.resString("label.actions")); //$NON-NLS-1$
		final Menu menu = new Menu(Activator.getInstance().getPluginShell());
		if (form.containsAction()) {
			// We now create the action
			for (final ConsoleField f : form.getFields()) {
				if (f instanceof ConsoleAction) {
					final ConsoleAction action = (ConsoleAction) f;
					createActionMenuItem(menu, action);
				}
			}
			new MenuItem(menu, SWT.SEPARATOR);
		}
		if (copyToClipboardAction != null) {
			addSpecialAction(menu, copyToClipboardAction, AFSIcon.ENTRY_COPY.image());
		}
		addSpecialAction(menu, sendbyMailAction, IIconConsts.SENDMAIL);
		toolItem.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						final Rectangle rect = toolItem.getBounds();
						Point pt = new Point(rect.x, rect.y + rect.height);
						pt = toolbar.toDisplay(pt);
						menu.setLocation(pt.x, pt.y);
						menu.setVisible(true);
					}
				});
	}

	/**
	 * This method is used to create the content of a section
	 *
	 * @param sectionId
	 * @param form
	 * @param help
	 */
	public void fill(final Form form) {
		final SectionId section = form.getSection();
		createToolbar(actionBarComposite, form);
		fillForm(form);
		// Help management
		final String help = section.getHelp();
		if (help != null) {
			createHelpPanel(helpComposite, help);
		}
	}

	/**
	 * This method is used to create the content of a section
	 *
	 * @param sectionId
	 * @param form
	 * @param help
	 */
	public void fillForm(final Form form) {
		// Loop on the fields to create them
		Composite currentParent = getParentComposite();
		for (final ConsoleField f : form.getFields()) {
			if (f instanceof ConsoleProperty) {
				createProperty(currentParent, (ConsoleProperty) f);
				// if a field is a control set so, it will be
				// the parent of all teh following fields.
			} else if (f instanceof ConsoleSet) {
				final Group g = createGroup(getParentComposite(), (ConsoleSet) f);
				currentParent = g;
			} else if (f instanceof ConsoleText) {
				createText(currentParent, (ConsoleText) f);
			} else if (f instanceof ConsoleMessage) {
				createMessage(currentParent, (ConsoleMessage) f);
			}
		}

	}

	protected void format() {
		GridLayout l = new GridLayout(3, false);
		l.marginHeight = l.marginWidth = 0;
		l.verticalSpacing = 0;
		setLayout(l);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		setLayoutData(gridData);
		actionBarComposite = new Composite(this, SWT.NONE);
		l = new GridLayout(2, false);
		l.marginHeight = l.marginWidth = 0;
		actionBarComposite.setLayout(l);
		gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalSpan = 3;
		actionBarComposite.setLayoutData(gridData);
		final Label label = new Label(actionBarComposite, SWT.NONE);
		gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.grabExcessHorizontalSpace = true;
		label.setLayoutData(gridData);
		bodyComposite = new Composite(this, SWT.BORDER);
		l = new GridLayout(3, false);
		bodyComposite.setLayout(l);
		gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 1;
		bodyComposite.setLayoutData(gridData);
		helpComposite = new Composite(this, SWT.NONE);
		l = new GridLayout(4, false);
		l.marginHeight = l.marginWidth = 5;
		helpComposite.setLayout(l);
		gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalSpan = 3;
		helpComposite.setLayoutData(gridData);
		final Label labelImage = new Label(helpComposite, SWT.NONE);
		labelImage.setImage(AFSIcon.HELP.image());
		gridData = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
		labelImage.setLayoutData(gridData);

	}

	@Override
	public String getContentToCopy() {
		return sectionToString();
	}

	/**
	 * Load extensions
	 *
	 * @return
	 */
	private AbstractCopyToClipboardAction getCopyToClipboardAction() {
		final IExtensionRegistry registry = Platform.getExtensionRegistry();
		final IConfigurationElement[] config = registry
				.getConfigurationElementsFor("com.arcadsoftware.client.server.admin.common.copytoclipboard"); //$NON-NLS-1$
		try {
			for (final IConfigurationElement e : config) {
				final Object o = e.createExecutableExtension("class"); //$NON-NLS-1$
				if (o instanceof AbstractCopyToClipboardAction) {
					((AbstractCopyToClipboardAction) o).setContentProvider(this);
					return (AbstractCopyToClipboardAction) o;
				}
			}
		} catch (final CoreException ex) {
			Activator.getInstance().debug(ex.getLocalizedMessage());
		}
		return null;
	}

	protected Composite getParentComposite() {
		return bodyComposite;
	}

	private String getType(final ConsoleProperty property) {
		if (property == null) {
			return ConsoleProperty.TYPE_STRING;
		}
		final String result = property.getType();
		if (result != null) {
			return result;
		}
		final String defaultValue = property.getDefaultvalue();
		if ((defaultValue == null) || defaultValue.equals("")) {
			return ConsoleProperty.TYPE_STRING;
		}
		if (defaultValue.equalsIgnoreCase("true") || defaultValue.equalsIgnoreCase("false")) { //$NON-NLS-1$ //$NON-NLS-2$
			return ConsoleProperty.TYPE_BOOLEAN;
		}
		try {
			Integer.parseInt(defaultValue);
			return ConsoleProperty.TYPE_INTEGER;
		} catch (final NumberFormatException e) {
			return ConsoleProperty.TYPE_STRING;
		}

	}

	private String sectionToString() {
		final StringBuilder content = new StringBuilder();
		content.append("SectionId: ").append(actionManager.getSectionId().getId()).append("\n");//$NON-NLS-1$ ; //$NON-NLS-2$
		final IServer server = actionManager.getConnector().getServerConnection().getServer();
		content.append("Server Information: ")//$NON-NLS-1$
				.append(server.getName()).append("/").append(server.getUrl())//$NON-NLS-1$
				.append("\n");//$NON-NLS-1$
		content.append("--- begin of the section ---\n");//$NON-NLS-1$
		final Set<Entry<String, Object>> entries = actionManager.getValues().entrySet();
		for (final Entry<String, Object> entry : entries) {
			content.append(entry.getKey()).append("=");//$NON-NLS-1$
			final Object value = entry.getValue();
			if (value instanceof String) {
				final ConsoleProperty prop = actionManager.getForm().getProperty(entry.getKey());
				if ((prop != null) && prop.isPassword()) {
					content.append(((String) value).length() + "*"); //$NON-NLS-1$
				} else {
					content.append((String) value);
				}
			} else {
				content.append("=**** ")//$NON-NLS-1$
						.append(value.toString())
						.append("****");//$NON-NLS-1$
			}
			content.append("\n");//$NON-NLS-1$
		}
		content.append("--- end of the section ---");//$NON-NLS-1$
		return content.toString();
	}

}
