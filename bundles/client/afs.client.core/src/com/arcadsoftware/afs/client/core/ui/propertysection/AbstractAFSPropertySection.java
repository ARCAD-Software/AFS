/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.afs.client.core.ui.propertysection;

import java.util.Hashtable;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

import com.arcadsoftware.afs.client.core.ui.propertysource.AbstractAFSProperties;

public abstract class AbstractAFSPropertySection extends AbstractPropertySection {

	private final Hashtable<Text, TextModifyListener> textListeners;
	private final Hashtable<Button, ButtonSelectionListener> checkboxListeners;
	protected AbstractAFSProperties properties;

	public class TextModifyListener implements ModifyListener {

		private String id;
		private Text owner;

		public TextModifyListener(Text owner, String id) {
			this.owner = owner;
			this.id = id;
		}

		public void modifyText(ModifyEvent arg0) {
			properties.setPropertyValue(id, owner.getText());
		}
	}

	private class ButtonSelectionListener extends SelectionAdapter {

		private String id;
		private Button owner;

		public ButtonSelectionListener(Button owner, String id) {
			this.owner = owner;
			this.id = id;
		}

		@Override
		public void widgetSelected(SelectionEvent e) {
			properties.setPropertyValue(id, owner.getSelection());
		}
	}

	public AbstractAFSPropertySection() {
		super();
		textListeners = new Hashtable<Text, TextModifyListener>();
		checkboxListeners = new Hashtable<Button, ButtonSelectionListener>();
	}

	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
		super.createControls(parent, aTabbedPropertySheetPage);
		FillLayout fl = (FillLayout) parent.getLayout();
		fl.spacing = fl.marginWidth = fl.marginHeight = 5;
		Section section = getWidgetFactory().createSection(parent, ExpandableComposite.TITLE_BAR);
		section.setText(getSectionTitle());
		Composite composite = getWidgetFactory().createComposite(section, SWT.NONE);
		section.setClient(composite);
		GridLayout gl = new GridLayout(3, false);
		gl.verticalSpacing = 2;
		gl.marginWidth = gl.marginHeight = 0;
		composite.setLayout(gl);
		createContent(composite);
	}

	protected Text createText(Composite parent, String label, String id) {
		return createText(parent, label, id, false);
	}

	protected Text createText(Composite parent, String label, String id, boolean readonly) {
		getWidgetFactory().createCLabel(parent, label);
		getWidgetFactory().createCLabel(parent, ":"); //$NON-NLS-1$
		Text text = getWidgetFactory().createText(parent, ""); //$NON-NLS-1$
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.grabExcessHorizontalSpace = true;
		text.setLayoutData(gd);
		TextModifyListener listener = new TextModifyListener(text, id);
		text.addModifyListener(listener);
		textListeners.put(text, listener);
		text.setEditable(!readonly);
		text.setEnabled(!readonly);
		text.setData(id);
		return text;
	}

	protected void registerText(Text text, String propertyId) {
		TextModifyListener listener = new TextModifyListener(text, propertyId);
		textListeners.put(text, listener);
		text.addModifyListener(listener);
		text.setData(propertyId);
	}

	protected void registerCheckbox(Button checkbox, String propertyId) {
		ButtonSelectionListener listener = new ButtonSelectionListener(checkbox, propertyId);
		checkboxListeners.put(checkbox, listener);
		checkbox.addSelectionListener(listener);
		checkbox.setData(propertyId);
	}

	protected Button createCheckbox(Composite parent, String label, String id) {
		return createCheckbox(parent, label, id, false);
	}

	protected Button createCheckbox(Composite parent, String label, String id, boolean readonly) {
		getWidgetFactory().createCLabel(parent, "");//$NON-NLS-1$
		getWidgetFactory().createCLabel(parent, ""); //$NON-NLS-1$
		Button checkbox = getWidgetFactory().createButton(parent, label, SWT.CHECK);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.grabExcessHorizontalSpace = true;
		checkbox.setLayoutData(gd);
		ButtonSelectionListener listener = new ButtonSelectionListener(checkbox, id);
		checkbox.addSelectionListener(listener);
		checkboxListeners.put(checkbox, listener);
		checkbox.setEnabled(!readonly);
		checkbox.setData(id);
		return checkbox;
	}

	public void setInput(IWorkbenchPart part, ISelection selection) {
		super.setInput(part, selection);
		if (selection instanceof IStructuredSelection) {
			Object input = ((IStructuredSelection) selection).getFirstElement();
			if (input instanceof IAdaptable) {
				IAdaptable adaptable = (IAdaptable) input;
				Object result = adaptable.getAdapter(AbstractAFSProperties.class);
				if (result instanceof AbstractAFSProperties) {
					properties = (AbstractAFSProperties) result;
					inputChanged();
				}
			}
		}
	}

	public void inputChanged() {}

	public void refreshCheckbox(Button button) {
		ButtonSelectionListener listener = checkboxListeners.get(button);
		if (listener != null) {
			button.removeSelectionListener(listener);
			String id = (String) button.getData();
			if (id != null) {
				Object value = properties.getPropertyValue(id);
				if ((value != null) && (value instanceof Boolean)) {
					Boolean vboolean = (Boolean) value;
					button.setSelection(vboolean);
				}
			}
			button.addSelectionListener(listener);
		}
	}

	public void refreshText(Text text) {
		TextModifyListener listener = textListeners.get(text);
		if (listener != null) {
			text.removeModifyListener(listener);
			String id = (String) text.getData();
			if (id != null) {
				String value = (String) properties.getPropertyValue(id);
				if (value != null) {
					text.setText(value);
				}
			}
			text.addModifyListener(listener);
		}
	}

	public void refresh() {
		for (Entry<Text, TextModifyListener> entry : textListeners.entrySet()) {
			refreshText(entry.getKey());
		}
		for (Entry<Button, ButtonSelectionListener> entry : checkboxListeners.entrySet()) {
			refreshCheckbox(entry.getKey());
		}
	}

	public abstract String getSectionTitle();

	public abstract void createContent(Composite parent);

}
