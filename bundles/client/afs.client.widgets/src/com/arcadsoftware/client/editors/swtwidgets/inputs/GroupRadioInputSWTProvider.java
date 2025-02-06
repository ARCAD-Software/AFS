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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.READ_ONLY;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapChangedListener;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

public class GroupRadioInputSWTProvider implements IInputSWTProvider, IWidgetValue, SelectionListener {

	public static final String TYPE_BASIC_INTEGER = "integer"; //$NON-NLS-1$
	public static final String TYPE_BASIC_STRING = "string"; //$NON-NLS-1$
	public static final String TYPE_BASIC_BOOLEAN = "boolean"; //$NON-NLS-1$
	public static final String TYPE_BASIC_FLOAT = "float"; //$NON-NLS-1$
	public static final String TYPE_BASIC_DATE = "date"; //$NON-NLS-1$
	public static final String ELEMENT_ITEM = "item"; //$NON-NLS-1$
	public static final String ELEMENT_VALUE = "value"; //$NON-NLS-1$
	public static final String ELEMENT_INDEX = "index"; //$NON-NLS-1$
	public static final String ELEMENT_TRANSLATE = "translate"; //$NON-NLS-1$
	public static final String ELEMENT_DEFAULT = "default"; //$NON-NLS-1$

	private List<Button> buttons;
	private ISWTRenderer renderer;
	private Group group;
	private String type;
	private Object currentValue;
	private String attribute;
	private final List<SelectionListener> listeners = new ArrayList<>();

	@Override
	public void create(ISWTRenderer swtRenderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		renderer = swtRenderer;
		attribute = element.getCode();
		final boolean translate = parameters.getParameterBoolean(ELEMENT_TRANSLATE);
		type = parameters.getParameter("type");
		String groupLabel = parameters.getParameter(LABEL, element.getName());
		if ((groupLabel != null) && translate) {
			groupLabel = renderer.getLocalizedMessage(groupLabel);
		}
		group = GuiFormatTools.createGroup(renderer.getParent(), groupLabel);
		final List<ElementParameter> elements = parameters.getListElementParameter(ELEMENT_ITEM);
		buttons = new ArrayList<>(elements.size());
		Button tmp;
		int index;
		String label;
		final int elemntNb = elements.size();
		for (final ElementParameter elementParameter : elements) {
			label = elementParameter.getParameter("label");
			if ((label != null) && translate) {
				label = renderer.getLocalizedMessage(label);
			}
			index = elementParameter.getParameterInteger(ELEMENT_INDEX, -1);
			if ((index > -1) && (index < elemntNb)) {
				tmp = GuiFormatTools.createRadioButton(group, label);
				tmp.setData(ELEMENT_VALUE, elementParameter.getParameter(ELEMENT_VALUE));
				buttons.add(index, tmp);
				if (elementParameter.getParameterBoolean(ELEMENT_DEFAULT)) {
					tmp.setSelection(true);
					currentValue = elementParameter.getParameter(ELEMENT_VALUE);
				}
				tmp.addSelectionListener(this);
			}
		}
		group.setEnabled(!element.isReadonly() && !parameters.getParameterBoolean(READ_ONLY));
		renderer.getRendererBinding().bindElement(element, this);
		renderer.addLoadListener(new IBeanMapChangedListener() {
			@Override
			public void changed(BeanMapEvent event) {
				// force default attribute if not already set
				if (!renderer.getCurrentBean().contains(attribute)) {
					renderer.put(attribute, currentValue);
				}
			}
		});
	}

	@Override
	public void dispose() {
		listeners.clear();
	}

	@Override
	public Control getWidget() {
		return group;
	}

	@Override
	public Object getValue() {
		return currentValue;
	}

	@Override
	public void addSelectionListener(SelectionListener selectionListener) {
		listeners.add(selectionListener);
	}

	@Override
	public void setValue(Object newValue) {
		if (newValue instanceof Integer) {
			final Button selected = buttons.get((Integer) newValue);
			selected.setSelection(true);
		}
	}

	@Override
	public Object getValueType() {
		if (type.equals(TYPE_BASIC_INTEGER)) {
			return Integer.class;
		}
		if (type.equals(TYPE_BASIC_STRING)) {
			return String.class;
		}
		if (type.equals(TYPE_BASIC_DATE)) {
			return Date.class;
		}
		if (type.equals(TYPE_BASIC_FLOAT)) {
			return Double.class;
		}
		if (type.equals(TYPE_BASIC_BOOLEAN)) {
			return Boolean.class;
		}
		return String.class;
	}

	@Override
	public void widgetSelected(SelectionEvent e) {
		if ((e.widget instanceof Button) && buttons.contains(e.widget)) {
			final Button w = (Button) e.widget;
			if (w.getSelection()) {
				currentValue = w.getData(ELEMENT_VALUE);
				renderer.put(attribute, currentValue);
				for (final SelectionListener listener : listeners) {
					listener.widgetSelected(e);
				}
			}
		}
	}

	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
		widgetSelected(e);
	}
}