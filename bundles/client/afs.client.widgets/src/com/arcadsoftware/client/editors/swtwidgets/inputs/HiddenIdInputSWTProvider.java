/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a CheckBox SWT Widget provider for the dynamic editors.
 */
public class HiddenIdInputSWTProvider implements IInputSWTProvider, SelectionListener {

	private String internalEditorId;
	private BindedWidget bindedValue;
	private Integer bindedId;
	private Text text;
	private ISWTRenderer renderer;

	private class BindedWidget implements IWidgetValue {
		private List<SelectionListener> listeners;

		@Override
		public void addSelectionListener(SelectionListener selectionListener) {
			if (listeners == null) {
				listeners = new ArrayList<>();
			}
			listeners.add(selectionListener);
		}

		@Override
		public Object getValue() {
			return bindedId;
		}

		@Override
		public Object getValueType() {
			return null;
		}

		@Override
		public Control getWidget() {
			return text;
		}

		@Override
		public void setValue(Object newValue) {
			if (newValue instanceof Integer) {
				bindedId = (Integer) newValue;
				for (final SelectionListener listener : listeners) {
					listener.widgetSelected(null);
				}
			}
		}
	}

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		internalEditorId = parameters.getParameter(IConstants.INTERNAL_EDITOR_ID);
		this.renderer = renderer;
		text = new Text(renderer.getParent(), SWT.BORDER);
		text.setVisible(false);
		bindedValue = new BindedWidget();
		bindedValue.addSelectionListener(this);
		renderer.getRendererBinding().bindElement(element, bindedValue);
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
		// Do nothing
	}

	@Override
	public void widgetSelected(SelectionEvent e) {
		if (internalEditorId != null) {
			if (bindedId > 0) {
				renderer.getInternalEditors().loadInternalEditor(internalEditorId, bindedId);
			}
		}
	}

}
