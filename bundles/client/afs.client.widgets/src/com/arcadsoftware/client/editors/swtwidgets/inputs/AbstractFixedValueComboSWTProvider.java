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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.client.editors.swtwidgets.widgets.AbstractFixedCombo;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Combo SWT Widget provider for the dynamic editors.
 */
public abstract class AbstractFixedValueComboSWTProvider implements IInputSWTProvider {
	
	private FixedCombo combo;
	protected ISWTRenderer renderer;
	protected ILayoutParameters parameters;

	private class FixedCombo extends AbstractFixedCombo {

		public FixedCombo(Composite parent, int style,
				ILayoutParameters parameters, ISWTRenderer renderer,
				Element element, int horizontalSpan) {
			super(parent, style, parameters, renderer, element, horizontalSpan);
		}

		@Override
		public void fill(Combo combo) {
			AbstractFixedValueComboSWTProvider.this.fill(combo);
		}

		@Override
		public int valueToIndex(Object value) {
			return AbstractFixedValueComboSWTProvider.this.valueToIndex(value);
		}

		@Override
		public Object indexToValue(int index) {
			return AbstractFixedValueComboSWTProvider.this.indexToValue(index);
		}

		@Override
		public Object getType() {
			return AbstractFixedValueComboSWTProvider.this.getResultType();
		}
		
	}
	
	public void create(ISWTRenderer swtRenderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		renderer = swtRenderer;
		this.parameters = parameters;
		String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		int horizontalSpan = (label.length() > 0) ? 1 : 3;
		int style = 0;
		if (parameters.getParameterBoolean("readOnly")) { //$NON-NLS-1$
			style |= SWT.READ_ONLY;
		}
		if (parameters.getParameterBoolean("border")) { //$NON-NLS-1$
			style |= SWT.BORDER;
		}
		if (parameters.getParameterBoolean("drop")) { //$NON-NLS-1$
			style |= SWT.DROP_DOWN;
		}
		combo = new FixedCombo(renderer.getParent(), style, parameters, renderer, element, horizontalSpan);
		Combo widget = (Combo) combo.getWidget();
		widget.setEnabled(!element.isReadonly());
		//widget.setEnabled(true);
		if (parameters.getParameterBoolean(DEFAULT))
			widget.setFocus();
		//TODO RAP
		//renderer.getToolkit().paintBordersFor(renderer.getParent());		
		if (parameters.getParameterBoolean(MANDATORY))
			renderer.addMandatoryAttribute(element.getCode());
		renderer.getRendererBinding().bindElement(element, combo);
	}

	public void dispose() {
	}
	
	public abstract void fill(Combo combo);
	
	public abstract int valueToIndex(Object value);
	
	public abstract Object indexToValue(int index);
	
	public abstract Object getResultType();
}
