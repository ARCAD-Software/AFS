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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DIGITS;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MAX;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MIN;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Spinner;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IEditorChangeListener;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Spinner SWT Widget provider for the dynamic editors.
 */
public class SpinnerInputSWTProvider implements IInputSWTProvider, IEditorChangeListener {

	Spinner spinner;
	int digits;
	private String enabledIfProperty;

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		digits = parameters.getParameterInteger(DIGITS, 0);
		final int minimum = parameters.getParameterInteger(MIN, Integer.MIN_VALUE);
		final int maximum = parameters.getParameterInteger(MAX, Integer.MAX_VALUE);
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}

		spinner = new Spinner(renderer.getParent(), SWT.BORDER);
		spinner.setEnabled(!element.isReadonly());
		spinner.setMinimum(minimum);
		spinner.setMaximum(maximum);
		if (parameters.getParameterBoolean(DEFAULT)) {
			spinner.setFocus();
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			if (label.length() > 0) {
				spinner.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			} else {
				spinner.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, 3, 1));
			}
		}
		spinner.setDigits(digits);
		// TODO RAP
		// renderer.getToolkit().paintBordersFor(renderer.getParent());

		enabledIfProperty = parameters.getParameter(IConstants.ENABLED_IF, null);
		if (enabledIfProperty != null) {
			handleEnabledIfChange(renderer);
			renderer.addChangeListener(this);
		}

		if (digits != 0) {
			renderer.getRendererBinding().bindElement(element, new IWidgetValue() {
				@Override
				public void addSelectionListener(SelectionListener selectionListener) {
					spinner.addSelectionListener(selectionListener);
				}

				@Override
				public Object getValue() {
					Double value = Double.valueOf(spinner.getSelection());
					for (int i = 0; i < digits; i++) {
						value /= 10;
					}
					return value;
				}

				@Override
				public Object getValueType() {
					return Spinner.class;
				}

				@Override
				public Control getWidget() {
					return spinner;
				}

				@Override
				public void setValue(Object newValue) {
					Double value = Double.parseDouble((String) newValue);
					for (int i = 0; i < digits; i++) {
						value *= 10;
					}
					spinner.setSelection(value.intValue());
				}
			});
		} else {
			renderer.getRendererBinding().bindElement(element, spinner);
		}
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	@Override
	public void changed(ISWTRenderer renderer) {
		handleEnabledIfChange(renderer);
	}

	private void handleEnabledIfChange(final ISWTRenderer renderer) {
		if ((spinner != null) && !spinner.isDisposed() && (enabledIfProperty != null)) {
			final boolean enabled = renderer.getCurrentBean().getBoolean(enabledIfProperty);
			spinner.getDisplay().asyncExec(() -> spinner.setEnabled(enabled));
		}
	}

}
