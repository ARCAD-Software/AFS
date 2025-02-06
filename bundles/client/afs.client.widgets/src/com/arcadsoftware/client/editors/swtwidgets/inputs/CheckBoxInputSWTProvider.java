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
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT_VALUE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.READ_ONLY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TOOLTIP;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a CheckBox SWT Widget provider for the dynamic editors.
 */
public class CheckBoxInputSWTProvider implements IInputSWTProvider {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity entity) {
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		final Button checkBox;
		// [ML] 2021 - New parameter transform the check box into a Radio box !
		if (parameters.getParameterBoolean("exclusive")) { //$NON-NLS-1$
			checkBox = new Button(renderer.getParent(), SWT.RADIO);
		} else {
			checkBox = new Button(renderer.getParent(), SWT.CHECK);
		}
		if (label.length() == 0) {
			final GridData layoutData = new GridData();
			layoutData.horizontalSpan = 3;
			checkBox.setLayoutData(layoutData);
		}
		checkBox.setEnabled(!element.isReadonly() && !parameters.getParameterBoolean(READ_ONLY));
		if (parameters.getParameterBoolean(DEFAULT)) {
			checkBox.setFocus();
		}
		final boolean value = parameters.getParameterBoolean(DEFAULT_VALUE, false);
		checkBox.setSelection(value);
		final String tooltip = renderer.getLocalizedMessage(parameters.getParameter(TOOLTIP));
		if ((tooltip != null) && (tooltip.length() > 0)) {
			checkBox.setToolTipText(tooltip);
		}
		renderer.getRendererBinding().bindElement(element, checkBox);
	}

	@Override
	public void dispose() {
	}
}