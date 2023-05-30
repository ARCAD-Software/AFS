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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

public class ComboListSWTProvider implements IInputSWTProvider, IConstants {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity structure) {
		String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		int style = SWT.DROP_DOWN;
		if (parameters.getParameterBoolean(BORDER)) {
			style |= SWT.BORDER;
		}
		if (parameters.getParameterBoolean("fixed")) { //$NON-NLS-1$
			style |= SWT.READ_ONLY;
		}
		final Combo combo = new Combo(renderer.getParent(), style);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			if (label.length() > 0) {
				combo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false, 1, 1));
			} else {
				combo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, false, false, 3, 1));
			}
		}
		for (ElementParameter item : parameters.getListElementParameter("item")) { //$NON-NLS-1$
			combo.add(parameters.getElementParameter(item, "value")); //$NON-NLS-1$
		}
		combo.setEnabled(!element.isReadonly());
		renderer.getRendererBinding().bindElement(element, combo);
		if (parameters.getParameterBoolean(MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}
		if (parameters.getParameterBoolean(DEFAULT)) {
			combo.setFocus();
		}
	}

	@Override
	public void dispose() {
	}

}
