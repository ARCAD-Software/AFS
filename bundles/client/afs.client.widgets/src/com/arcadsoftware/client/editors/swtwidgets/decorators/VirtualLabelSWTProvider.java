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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

public class VirtualLabelSWTProvider implements IDecoratorSWTProvider {

	private String defaultText;
	private Label label;

	@Override
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		final String lbx = parameters.getParameter("label", ""); //$NON-NLS-1$ //$NON-NLS-2$
		// Base label
		if (lbx.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), renderer.getLocalizedMessage(lbx));
			renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
		}
		defaultText = renderer.getLocalizedMessage(parameters.getParameter("default", "")); //$NON-NLS-1$ //$NON-NLS-2$
		label = renderer.getToolkit().createLabel(renderer.getParent(), defaultText);
		if ((lbx.length() == 0) && (renderer.getParent().getLayout() instanceof GridLayout)) {
			// fill the grid line.
			label.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false,
					((GridLayout) renderer.getParent().getLayout()).numColumns, 1));
		}
		final String virtualKey = parameters.getParameter("virtual", ""); //$NON-NLS-1$ //$NON-NLS-2$
		final Object objectValue = renderer.getVirtualValue(virtualKey);
		String value = "";
		if (objectValue instanceof String) {
			if (objectValue != null) {
				value = (String) objectValue;
			}
		}
		label.setText(value);
		return label;
	}

	@Override
	public void dispose() {

	}

}
