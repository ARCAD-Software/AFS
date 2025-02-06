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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

public class DateLabelSWTProvider implements IDecoratorSWTProvider {

	private Label label;

	@Override
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		final String lbx = parameters.getParameter("label", ""); //$NON-NLS-1$ //$NON-NLS-2$
		// Base label
		if (lbx.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), renderer.getLocalizedMessage(lbx));
			renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
		}
		label = renderer.getToolkit().createLabel(renderer.getParent(), "");
		if ((lbx.length() == 0) && (renderer.getParent().getLayout() instanceof GridLayout)) {
			// fill the grid line.
			label.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false,
					((GridLayout) renderer.getParent().getLayout()).numColumns, 1));
		}
		final String attribute = parameters.getParameter("attribute", ""); //$NON-NLS-1$ //$NON-NLS-2$
		String format = parameters.getParameter("format", ""); //$NON-NLS-1$ //$NON-NLS-2$
		final Date date = renderer.getCurrentBean().getDate(attribute);
		String value = "";
		if (date != null) {
			if (date instanceof Date) {
				if ((format == null) || (format.length() == 0)) {
					format = "yyyy/MM/dd HH:mm:ss";
				}
				final SimpleDateFormat sdf = new SimpleDateFormat(format);
				value = sdf.format(date);
			}
		}
		label.setText(value);
		return label;
	}

	@Override
	public void dispose() {

	}

}
