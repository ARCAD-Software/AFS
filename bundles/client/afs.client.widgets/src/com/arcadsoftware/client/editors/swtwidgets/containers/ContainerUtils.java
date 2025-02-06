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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLSPAN;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FILL_BOTH;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FILL_STYLE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WIDTH;

import org.eclipse.swt.layout.GridData;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;

public class ContainerUtils {
	public static GridData createGridData(ILayoutParameters parameters) {
		GridData gridData;
		final int colSpan = parameters.getParameterInteger(COLSPAN, 3);
		final int width = parameters.getParameterInteger(WIDTH, -1);
		final int height = parameters.getParameterInteger(IConstants.HEIGHT, -1);
		if (parameters.getParameterBoolean(FILL_BOTH)) {
			gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
		} else {
			final String fillStyle = parameters.getParameter(FILL_STYLE, IConstants.FILL_STYLE_BOTH);
			if (fillStyle.equalsIgnoreCase(IConstants.FILL_STYLE_HORIZONTAL)) {
				gridData = new GridData(GridData.FILL_HORIZONTAL);
				gridData.grabExcessHorizontalSpace = true;
				gridData.grabExcessVerticalSpace = false;
			} else if (fillStyle.equalsIgnoreCase(IConstants.FILL_STYLE_VERTICAL)) {
				gridData = new GridData(GridData.FILL_VERTICAL);
				gridData.grabExcessHorizontalSpace = false;
				gridData.grabExcessVerticalSpace = true;
			} else {
				gridData = new GridData(GridData.FILL_BOTH);
				gridData.grabExcessHorizontalSpace = true;
				gridData.grabExcessVerticalSpace = true;
			}
		}
		gridData.horizontalSpan = colSpan;
		if (width > -1) {
			gridData.widthHint = width;
		}
		if (height > -1) {
			gridData.heightHint = height;
		}
		return gridData;
	}
}
