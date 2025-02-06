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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.HORIZONTAL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ORIENTATION;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Separator Decorator SWT Widget provider for the dynamic editors.
 */
public class SeparatorDecoratorSWTProvider implements IDecoratorSWTProvider {

	@Override
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		final int orientation = (HORIZONTAL.equals(parameters.getParameter(ORIENTATION))) ? SWT.HORIZONTAL
				: SWT.VERTICAL;
		final Label separator = renderer.getToolkit().createSeparator(renderer.getParent(), orientation);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			separator.setLayoutData(new GridData(
					orientation == SWT.HORIZONTAL ? GridData.FILL : GridData.BEGINNING,
					orientation == SWT.VERTICAL ? GridData.FILL : GridData.BEGINNING,
					false, false, 3, 1));
		}
		return separator;
	}

	@Override
	public void dispose() {
		// Do nothing
	}

}
