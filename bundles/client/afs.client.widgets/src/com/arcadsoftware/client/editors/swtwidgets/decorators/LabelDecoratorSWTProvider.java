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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Label Decorator SWT Widget provider for the dynamic editors.
 */
public class LabelDecoratorSWTProvider implements IDecoratorSWTProvider {

	@Override
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		final Label label = renderer.getToolkit().createLabel(renderer.getParent(),
				renderer.getLocalizedMessage(parameters.getParameter(LABEL)));
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			label.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false, 3, 1));
		}
		return label;
	}

	@Override
	public void dispose() {
		// Do nothing
	}

}
