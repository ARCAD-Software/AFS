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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FILL_HORIZONTAL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FILL_VERTICAL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.HORIZONTAL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ORIENTATION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.RATIO;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IValidatingSubWidgets;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a SashForm Container SWT Widget provider for the dynamic editors.
 */
public class SashFormContainerSWTProvider implements IContainerSWTProvider, IValidatingSubWidgets {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure) {
		final int orientation = (HORIZONTAL.equals(parameters.getParameter(ORIENTATION))) ? SWT.HORIZONTAL
				: SWT.VERTICAL;
		final SashForm mainSashForm = new SashForm(renderer.getParent(), orientation);
		mainSashForm.SASH_WIDTH = 2;
		final int ratio = parameters.getParameterInteger(RATIO, 50);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			mainSashForm.setLayoutData(new GridData(GridData.FILL, GridData.FILL, parameters
					.getParameterBoolean(FILL_HORIZONTAL), parameters.getParameterBoolean(FILL_VERTICAL), 3, 1));
		}
		renderer.getToolkit().adapt(mainSashForm);
		final GridLayout layout = new GridLayout();
		layout.marginBottom = layout.marginHeight = layout.marginLeft = layout.marginRight = layout.marginTop = layout.marginWidth = 0;
		mainSashForm.setLayout(layout);
		renderer.createSubContainer(this, parameters, mainSashForm);
		mainSashForm.setWeights(new int[] { ratio, 100 - ratio });
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	@Override
	public boolean acceptDecorator(IDecoratorSWTProvider provider) {
		return false;
	}

	@Override
	public boolean acceptInput(IInputSWTProvider provider) {
		return false;
	}

	@Override
	public boolean acceptSubContainer(IContainerSWTProvider provider) {
		return true;
	}

}
