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

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IValidatingSubWidgets;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Panels Container SWT Widget provider for the dynamic editors. Panels container can contain
 * only Panel container
 *
 * @see PanelSWTProvider
 */
public class PanelsSWTProvider implements IContainerSWTProvider, IValidatingSubWidgets {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters params, boolean isEmpty, MetaDataEntity structure) {
		int style = SWT.MULTI;
		if (params.getParameterBoolean(IConstants.BORDER)) {
			style |= SWT.BORDER;
		}
		if (IConstants.BOTTOM.equalsIgnoreCase(params.getParameter(IConstants.ALIGN))) {
			style |= SWT.BOTTOM;
		} else {
			style |= SWT.TOP;
		}
		final CTabFolder panels = new CTabFolder(renderer.getParent(), style);
		// If there is a parent provider then we must adapt the panels to the
		// Eclipse Forms style !
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			final GridData layoutData = new GridData();
			layoutData.horizontalAlignment = GridData.FILL;
			layoutData.verticalAlignment = GridData.FILL;
			layoutData.grabExcessHorizontalSpace = true;
			layoutData.horizontalSpan = 3;
			layoutData.grabExcessVerticalSpace = params.getParameterBoolean(IConstants.FILL_BOTH);
			panels.setLayoutData(layoutData);
		}
		if (renderer.getParentProvider() != null) {
			renderer.getToolkit().adapt(panels);
		}
		renderer.createSubContainer(this, params, panels);
		if (panels.getItemCount() > 0) {
			panels.setSelection(0);
		}
		if (panels.getItemCount() == 1) {
			panels.setTabHeight(0);
		}
	}

	@Override
	public void dispose() {
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
		return provider instanceof PanelSWTProvider;
	}

}
