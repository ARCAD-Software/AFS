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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLS;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FILL_BOTH;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TITLE;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Group;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Group Container SWT Widget provider for the
 * dynamic editors.
 */
public class GroupContainerSWTProvider implements IContainerSWTProvider {

	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure) {
		Group group = new Group(renderer.getParent(),SWT.NONE);
		group.setText(renderer.getLocalizedMessage(parameters.getParameter(TITLE)));
		if (renderer.getParent().getLayout() instanceof GridLayout)
			group.setLayoutData((parameters.getParameterBoolean(FILL_BOTH)) ? new GridData(GridData.FILL,
					GridData.FILL, true, true, 3, 1) : new GridData(GridData.FILL, GridData.FILL, true, false, 3, 1));
		group.setLayout(new GridLayout(parameters.getParameterInteger(COLS, 3), false));
		renderer.getToolkit().adapt(group);
		renderer.createSubContainer(this, parameters, group);
	}

	public void dispose() {
		//Do nothing
	}

}
