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
package com.arcadsoftware.client.editors.swtwidgets.containers;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.COLS;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TITLE;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.ExpandableComposite;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Expandable Container SWT Widget provider for the dynamic editors. NOT IMPLEMENTED --> NOT USED
 */
public class ExpandableContainerSWTProvider implements IContainerSWTProvider {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure) {
		final ExpandableComposite group = renderer.getToolkit().createExpandableComposite(renderer.getParent(),
				ExpandableComposite.TREE_NODE | ExpandableComposite.TITLE_BAR);

		if (renderer.getParent().getLayout() instanceof GridLayout) {
			group.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		}
		group.setText(renderer.getLocalizedMessage(parameters.getParameter(TITLE)));
		final Composite client = renderer.getToolkit().createComposite(group);
		group.setClient(client);
		client.setLayout(new GridLayout(parameters.getParameterInteger(COLS, 3), false));
		renderer.createSubContainer(this, parameters, client);
	}

	@Override
	public void dispose() {
		// Do nothing
	}

}
