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
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EXPANDED;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LARGE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.SECTION_DESCRIPTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TITLE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWISTIE;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.Section;

import com.arcadsoftware.client.editors.swtwidgets.listeners.ScrolledCompositeExpansionListener;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IContainerSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Section Container SWT Widget provider for the dynamic editors.
 */
public class SectionContainerSWTProvider implements IContainerSWTProvider {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure) {
		int style = ExpandableComposite.TREE_NODE;
		if (parameters.getParameterBoolean(TWISTIE)) {
			style = ExpandableComposite.TWISTIE;
		}
		if (parameters.getParameterBoolean(LARGE)) {
			style |= ExpandableComposite.TITLE_BAR;
		} else {
			style |= ExpandableComposite.SHORT_TITLE_BAR;
		}
		if (parameters.getParameterBoolean(EXPANDED)) {
			style |= ExpandableComposite.EXPANDED;
		}

		String description = parameters.getParameter(SECTION_DESCRIPTION, EMPTY);
		if ((description != null) && (description.length() > 0)) {
			style |= Section.DESCRIPTION;
			description = renderer.getLocalizedMessage(description);
		}

		final Section section = renderer.getToolkit().createSection(renderer.getParent(), style);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			final GridData gridData = ContainerUtils.createGridData(parameters);
			section.setLayoutData(gridData);
		}
		section.setText(renderer.getLocalizedMessage(parameters.getParameter(TITLE, EMPTY)));
		if ((description != null) && (description.length() > 0)) {
			section.setDescription(description);
		}

		final Composite client = renderer.getToolkit().createComposite(section);
		client.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		section.setClient(client);
		section.addExpansionListener(new ScrolledCompositeExpansionListener());
		client.setLayout(new GridLayout(parameters.getParameterInteger(COLS, 3), false));
		renderer.createSubContainer(this, parameters, client);
	}

	@Override
	public void dispose() {
		// Do nothing
	}

}
