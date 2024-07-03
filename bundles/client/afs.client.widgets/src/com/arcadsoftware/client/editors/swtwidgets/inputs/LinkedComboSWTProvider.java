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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.client.editors.swtwidgets.widgets.BeanMapLinkedCombo;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainer;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Combo SWT Widget provider for the dynamic editors.
 */
public class LinkedComboSWTProvider extends AbstractSWTProvider {

	@Override
	public IBeanMapContainer createContainer(ISWTRenderer renderer,
			ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		final String label = renderer.getLocalizedMessage(parameters.getParameter("label", element.getName())); //$NON-NLS-1$
		int horizontalSpan = 3;
		if (label.length() > 0) {
			horizontalSpan = 1;
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), ":"); //$NON-NLS-1$
		}
		final BeanMapLinkedCombo combo = new BeanMapLinkedCombo( //
				renderer.getParent(), //
				SWT.READ_ONLY, //
				parameters.getParameter("linkCode"), //$NON-NLS-1$
				structure.getAttribute(parameters.getParameter("linked")), //$NON-NLS-1$
				parameters, //
				renderer, //
				element, //
				horizontalSpan);
		configureWidget((Combo) combo.getWidget(), renderer, parameters, element);
		return combo;
	}

}
