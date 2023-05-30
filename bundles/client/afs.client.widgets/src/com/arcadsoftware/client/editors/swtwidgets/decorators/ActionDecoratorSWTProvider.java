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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.IN_FORM_TOOL_BAR;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.IN_TOOL_BAR;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MENU_LABEL;

import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement an Action Decorator SWT Widget provider for the dynamic
 * editors. This decorator is not visible in editor.
 */
public class ActionDecoratorSWTProvider implements IDecoratorSWTProvider {

	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL));
		String icon = parameters.getParameter(ICON);

		IEditorAction action = EditorActionFactory.getEditorAction(parameters.getParameter(ACTION));
		if (action != null) {
			action.setText(label);
			action.setToolTipText(label);
			if (icon != null)
				action.setImageDescriptor(renderer.getImageDescriptor(icon));
			action.setBeanMapSelector(renderer);
			action.setRenderer(renderer);
			if (parameters.getParameterBoolean(IN_TOOL_BAR))
				renderer.getRendererActions().addToolBarAction(action);

			String menuLabel = renderer.getLocalizedMessage(parameters.getParameter(MENU_LABEL));
			if (menuLabel != null)
				renderer.getRendererActions().addMenuAction(menuLabel, action);
			if (parameters.getParameterBoolean(IN_FORM_TOOL_BAR))
				renderer.addActionOnFormToolBar(action);
		}
		return null;
	}

	public void dispose() {
		// Do nothing
	}

}
