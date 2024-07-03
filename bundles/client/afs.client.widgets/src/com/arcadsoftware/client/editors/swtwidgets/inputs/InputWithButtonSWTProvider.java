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
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class InputWithButtonSWTProvider extends MinimalistInputSWTProvider {
	private IEditorAction action;

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		super.create(renderer, parameters, element, structure);

		if (element instanceof MetaDataAttribute) {
			// Init action
			final String actionId = parameters.getParameter("action");
			if (actionId != null) {
				final String buttonLabel = renderer
						.getLocalizedMessage(parameters.getParameter("actionLabel", "action"));
				final Button button = renderer.getToolkit().createButton(renderer.getParent(), buttonLabel, SWT.NONE);
				final String actionTooltip = parameters.getParameter("actionTooltip");
				if (actionTooltip != null) {
					button.setToolTipText(renderer.getLocalizedMessage(actionTooltip));
				}
				button.addSelectionListener(new SelectionListener() {
					@Override
					public void widgetDefaultSelected(SelectionEvent e) {
					}

					@Override
					public void widgetSelected(SelectionEvent e) {
						executeAction();
					}
				});
				createAction(parameters.getParameter("action"), renderer);
			}
		}
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	protected void createAction(String actionId, ISWTRenderer renderer) {
		action = EditorActionFactory.getEditorAction(actionId);
		if (action != null) {
			action.setBeanMapSelector(renderer);
			action.setRenderer(renderer);
		}

	}

	protected void executeAction() {
		if (action != null) {
			action.run();
		}
	}
}
