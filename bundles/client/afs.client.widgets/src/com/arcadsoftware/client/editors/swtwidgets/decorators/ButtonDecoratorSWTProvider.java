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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.AT_BOTTOM;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.AT_RIGHT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.GRAB_HORIZONTAL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.SHOWTEXT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TOOLTIP;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WIDTH;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Button Decorator SWT Widget provider for the dynamic editors.
 */
public class ButtonDecoratorSWTProvider implements IDecoratorSWTProvider {

	private IEditorAction action;
	private Image image;

	@Override
	public Widget create(final ISWTRenderer renderer, final ILayoutParameters parameters, MetaDataEntity structure) {
		Button button = null;
		final String icon = parameters.getParameter(ICON);
		final String tooltip = parameters.getParameter(TOOLTIP);
		final boolean showText = parameters.getParameterBoolean(SHOWTEXT);
		if (icon != null) {
			final ImageDescriptor imageDescriptor = renderer.getImageDescriptor(icon);
			if (imageDescriptor != null) {
				image = imageDescriptor.createImage();
			}
		}
		if (image == null) {
			button = renderer.getToolkit().createButton(renderer.getParent(),
					renderer.getLocalizedMessage(parameters.getParameter(LABEL)), SWT.PUSH);
		} else {
			if (showText) {
				button = renderer.getToolkit().createButton(renderer.getParent(),
						renderer.getLocalizedMessage(parameters.getParameter(LABEL)), SWT.NONE);
			} else {
				button = renderer.getToolkit().createButton(renderer.getParent(), EMPTY, SWT.NONE);
			}
			button.setImage(image);
		}
		if ((tooltip != null) && (tooltip.length() > 0)) {
			button.setToolTipText(renderer.getLocalizedMessage(parameters.getParameter(TOOLTIP)));
		}
		button.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// IAction action =
				// renderer.getAction(parameters.getParameter(ACTION));
				// if (action != null) {
				// action.run();
				// }
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				executeAction();
			}
		});
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			GridData gridData;
			final int verticalAlignment = (parameters.getParameterBoolean(AT_BOTTOM)) ? GridData.END
					: GridData.BEGINNING;
			if (parameters.getParameterBoolean(GRAB_HORIZONTAL)) {
				gridData = new GridData(GridData.FILL, verticalAlignment, true, false, 3, 1);
			} else {
				final int horizontalAlignment = (parameters.getParameterBoolean(AT_RIGHT)) ? GridData.END
						: GridData.BEGINNING;
				gridData = new GridData(horizontalAlignment, verticalAlignment, false, false, 3, 1);
			}
			final int width = parameters.getParameterInteger(WIDTH, -1);
			if (width > -1) {
				gridData.widthHint = width;
			}

			button.setLayoutData(gridData);
		}

		createAction(parameters.getParameter(ACTION), renderer);
		return button;
	}

	private void createAction(String actionId, ISWTRenderer renderer) {
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

	@Override
	public void dispose() {
		// Do nothing
	}

}
