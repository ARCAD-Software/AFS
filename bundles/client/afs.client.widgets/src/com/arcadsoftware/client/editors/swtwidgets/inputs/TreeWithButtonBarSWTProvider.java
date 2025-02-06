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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTreeViewer;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.actions.AbstractEditorAction;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;

/**
 * This widget manages a Tree with three buttons dedicated to the Add, Update and Delete action</br>
 * These buttons are displayed into a ButtonBar located under the tree
 */
public abstract class TreeWithButtonBarSWTProvider extends TreeSWTProvider {

	private static final String ACTIONID_DELETE = "*delete";//$NON-NLS-1$
	private static final String ACTIONID_UPDATE = "*update";//$NON-NLS-1$

	IAction dbclickAction = null;
	HashMap<String, IEditorAction> allActions;
	List<IAction> menuActions;
	List<IAction> toolbarActions;

	protected boolean readOnlyEdition = false;

	@Override
	protected void manageActions(ILayoutParameters parameters) {
		allActions = new HashMap<>();
		menuActions = new ArrayList<>();
		toolbarActions = new ArrayList<>();
		final List<ElementParameter> actions = parameters.getListElementParameter(IConstants.EDITOR_ACTION);
		for (final ElementParameter action : actions) {
			final String actionId = parameters.getElementParameter(action, IConstants.ACTION);
			final String label = parameters.getElementParameter(action, IConstants.LABEL);
			final String icon = parameters.getElementParameter(action, IConstants.ICON);
			final boolean dbclick = parameters.getElementParameterBoolean(action, IConstants.DBCLICK);
			final boolean showInMenu = parameters.getElementParameterBoolean(action, IConstants.SHOW_INMENU);
			final boolean showInToolbar = parameters.getElementParameterBoolean(action, IConstants.SHOW_INTOOLBAR);
			IEditorAction a = null;
			if (actionId != null) {
				if (actionId.equalsIgnoreCase(ACTIONID_UPDATE)) {
					a = createEditEditorAction();
				} else if (actionId.equalsIgnoreCase(ACTIONID_DELETE)) {
					a = createRemoveEditorAction();
				} else {
					a = EditorActionFactory.getEditorAction(actionId);
				}
				if (a != null) {
					a.setText(renderer.getLocalizedMessage(label));
					a.setToolTipText(renderer.getLocalizedMessage(label));
					a.setBeanMapSelector(this);
					a.setRenderer(renderer);
					a.setTreeViewer(getList());
					a.setElement(element);
					if (icon != null) {
						a.setImageDescriptor(renderer.getImageDescriptor(icon));
					}
					if (showInMenu) {
						menuActions.add(a);
					}
					if (showInToolbar) {
						menuActions.add(a);
					}
					allActions.put(actionId, a);
				}
				if (dbclick) {
					dbclickAction = a;
				}
			}
		}
	}

	@Override
	public void setList(BeanMapTreeViewer list) {
		super.setList(list);
		final Iterator<String> it = allActions.keySet().iterator();
		while (it.hasNext()) {
			allActions.get(it.next()).setTreeViewer(list);
		}
	}

	protected Button createButton(Composite parent, ILayoutParameters parameters, ElementParameter button) {
		String text = parameters.getElementParameter(button, IConstants.LABEL);
		if (text != null) {
			text = renderer.getLocalizedMessage(text);
		} else {
			text = ""; //$NON-NLS-1$
		}

		final Button b = getRenderer().getToolkit().createButton(parent, text, SWT.PUSH);
		final int anchor = parameters.getElementParameterInteger(button, IConstants.ANCHOR, 0);
		final int offset = parameters.getElementParameterInteger(button, IConstants.OFFSET, 0);
		final int width = parameters.getElementParameterInteger(button, IConstants.WIDTH, -1);
		final int height = parameters.getElementParameterInteger(button, IConstants.HEIGHT, -1);

		final FormData fData = new FormData();
		fData.top = new FormAttachment(0, 0);
		fData.height = height;
		fData.width = width;
		if (anchor == 0) {// left anchor
			fData.left = new FormAttachment(0, offset);
		} else {
			fData.left = new FormAttachment(100, offset);
		}
		b.setLayoutData(fData);

		final String actionId = parameters.getElementParameter(button, IConstants.ACTIONID);
		final IAction action = allActions.get(actionId);
		if (action != null) {
			b.addSelectionListener(
					new SelectionAdapter() {
						@Override
						public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
							action.run();
						}
					});
			if (text.equalsIgnoreCase("")) { //$NON-NLS-1$
				b.setText(action.getText());
			}
			if (action.getImageDescriptor() != null) {
				b.setImage(action.getImageDescriptor().createImage());
			}
		}
		return b;
	}

	private void manageButtonBar(Composite parent, ILayoutParameters parameters, String position) {
		final List<ElementParameter> buttons = parameters.getListElementParameter(IConstants.BUTTON);
		final List<ElementParameter> buttonBars = parameters.getListElementParameter(IConstants.BUTTONBAR);
		for (final ElementParameter buttonBar : buttonBars) {
			final String id = parameters.getElementParameter(buttonBar, IConstants.ID);
			final String pos = parameters.getElementParameter(buttonBar, IConstants.POSITION);
			final int height = parameters.getElementParameterInteger(buttonBar, IConstants.HEIGHT, -1);
			if (pos.equalsIgnoreCase(position)) {
				// create de la barre
				final Composite bar = new Composite(parent, SWT.NONE);
				final GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
				layoutData.horizontalSpan = 3;
				if (height != -1) {
					layoutData.heightHint = height;
				} else {
					layoutData.grabExcessVerticalSpace = true;
					layoutData.verticalAlignment = GridData.FILL_VERTICAL;
				}
				bar.setLayoutData(layoutData);
				bar.setLayout(new FormLayout());
				for (final ElementParameter button : buttons) {
					final String barId = parameters.getElementParameter(button, IConstants.BUTTONBARID);
					if (barId.equalsIgnoreCase(id)) {
						createButton(bar, parameters, button);
					}
				}
			}
		}
	}

	private IEditorAction createEditEditorAction() {
		return new AbstractEditorAction() {
			@Override
			public void run() {
				updateBeanMap();
			}
		};
	}

	private IEditorAction createRemoveEditorAction() {
		return new AbstractEditorAction() {
			@Override
			public void run() {
				removeBeanMap(TreeWithButtonBarSWTProvider.this.element);
			}
		};
	}

	@Override
	protected void createControlBeforeTree(Composite parent) {
		super.createControlBeforeTree(parent);
		manageButtonBar(parent, getLayoutParameters(), IConstants.BEFORE);
	}

	@Override
	protected void createControlAfterTree(Composite parent) {
		super.createControlAfterTree(parent);
		manageButtonBar(parent, getLayoutParameters(), IConstants.AFTER);
	}

	@Override
	protected void doActionOnDoubleClick() {
		if (dbclickAction != null) {
			dbclickAction.run();
		} else {
			super.doActionOnDoubleClick();
		}
	}

	public String getEditionLayoutName() {
		return getLayoutParameters().getParameter(IConstants.LAYOUT_NAME, "");
	}

	public HashMap<String, IEditorAction> getAllActions() {
		return allActions;
	}
}
