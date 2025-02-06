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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTableViewer;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.actions.AbstractEditorAction;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;

/**
 * This widget manages a Table with three buttons dedicated to the Add, Update and Delete action</br>
 * This button are displayed into a ButtonBar located under the table
 *
 * @author ARCAD Software
 */
public abstract class TableWithButtonBarSWTProvider extends TableSWTProvider {

	private static final String ACTIONID_ADD = "*add"; //$NON-NLS-1$
	private static final String ACTIONID_DELETE = "*delete";//$NON-NLS-1$
	private static final String ACTIONID_UPDATE = "*update";//$NON-NLS-1$

	IAction dbclickAction;
	HashMap<String, IEditorAction> allActions;
	List<IAction> menuActions;
	List<IAction> toolbarActions;
	protected boolean readOnlyEdition;

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
				if (actionId.equalsIgnoreCase(ACTIONID_ADD)) {
					a = createAddEditorAction();
				} else if (actionId.equalsIgnoreCase(ACTIONID_UPDATE)) {
					a = createEditEditorAction();
				} else if (actionId.equalsIgnoreCase(ACTIONID_DELETE)) {
					a = createRemoveEditorAction();
				} else {
					a = createCustomAction(actionId);
					if (a == null) {
						a = EditorActionFactory.getEditorAction(actionId);
					}
				}
				if (a != null) {
					a.setText(renderer.getLocalizedMessage(label));
					a.setToolTipText(renderer.getLocalizedMessage(label));
					a.setBeanMapSelector(this);
					a.setRenderer(renderer);
					a.setTableViewer(getList());
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

	protected IEditorAction createCustomAction(String actionId) {
		return null;
	}

	@Override
	public void setList(BeanMapTableViewer list) {
		super.setList(list);
		final Iterator<String> it = allActions.keySet().iterator();
		while (it.hasNext()) {
			allActions.get(it.next()).setTableViewer(list);
		}
	}

	private Button createButton(Composite parent, ILayoutParameters parameters, ElementParameter button) {

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
							relayout(e.widget);
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

	protected void relayout(Widget widget) {
		if (widget instanceof Control) {
			final Composite parent = ((Control) widget).getParent().getParent();
			if (parent.getLayout() instanceof TableWrapLayout) {
				parent.getParent().layout(true, true);
			}
		}
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
				if (parent.getLayout() instanceof GridLayout) {
					final GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
					layoutData.horizontalSpan = 3;
					if (height != -1) {
						layoutData.heightHint = height;
					} else {
						layoutData.grabExcessVerticalSpace = true;
						layoutData.verticalAlignment = GridData.FILL_VERTICAL;
					}
					bar.setLayoutData(layoutData);
				} else if (parent.getLayout() instanceof TableWrapLayout) {
					final TableWrapData twd = new TableWrapData(TableWrapData.FILL_GRAB);
					twd.colspan = ((TableWrapLayout) parent.getLayout()).numColumns;
					if (height > 0) {
						twd.heightHint = height;
					} else {
						twd.grabVertical = true;
						twd.valign = TableWrapData.FILL;
					}
					bar.setLayoutData(twd);
				}
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

	private IEditorAction createAddEditorAction() {
		return new AbstractEditorAction() {
			@Override
			public void run() {
				createBeanMap(TableWithButtonBarSWTProvider.this.element, true);
			}
		};
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
				removeBeanMap(TableWithButtonBarSWTProvider.this.element);
			}
		};
	}

	@Override
	protected void createControlBeforeTable(Composite parent) {
		super.createControlBeforeTable(parent);
		manageButtonBar(parent, getLayoutParameters(), IConstants.BEFORE);
	}

	@Override
	protected void createControlAfterTable(Composite parent) {
		super.createControlAfterTable(parent);
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

	public List<IAction> getMenuActions() {
		return menuActions;
	}
}
