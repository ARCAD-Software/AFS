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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.util.ArrayList;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.EvolutionCoreUIPlugin;
import com.arcadsoftware.aev.core.ui.tools.CoreUILabels;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.viewers.ConnectedBeanMapListTableViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractConnectedBeanMapTableManager extends AbstractConnectedComposite {

	ConnectedBeanMapListTableViewer viewer;
	private final MetaDataEntity entity;

	private BeanMap selectedItem;

	private IAction addAction;
	private IAction editAction;
	private IAction deleteAction;

	private boolean allowEdit;
	private boolean allowMultiSelection;

	private Button bAdd;
	private Button bDelete;
	private boolean readonly = false;

	public AbstractConnectedBeanMapTableManager(Composite parent, int style, ServerConnection connection,
			boolean allowEdit) {
		this(parent, style, connection, allowEdit, false);
	}

	public AbstractConnectedBeanMapTableManager(Composite parent, int style, ServerConnection connection,
			boolean allowEdit, boolean multiSelection) {
		super(parent, style, connection, false);
		format();
		entity = getHelper().getEntity(getType());
		this.allowEdit = allowEdit;
		allowMultiSelection = multiSelection;
		makeAction();
		createContent(this);
	}

	public AbstractConnectedBeanMapTableManager(Composite parent, int style, ServerConnection connection) {
		super(parent, style, connection, false);
		format();
		entity = getHelper().getEntity(getType());
		makeAction();
		createContent(this);
	}

	@Override
	protected void format() {
		final GridLayout grid = new GridLayout(3, false);
		grid.marginHeight = 0;
		grid.marginWidth = 0;
		setLayout(grid);
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		setLayoutData(gridData);
	}

	private void makeAction() {
		if (getAllowAdd()) {
			addAction = new Action() {
				@Override
				public void run() {
					add();
				}
			};
		}

		if (getAllowEdit()) {
			editAction = new Action() {
				@Override
				public void run() {
					update();
				}
			};
		}
		if (getAllowDelete()) {
			deleteAction = new Action() {
				@Override
				public void run() {
					delete();
				}
			};
		}

	}

	protected boolean getAllowEdit() {
		return allowEdit;
	}

	protected boolean getAllowDelete() {
		return true;
	}

	protected boolean getAllowAdd() {
		return true;
	}

	@Override
	public void createContent(Composite parent) {

		int style = SWT.FULL_SELECTION;
		if (allowMultiSelection) {
			style = style | SWT.MULTI;
		}

		viewer = new ConnectedBeanMapListTableViewer(getConnection(), parent, style, entity, getAttributeList()) {
			@Override
			protected Image getCustomColumnImage(Object element, int actualColumnIndex) {
				return AbstractConnectedBeanMapTableManager.this.getCustomColumnImage(element, actualColumnIndex);
			}

			@Override
			protected int getColumnSize(String attribute) {
				final int size = getUserColumnSize(attribute);
				if (size == -1) {
					return super.getColumnSize(attribute);
				}
				return size;
			}

			@Override
			public String getIdentifier() {
				return null;
			}

			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				AbstractConnectedBeanMapTableManager.this.onSelection(selection);
			}

			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				onDoubleClick(selection);
			}

			@Override
			public String getValue(Object element, int columnIndex) {
				final String s = AbstractConnectedBeanMapTableManager.this.getValue(element, columnIndex);
				if (s == null) {
					return super.getValue(element, columnIndex);
				} else {
					return s;
				}
			}

		};

		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.verticalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = 3;
		viewer.getTable().setLayoutData(gridData);
		/// create button bar
		final Composite buttonBar = new Composite(parent, SWT.NONE);
		gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalSpan = 3;
		gridData.heightHint = 25;
		buttonBar.setLayoutData(gridData);
		buttonBar.setLayout(new FormLayout());

		if (getAllowAdd() || getAllowEdit() || getAllowDelete()) {
			// create button
			if (getAllowAdd()) {
				bAdd = createButton(buttonBar,
						Activator.resString(isCreateDeleteStyle() ? "button.create.text" : "button.add.text"),
						isCreateDeleteStyle() ? AFSIcon.CREATE.imageDescriptor() : AFSIcon.ADD.imageDescriptor(),
						addAction,
						1, -100, 100, 25);
			}
			if (getAllowEdit()) {
				createButton(buttonBar,
						Activator.resString("button.edit.text"),
						AFSIcon.EDIT.imageDescriptor(),
						editAction,
						1, (getAllowAdd() ? -210 : -100), 100, 25);
			}
			if (getAllowDelete()) {
				bDelete = createButton(buttonBar,
						Activator.resString(isCreateDeleteStyle() ? "button.delete.text" : "button.remove.text"),
						isCreateDeleteStyle() ? AFSIcon.DELETE.imageDescriptor() : AFSIcon.REMOVE.imageDescriptor(),
						deleteAction,
						0, -0, 100, 25);
			}
		}

	}

	private Button createButton(Composite buttonBar,
			String label,
			ImageDescriptor imageDescriptor,
			final IAction action,
			int anchor, int offset, int width, int height) {
		final Button b = new Button(buttonBar, SWT.PUSH);
		b.setText(label);
		b.setImage(imageDescriptor.createImage());
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

		if (action != null) {
			b.addSelectionListener(
					new SelectionAdapter() {
						@Override
						public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
							action.run();
						}
					});
		}
		return b;
	}

	protected int getUserColumnSize(String attribute) {
		return -1;
	}

	public void onSelection(IStructuredSelection selection) {
		if (!selection.isEmpty()) {
			setSelectedBeanMap((BeanMap) selection.getFirstElement());
		} else {
			setSelectedBeanMap(null);
		}
	}

	protected boolean doOnAdd() {
		return true;
	}

	/**
	 * @param updated
	 */
	protected boolean doOnUpdate(BeanMap updated) {
		return true;
	}

	/**
	 * @param deleted
	 */
	protected boolean doOnDelete(BeanMap deleted) {
		return true;
	}

	public void setSelectedBeanMap(BeanMap selectedItem) {
		this.selectedItem = selectedItem;
	}

	public BeanMap getSelectedBeanMap() {
		return selectedItem;
	}

	public BeanMapList getSelectedBeanMapList() {
		return viewer.getSelectedBeanMapList();
	}

	public void add() {
		if (doOnAdd()) {
			refresh();
		}
	}

	public void refresh() {
		viewer.refresh();
	}

	@Override
	public void update() {
		if (allowMultiSelection) {
			final BeanMapList selectedList = viewer.getSelectedBeanMapList();
			for (final BeanMap bean : selectedList) {
				if (isValidItem(bean)) {
					if (doOnUpdate(bean)) {
						refresh();
					}
				}
			}
		} else {
			if (getSelectedBeanMap() != null) {
				if (isValidItem(getSelectedBeanMap())) {
					if (doOnUpdate(getSelectedBeanMap())) {
						refresh();
					}
				}
			}
		}
	}

	protected String getConfirmationDialogTitle() {
		return "ARCAD";
	}

	protected String getDeletionConfirmationMessage(boolean multi) {
		return CoreUILabels.resString("confirmDeletion.text");
	}

	public void delete() {
		if (allowMultiSelection) {
			final BeanMapList selectedList = viewer.getSelectedBeanMapList();
			if (selectedList.size() > 0) {
				if (MessageDialog.openConfirm(EvolutionCoreUIPlugin.getShell(), getConfirmationDialogTitle(),
						getDeletionConfirmationMessage(allowMultiSelection))) {
					for (final BeanMap bean : selectedList) {
						if (isValidItem(bean)) {
							if (doOnDelete(bean)) {
								refresh();
							}
						}
					}
					setSelectedBeanMap(null);
				}
			}
		} else {
			if (getSelectedBeanMap() != null) {
				if (isValidItem(getSelectedBeanMap())) {
					if (MessageDialog.openConfirm(EvolutionCoreUIPlugin.getShell(), getConfirmationDialogTitle(),
							getDeletionConfirmationMessage(false))) {
						if (doOnDelete(getSelectedBeanMap())) {
							setSelectedBeanMap(null);
							refresh();
						}
					}
				}
			}
		}
	}

	public boolean isValidItem(BeanMap item) {
		return item.getType().equalsIgnoreCase(getType());
	}

	protected ArrayList<Action> getActions() {
		return new ArrayList<>();
	}

	protected Image getCustomColumnImage(Object element, int actualColumnIndex) {
		return null;
	}

	public void setInput(BeanMapList list) {
		viewer.setInput(list);

	}

	protected void onDoubleClick(IStructuredSelection selection) {
		if (getAllowEdit()) {
			if (editAction != null) {
				editAction.run();
			}
		}
	}

	public void setReadonly(boolean readonly) {
		this.readonly = readonly;
		bAdd.setEnabled(!readonly);
		bDelete.setEnabled(!readonly);
	}

	public boolean isReadonly() {
		return readonly;
	}

	public String getValue(Object element, int columnIndex) {
		return null;
	}

	public void setSelection(BeanMap selectedBeanMap) {
		viewer.getViewer().setSelection(
				new StructuredSelection(selectedBeanMap));
	}

	public void setSelection(BeanMapList selectedBeanMapList) {
		viewer.getViewer().setSelection(
				new StructuredSelection(selectedBeanMapList.toArray()));
	}

	/**
	 * Override and return false if the bottom toolbar buttons must be "Add" and "Remove" instead of "Create" and
	 * "Delete".
	 *
	 * @return true
	 */
	public boolean isCreateDeleteStyle() {
		return true;
	}

	public abstract String getType();

	public abstract String getAttributeList();

}
