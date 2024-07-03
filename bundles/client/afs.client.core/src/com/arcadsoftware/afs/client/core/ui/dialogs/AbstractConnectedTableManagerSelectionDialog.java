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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractConnectedBeanMapTableManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;

public abstract class AbstractConnectedTableManagerSelectionDialog extends AbstractAFSDialog {

	protected AbstractConnectedBeanMapTableManager tableManager;
	protected BeanMapList input;
	protected ServerConnection connection;
	protected DataAccessHelper helper;
	private BeanMapList selection;

	// private boolean multiSelection = false;

	public AbstractConnectedTableManagerSelectionDialog(Shell parentShell, ServerConnection connection) {
		super(parentShell, true, true);
		initialize(connection);
	}

	protected void initialize(ServerConnection connection) {
		this.connection = connection;
		helper = new DataAccessHelper(connection);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gd = (GridLayout) composite.getLayout();
		gd.marginHeight = gd.marginWidth = 0;
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;
		gd.verticalSpacing = 0;
		createAreaBefore(parent);
		tableManager = new AbstractConnectedBeanMapTableManager(composite, SWT.BORDER, connection,
				isEditable(), isMultiSelectionAvailable()) {
			
			@Override
			public String getType() {
				return AbstractConnectedTableManagerSelectionDialog.this.getType();
			}

			@Override
			public String getAttributeList() {
				return AbstractConnectedTableManagerSelectionDialog.this.getAttributeList();
			}

			@Override
			protected Image getCustomColumnImage(Object element,
					int actualColumnIndex) {
				if (actualColumnIndex == 0) {
					return getIcon();
				}
				return super.getCustomColumnImage(element, actualColumnIndex);
			}

			@Override
			protected boolean doOnAdd() {
				return AbstractConnectedTableManagerSelectionDialog.this.manageAddition();
			}

			@Override
			protected boolean doOnDelete(BeanMap deleted) {
				return manageDeletion(deleted);
			}

			@Override
			protected boolean doOnUpdate(BeanMap updated) {
				return manageUpdate(updated);
			}

			@Override
			protected boolean getAllowDelete() {
				return allowDelete();
			}

			@Override
			protected boolean getAllowEdit() {
				return allowEdit();
			}

			@Override
			protected boolean getAllowAdd() {
				return allowAdd();
			}

			@Override
			public boolean isCreateDeleteStyle() {
				return createDeleteStyle();
			}
		};
		final ISearchCriteria criteria = getInitialCriteria();
		if (criteria != null) {
			input = helper.getList(getType(), criteria);
		} else {
			input = helper.getList(getType());
		}
		final BeanMapList input = getInput();
		if (input != null) {
			tableManager.setInput(input);
		}
		createAreaAfter(parent);
		return composite;
	}

	public void createAreaBefore(Composite parent) {}

	public void createAreaAfter(Composite parent) {}

	protected boolean manageAddition() {
		BeanMap added = new BeanMap(getType());
		boolean result = add(added);
		if (result) {
			result = helper.create(added);
			if (result) {
				added = helper.read(getType(), added.getId(), getAttributeList());
				final BeanMapList input = getInput();
				input.add(added);
			}
		}
		return result;
	}

	protected boolean manageUpdate(BeanMap edited) {
		boolean result = edit(edited);
		if (result) {
			result = helper.update(edited);
		}
		return result;
	}

	protected boolean manageDeletion(BeanMap deleted) {
		boolean result = delete(deleted);
		if (result) {
			result = helper.delete(deleted);
		}
		return result;
	}

	protected boolean isEditable() {
		return true;
	}

	protected boolean isMultiSelectionAvailable() {
		return true;
	}

	protected boolean allowDelete() {
		return true;
	}

	protected boolean allowEdit() {
		return true;
	}

	protected boolean allowAdd() {
		return true;
	}

	/**
	 * Override this method to return the Image icon of the element of the list.
	 *
	 * @return The icon of the displayed element
	 */
	protected Image getIcon() {
		return null;
	}

	/**
	 * Returns the beanMap list taht represents the input of the list
	 *
	 * @return The beanMapList that will be displayed into the selection list
	 */
	protected BeanMapList getInput() {
		return input;
	}

	public BeanMap getSelectedItem() {
		if ((selection != null) && (selection.size() > 0)) {
			return selection.get(0);
		}
		return null;
	}

	public BeanMapList getSelectedItems() {
		return selection;
	}

	public ISearchCriteria getInitialCriteria() {
		return null;
	}

	public void setSelectedItem(BeanMap b) {
		tableManager.setSelection(b);
	}

	public void setSelectedItems(BeanMapList list) {
		tableManager.setSelection(list);
	}

	@Override
	protected void okPressed() {
		selection = tableManager.getSelectedBeanMapList();
		super.okPressed();
	}

	/**
	 * Return the type of the BeanMap you want to manage.
	 *
	 * @return beanMap type
	 */
	public abstract String getType();

	/**
	 * Return the list of the attributes that must be displayed on the list
	 *
	 * @return blank separated list of attributes
	 */
	public abstract String getAttributeList();

	public abstract boolean add(BeanMap added);

	public abstract boolean edit(BeanMap edited);

	public abstract boolean delete(BeanMap deleted);

	public static BeanMapList selectList(AbstractConnectedTableManagerSelectionDialog dialog) {
		if (dialog.open() == Window.OK) {
			return dialog.getSelectedItems();
		}
		return null;
	}

	public static BeanMap select(AbstractConnectedTableManagerSelectionDialog dialog) {
		if (dialog.open() == Window.OK) {
			return dialog.getSelectedItem();
		}
		return null;
	}

	protected boolean createDeleteStyle() {
		return false;
	}
}
