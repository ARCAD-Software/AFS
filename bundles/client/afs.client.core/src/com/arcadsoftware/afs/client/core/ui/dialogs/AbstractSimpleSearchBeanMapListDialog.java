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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.client.core.ui.composites.ListCompositeAdapter;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * Subclass this class to create a selection dialog on a a BeanMapList
 *
 * @author ARCAD Software
 */
public abstract class AbstractSimpleSearchBeanMapListDialog extends
		AbstractSearchBeanMapListDialog {

	protected boolean showAll = true;

	/**
	 * Create the dialog.<br/>
	 * If the showAll parameter is set to true, then the result list will be loaded in one pass.
	 *
	 * @param parentShell
	 * @param connection
	 * @param showAll
	 */
	public AbstractSimpleSearchBeanMapListDialog(Shell parentShell,
			ServerConnection connection, boolean showAll) {
		super(parentShell, connection);
		this.showAll = showAll;
	}

	@Override
	protected AbstractSearchListComposite createListComposite(Composite parent,
			MetaDataEntity entity, ServerConnection connection) {
		final ListCompositeAdapter adapter = new ListCompositeAdapter(parent, entity, connection, !showAll) {

			@Override
			protected String createSelectClause() {
				return getSelectClause();

			}

			@Override
			protected String createSearchClause() {
				return getSearchClause();
			}

			@Override
			protected String createOrderClause() {
				return getOrderClause();
			}

			@Override
			public String getViewerIdentifier() {
				return null;
			}

			@Override
			protected Image getElementIcon(Object element) {
				Image icon = getIcon(element);
				if (icon == null) {
					icon = getIcon();
				}
				return icon;
			}

			@Override
			public boolean defineCount() {
				return showAll;
			}

			@Override
			public boolean enableMultiSelection() {
				return isMultiSelection();
			}

			@Override
			protected void doOnDoubleClickEvent(IStructuredSelection selection) {
				doOnDbClick(selection);
			}

			@Override
			protected String getColumnHeader(String attribute) {
				final String header = getUserDefineColumnHeader(attribute);
				if (header == null) {
					return super.getColumnHeader(attribute);
				}
				return header;
			}

			@Override
			protected String getDisplayedSelectClause() {
				final String result = AbstractSimpleSearchBeanMapListDialog.this.getDisplayedSelectClause();
				if (result == null) {
					return super.getDisplayedSelectClause();
				}
				return result;
			}

			@Override
			protected String getKeySearchAttribute() {
				return getSearchKeyAttribute();
			}
		};
		return adapter;
	}

	/**
	 * Returns the select clause
	 *
	 * @return
	 */
	public String getSearchKeyAttribute() {
		return null;
	}

	/**
	 * Override this method to return the Image icon of the element of the list.
	 *
	 * @return
	 */
	protected Image getIcon() {
		return null;
	}

	/**
	 * Override this method to return the Image icon for a specific element of the list.
	 *
	 * @param element
	 * @return
	 */
	protected Image getIcon(Object element) {
		return null;
	}

	/**
	 * Returns a blank separated list of the attributes that will defined the column headers of the list.
	 *
	 * @return
	 */
	public abstract String getSearchClause();

	/**
	 * Returns the select clause
	 *
	 * @return
	 */
	public abstract String getSelectClause();

	/**
	 * Returns the order clause
	 *
	 * @return
	 */
	protected String getOrderClause() {
		return ""; //$NON-NLS-1$
	}

	/**
	 * Returns the order clause
	 *
	 * @return
	 */
	protected String getDisplayedSelectClause() {
		return null;
	}

	protected void doOnDbClick(IStructuredSelection selection) {
		okPressed();
	}

	protected String getUserDefineColumnHeader(String attribute) {
		return null;
	}
}
