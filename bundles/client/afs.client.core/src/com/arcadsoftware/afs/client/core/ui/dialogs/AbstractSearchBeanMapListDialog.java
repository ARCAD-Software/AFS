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

import org.eclipse.jface.window.Window;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractSearchBeanMapListDialog extends AbstractAFSDialog {

	private AbstractSearchListComposite listComposite;
	private MetaDataEntity entity;
	protected ServerConnection connection;
	protected DataAccessHelper helper;
	protected Object finalSelection;
	private boolean multiSelection;

	public AbstractSearchBeanMapListDialog(Shell parentShell, ServerConnection connection) {
		super(parentShell, true, true);
		initialize(connection);
	}

	protected void initialize(ServerConnection connection) {
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		entity = helper.getEntity(getType());
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gd = (GridLayout) composite.getLayout();
		gd.marginHeight = gd.marginWidth = 0;
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;
		gd.verticalSpacing = 0;
		createAreaBefore(parent);
		listComposite = createListComposite(composite, entity, connection);
		listComposite.search();
		createAreaAfter(parent);
		return composite;
	}

	public void createAreaBefore(Composite parent) {}

	public void createAreaAfter(Composite parent) {}

	public void setSelectedItem(BeanMap b) {
		listComposite.setSelectedBeanMap(b);
	}

	public void setSelectedItems(BeanMapList list) {
		listComposite.setSelectedBeanMapList(list);
	}

	public BeanMapList getInput() {
		return (BeanMapList) listComposite.getViewer().getInput();
	}

	public BeanMapList getSelectedBeanMap() {
		if (finalSelection != null) {
			return (BeanMapList) finalSelection;
		}
		return listComposite.getSelectedBeanMap();
	}

	public BeanMap getSelected() {
		return listComposite.getSelectedResult();
	}

	/**
	 * Returns an AbstractListComposite
	 *
	 * @param parent
	 *            Parent Composite
	 * @param entity
	 *            Parent Entity
	 * @param connection
	 *            Parent Server Connection
	 * @return
	 */
	protected abstract AbstractSearchListComposite createListComposite(
			Composite parent,
			MetaDataEntity entity,
			ServerConnection connection);

	/**
	 * Return the type of the BeanMap you want to manage
	 *
	 * @return beanMap type
	 */
	public abstract String getType();

	public static BeanMapList selectList(AbstractSearchBeanMapListDialog dialog) {
		dialog.setMultiSelection(true);
		if (dialog.open() == Window.OK) {
			return dialog.getSelectedBeanMap();
		}
		return null;
	}

	public static BeanMap select(AbstractSearchBeanMapListDialog dialog) {
		dialog.setMultiSelection(false);
		if (dialog.open() == Window.OK) {
			return dialog.getSelected();
		}
		return null;
	}

	/**
	 * Allows to add a new listener on the selection
	 *
	 * @param listener
	 *            a lister to the selection
	 */
	public void addBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		listComposite.addBeanMapSelectionListener(listener);
	}

	public boolean isMultiSelection() {
		return multiSelection;
	}

	public void setMultiSelection(boolean multiSelection) {
		this.multiSelection = multiSelection;
	}

	/**
	 * Allows to remove a existing listener
	 *
	 * @param listener
	 */
	public void removeBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		listComposite.removeBeanMapSelectionListener(listener);
	}

	@Override
	protected void okPressed() {
		// Get selection before Viewer of list is disposed
		if (isMultiSelection()) {
			finalSelection = listComposite.getSelectedBeanMap();
		}
		super.okPressed();
	}

}
