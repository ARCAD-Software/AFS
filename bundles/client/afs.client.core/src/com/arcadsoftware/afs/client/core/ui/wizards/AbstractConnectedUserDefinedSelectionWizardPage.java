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
package com.arcadsoftware.afs.client.core.ui.wizards;

import java.text.SimpleDateFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This page can be used to select an entity different from those you currently create
 *
 * @author ARCAD Software
 */
public abstract class AbstractConnectedUserDefinedSelectionWizardPage extends AbstractConnectedWizardPage
		implements IBeanMapSelectionListener {

	protected MetaDataEntity entity;
	protected AbstractSearchListComposite list;
	protected DataAccessHelper helper;
	protected Composite mainComposite;
	protected Composite innerComposite;

	public AbstractConnectedUserDefinedSelectionWizardPage(ServerConnection connection,
			String pageName, String title, String description) {
		this(connection, pageName, title, description, false);
	}

	public AbstractConnectedUserDefinedSelectionWizardPage(ServerConnection connection,
			String pageName, String title, String description, boolean selectionRequired) {
		super(connection, pageName, title, description);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		entity = helper.getEntity(getType());
		setPageComplete(!selectionRequired);
	}

	protected AbstractSearchListComposite createSearchComposite(Composite parent,
			MetaDataEntity entityStructure, ServerConnection connection, boolean displayInformationPanel) {
		return null;
	}

	protected void createPage(Composite parent) {
		mainComposite = parent;

		innerComposite = new Composite(parent, SWT.NONE);
		final GridLayout gl = new GridLayout(1, true);
		innerComposite.setLayout(gl);
		gl.marginBottom = gl.marginLeft = gl.marginRight = gl.marginTop = 0;
		gl.marginWidth = gl.marginHeight = 0;
		final GridData gd = new GridData(GridData.FILL_BOTH);
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		innerComposite.setLayoutData(gd);

		createAreaBefore(innerComposite);
		list = createSearchComposite(innerComposite, entity, connection, false);
		search();
		createAreaAfter(innerComposite);
		list.addBeanMapSelectionListener(this);
	}

	protected void search() {
		list.search();
	}

	@Override
	protected void createControlPage(Composite parent) {
		parent.setLayout(new GridLayout(1, true));
		createPage(parent);
	}

	public void createAreaBefore(Composite parent) {

	}

	public void createAreaAfter(Composite parent) {

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
	 * Returns a blank separated list of the attributes that will defined the column headers of the list.
	 *
	 * @return
	 */
	public abstract String getSearchClause();

	/**
	 * Returns the xml represention search criteria string
	 *
	 * @return
	 */
	public abstract String getSelectClause();

	/**
	 * Return the type of the BeanMap you want to manage
	 *
	 * @return beanMap type
	 */
	public abstract String getType();

	/**
	 * Allows to add a new listener on the selection
	 *
	 * @param listener
	 *            a lister to the selection
	 */
	public void addBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		list.addBeanMapSelectionListener(listener);
	}

	/**
	 * Allows to remove a existing listener
	 *
	 * @param listener
	 */
	public void removeBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		list.removeBeanMapSelectionListener(listener);
	}

	/**
	 * Returns the xml represention search criteria string
	 *
	 * @return
	 */
	protected String getOrderClause() {
		return "";
	}

	public BeanMap getSelectedItem() {
		return list.getSelectedResult();
	}

	@Override
	public void beanMapSelected(BeanMap selected) {
		setPageComplete(true);
	}

	@Override
	public String[] filterType() {
		return null;
	}

	protected SimpleDateFormat getDateFormatter() {
		return null;
	}
}
