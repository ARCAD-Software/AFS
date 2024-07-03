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
import com.arcadsoftware.afs.client.core.ui.composites.BeanMapListAdapter;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 * This page can be used to select an entity different from those you currently create.
 * <p>
 * It displays a selection list based on a BeanMapList that the user has to fill and passed to this page using the
 * {@link #getInput()} method or by directly calling the {@link #setInput(BeanMapList)} one.
 * <p>
 * You can override the method {@link #createAreaAfter(Composite)} or {@link #createAreaBefore(Composite)} methods to
 * add some graphical element after or before the selection list.
 * </p>
 * <p>
 * Get the user selection using the {@link #getSelectedItem()} or {@link #getSelectedItems()} method. Note that the
 * {@link #getSelectedItem()} return the first element of the selection if the multiselection mode has been activated
 * using the {@link #isMultiSelectionAvailable()} overrode method (true by default)
 * </p>
 *
 * @author ARCAD Software
 */
public abstract class AbstractConnectedSimpleSelectionWizardPage extends AbstractConnectedWizardPage
		implements IBeanMapSelectionListener {

	protected BeanMapListAdapter list;
	protected DataAccessHelper helper;
	protected Composite mainComposite;
	protected Composite innerComposite;

	public AbstractConnectedSimpleSelectionWizardPage(ServerConnection connection,
			String pageName, String title, String description) {
		this(connection, pageName, title, description, false);
	}

	public AbstractConnectedSimpleSelectionWizardPage(ServerConnection connection,
			String pageName, String title, String description, boolean selectionRequired) {
		super(connection, pageName, title, description);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		setPageComplete(!selectionRequired);
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
		list = new BeanMapListAdapter(innerComposite, getType(), connection) {
			@Override
			protected String createSelectClause() {
				return getSelectClause();
			}

			@Override
			public String getViewerIdentifier() {
				return null;
			}

			@Override
			protected Image getElementIcon(Object element) {
				return getIcon();
			}

			@Override
			protected SimpleDateFormat getDateFormatter() {
				final SimpleDateFormat sdf = AbstractConnectedSimpleSelectionWizardPage.this.getDateFormatter();
				if (sdf == null) {
					return super.getDateFormatter();
				} else {
					return sdf;
				}
			}

			@Override
			protected boolean enableMultiSelection() {
				return isMultiSelectionAvailable();
			}

			@Override
			protected String getDisplayedSelectClause() {
				final String result = AbstractConnectedSimpleSelectionWizardPage.this.getDisplayedSelectClause();
				if (result == null) {
					return super.getDisplayedSelectClause();
				} else {
					return result;
				}
			}

		};

		createAreaAfter(innerComposite);
		list.addBeanMapSelectionListener(this);
		final BeanMapList input = getInput();
		if (input != null) {
			setInput(input);
		}
	}

	public String getDisplayedSelectClause() {
		return null;
	}

	public void setInput(BeanMapList input) {
		list.contentChanged(input);
	}

	protected boolean isMultiSelectionAvailable() {
		return true;
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
		return null;
	}

	/**
	 * Returns the list of the attributes that will be used to defined the list
	 *
	 * @return
	 */
	public abstract String getSelectClause();

	/**
	 * Return the type of the BeanMap you want to manage.
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

	public BeanMapList getSelectedItems() {
		return list.getSelectedBeanMap();
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
