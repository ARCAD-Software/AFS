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
package com.arcadsoftware.afs.client.core.ui.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractConnectedBeanMapTableManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;

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
public abstract class AbstractConnectedTableManagerWizardPage
		extends AbstractConnectedWizardPage {

	protected AbstractConnectedBeanMapTableManager tableManager;
	protected DataAccessHelper helper;
	protected Composite mainComposite;
	protected Composite innerComposite;

	protected BeanMapList input;

	public AbstractConnectedTableManagerWizardPage(ServerConnection connection,
			String pageName, String title, String description) {
		this(connection, pageName, title, description, false);
	}

	public AbstractConnectedTableManagerWizardPage(ServerConnection connection,
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
		tableManager = new AbstractConnectedBeanMapTableManager(innerComposite, SWT.BORDER, connection,
				isEditable(), isMultiSelectionAvailable()) {

			@Override
			public void onSelection(IStructuredSelection selection) {
				AbstractConnectedTableManagerWizardPage.this.onSelection(selection);
			}

			@Override
			public String getType() {
				return AbstractConnectedTableManagerWizardPage.this.getType();
			}

			@Override
			public String getAttributeList() {
				return AbstractConnectedTableManagerWizardPage.this.getAttributeList();
			}

			@Override
			protected Image getCustomColumnImage(Object element,
					int actualColumnIndex) {
				if (actualColumnIndex == 0) {
					return getIcon();
				} else {
					return super.getCustomColumnImage(element, actualColumnIndex);
				}
			}

			@Override
			protected boolean doOnAdd() {
				return AbstractConnectedTableManagerWizardPage.this.manageAddition();
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
		createAreaAfter(innerComposite);

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
	}

	protected void setInput(BeanMapList input) {
		tableManager.setInput(input);
	}

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
		return input;
	}

	public BeanMap getSelectedItem() {
		return tableManager.getSelectedBeanMap();
	}

	public BeanMapList getSelectedItems() {
		return tableManager.getSelectedBeanMapList();
	}

	public ISearchCriteria getInitialCriteria() {
		return null;
	}

	public void onSelection(IStructuredSelection selection) {
	}

	protected boolean createDeleteStyle() {
		return false;
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

}
