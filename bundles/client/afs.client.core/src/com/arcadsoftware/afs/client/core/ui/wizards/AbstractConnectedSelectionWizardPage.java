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

import java.text.SimpleDateFormat;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.client.core.ui.composites.ListCompositeAdapter;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This page can be used to select an entity different from those you currently create
 *
 * @author ARCAD Software
 */
public abstract class AbstractConnectedSelectionWizardPage extends AbstractConnectedUserDefinedSelectionWizardPage {

	public AbstractConnectedSelectionWizardPage(ServerConnection connection,
			String pageName, String title, String description) {
		super(connection, pageName, title, description);
	}

	public AbstractConnectedSelectionWizardPage(ServerConnection connection,
			String pageName, String title, String description,
			boolean selectionRequired) {
		super(connection, pageName, title, description, selectionRequired);
	}

	@Override
	protected AbstractSearchListComposite createSearchComposite(Composite parent,
			MetaDataEntity entityStructure, ServerConnection connection, boolean displayInformationPanel) {

		final AbstractSearchListComposite searchComposite = new ListCompositeAdapter(innerComposite, entity, connection,
				false) {
			@Override
			protected String createSelectClause() {
				return getSelectClause();

			}

			@Override
			public boolean enableMultiSelection() {
				return allowMultiSelection();
			}

			@Override
			protected String createSearchClause() {
				return getSearchClause();
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
			public boolean defineCount() {
				return true;
			}

			@Override
			protected String createOrderClause() {
				return getOrderClause();
			}

			@Override
			protected SimpleDateFormat getDateFormatter() {
				final SimpleDateFormat sdf = AbstractConnectedSelectionWizardPage.this.getDateFormatter();
				if (sdf == null) {
					return super.getDateFormatter();
				} else {
					return sdf;
				}
			}

			@Override
			protected QueryManager createQueryManager(
					ServerConnection connection) {
				final QueryManager qm = AbstractConnectedSelectionWizardPage.this.createQueryManager(connection);
				if (qm == null) {
					return super.createQueryManager(connection);
				} else {
					return qm;
				}
			}

			@Override
			protected void onSelectionChange(IStructuredSelection selection) {
				AbstractConnectedSelectionWizardPage.this.onSelectionChanged(selection);
			}

		};
		return searchComposite;
	}

	protected QueryManager createQueryManager(ServerConnection connection) {
		return null;
	}

	protected boolean allowMultiSelection() {
		return true;
	}

	protected void onSelectionChanged(IStructuredSelection selection) {
	}
}
