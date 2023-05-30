/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.afs.client.ssh.internal.composites;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.afs.client.ssh.internal.actions.SSHKeyAddAction;
import com.arcadsoftware.afs.client.ssh.internal.actions.SSHKeyDeleteAction;
import com.arcadsoftware.afs.client.ssh.internal.actions.SSHKeyEditAction;
import com.arcadsoftware.afs.client.ssh.internal.actions.SSHKeyImportAction;
import com.arcadsoftware.afs.client.ssh.internal.actions.SSHKeyOpenPublicKeyAction;
import com.arcadsoftware.afs.client.ssh.internal.actions.SSHKeyRefreshAction;
import com.arcadsoftware.afs.client.ssh.internal.ui.views.SSHKeyListView;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.ssh.model.SSHKey;

public class SSHKeyListComposite extends AbstractSearchListComposite {

	SSHKeyAddAction addAction;
	SSHKeyDeleteAction deleteAction;
	SSHKeyEditAction editAction;
	SSHKeyImportAction importAction;
	SSHKeyOpenPublicKeyAction openPublicKeyAction;
	SSHKeyRefreshAction refreshAction;

	public SSHKeyListComposite(final Composite parent, final MetaDataEntity entityStructure,
			final ServerConnection connection) {
		super(parent, entityStructure, connection);
	}

	@Override
	protected String createOrderClause() {
		return null;
	}

	@Override
	protected String createSearchClause() {
		return null;
	}

	@Override
	protected String createSelectClause() {
		return SSHKey.NAME + " " + SSHKey.TYPE + " " + SSHKey.LENGTH + " " + SSHKey.FINGERPRINT;
	}

	@Override
	protected void doOnDoubleClickEvent(final IStructuredSelection selection) {
		editAction.run();
	}

	@Override
	public List<Action> getActions() {
		final List<Action> result = new ArrayList<>();

		addAction = new SSHKeyAddAction(getConnection()) {
			@Override
			protected void doAfterRun() {
				refreshAction.run();
			}
		};

		editAction = new SSHKeyEditAction(getConnection()) {
			@Override
			protected BeanMapList getBeanMapListToManage() {
				return getSelectedBeanMap();
			}
		};

		openPublicKeyAction = new SSHKeyOpenPublicKeyAction(getConnection()) {
			@Override
			protected BeanMapList getBeanMapListToManage() {
				return getSelectedBeanMap();
			}
		};

		refreshAction = new SSHKeyRefreshAction() {
			@Override
			public SSHKeyListView getView() {
				return SSHKeyListComposite.this.getView();
			}
		};

		deleteAction = new SSHKeyDeleteAction(getConnection()) {
			@Override
			protected void doAfterRun() {
				refreshAction.run();
			}

			@Override
			protected BeanMapList getBeanMapListToManage() {
				return getSelectedBeanMap();
			}
		};

		importAction = new SSHKeyImportAction(getConnection()) {
			@Override
			protected void doAfterRun() {
				refreshAction.run();
			}
		};

		result.add(addAction);
		result.add(importAction);
		result.add(null);
		result.add(editAction);
		result.add(openPublicKeyAction);
		result.add(null);
		result.add(deleteAction);
		result.add(null);
		result.add(refreshAction);

		return result;
	}

	public BeanMap getClient() {
		return null;
	}

	@Override
	protected Image getElementIcon(final Object element) {
		return AFSIcon.SSHKEY.image();
	}

	@Override
	protected Bundle getParentBundle() {
		return Activator.getDefault().getBundle();
	}

	@Override
	protected UserMessage getSearchErrorMessage() {
		return null;
	}

	public SSHKeyListView getView() {
		return null;
	}

	@Override
	public String getViewerIdentifier() {
		return null;
	}

}
