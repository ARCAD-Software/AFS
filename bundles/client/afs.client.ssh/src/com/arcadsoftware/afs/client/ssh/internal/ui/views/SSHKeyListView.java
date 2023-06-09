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
package com.arcadsoftware.afs.client.ssh.internal.ui.views;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchListComposite;
import com.arcadsoftware.afs.client.core.ui.views.AbstractListView;
import com.arcadsoftware.afs.client.ssh.internal.ISSHRights;
import com.arcadsoftware.afs.client.ssh.internal.RightManager;
import com.arcadsoftware.afs.client.ssh.internal.composites.SSHKeyListComposite;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.ssh.model.SSHKey;

public class SSHKeyListView extends AbstractListView {

	public static final String ID = "com.arcadsoftware.afs.client.ssh.ui.views.SSHKeyListView"; //$NON-NLS-1$

	BeanMap client = null;
	SSHKeyListComposite listComposite;

	public SSHKeyListView() {
	}

	@Override
	protected AbstractSearchListComposite createListComposite(final Composite parent, final MetaDataEntity entity,
			final ServerConnection connection) {
		listComposite = new SSHKeyListComposite(parent, entity, connection) {

			@Override
			protected String createOrderClause() {
				return SSHKey.ENTITY;
			}

			@Override
			public SSHKeyListView getView() {
				return SSHKeyListView.this;
			}

		};
		return listComposite;
	}

	@Override
	protected void fillToolbar(final IToolBarManager manager) {
		super.fillToolbar(manager);
		final List<Action> actions = listComposite.getActions();
		for (final Action action : actions) {
			if (action != null) {
				manager.add(action);
			} else {
				manager.add(new Separator());
			}
		}
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return RightManager.getInstance().getExpectedRights(ISSHRights.SSHKEY_SEARCH);
	}

	@Override
	public String getType() {
		return SSHKey.ENTITY;
	}

	@Override
	protected void readStructureError() {

	}

	public void refreshKeys() {
		if (isAllowedToSearch()) {
			listComposite.search();
		}
	}

}
