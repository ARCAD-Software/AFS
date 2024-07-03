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
package com.arcadsoftware.afs.client.ssh.internal.actions;

import java.util.List;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBeanMapAction;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.afs.client.ssh.internal.ISSHRights;
import com.arcadsoftware.afs.client.ssh.internal.RightManager;
import com.arcadsoftware.afs.client.ssh.ui.dialogs.SSHPublicKeyDialog;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.ssh.model.SSHKey;

public class SSHKeyOpenPublicKeyAction extends AbstractConnectedBeanMapAction {

	public SSHKeyOpenPublicKeyAction(final ServerConnection connection) {
		super(connection);
	}

	@Override
	protected boolean execute() {
		final BeanMap b = getBeanMapToManage();
		if (b == null) {
			final BeanMapList list = getBeanMapListToManage();
			if (list != null) {
				for (final BeanMap bean : list) {
					openPublicKey(bean);
				}
			}
		} else {
			openPublicKey(b);
		}
		return true;
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return RightManager.getInstance().getExpectedRights(ISSHRights.SSHKEY_EDIT);
	}

	public void openPublicKey(final BeanMap key) {
		new SSHPublicKeyDialog(connection, new SSHKey(key)).open();
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("sshkey.action.open.public.key.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("sshkey.action.open.public.key.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SSHKEY_PUBLIC.imageDescriptor());
	}
}
