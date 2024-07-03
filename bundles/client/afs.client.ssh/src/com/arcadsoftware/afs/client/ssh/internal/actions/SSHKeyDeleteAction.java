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
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBeanMapDeleteAction;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.afs.client.ssh.internal.ISSHRights;
import com.arcadsoftware.afs.client.ssh.internal.RightManager;

public class SSHKeyDeleteAction extends AbstractConnectedBeanMapDeleteAction {

	public SSHKeyDeleteAction(final ServerConnection connection) {
		super(connection);
	}

	@Override
	protected String getDeleteConfirmationMessage() {
		return Activator.resString("sshkey.action.delete.msg.confirm");
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return RightManager.getInstance().getExpectedRights(ISSHRights.SSHKEY_DELETE);
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("sshkey.action.delete.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("sshkey.action.delete.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SSHKEY_DELETE.imageDescriptor());
	}
}
