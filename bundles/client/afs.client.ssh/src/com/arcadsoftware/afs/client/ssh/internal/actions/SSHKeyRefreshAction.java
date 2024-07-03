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

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.afs.client.ssh.internal.ui.views.SSHKeyListView;

public class SSHKeyRefreshAction extends ArcadAction {

	public SSHKeyRefreshAction() {
	}

	@Override
	protected boolean execute() {
		final SSHKeyListView view = getView();
		if (view != null) {
			view.refreshKeys();
		}
		return true;
	}

	public SSHKeyListView getView() {
		return null;
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("sshkey.action.refresh.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("sshkey.action.refresh.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
	}
}
