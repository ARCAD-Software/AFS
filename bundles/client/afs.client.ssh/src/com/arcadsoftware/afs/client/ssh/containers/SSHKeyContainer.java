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
package com.arcadsoftware.afs.client.ssh.containers;

import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.ui.containers.AbstractConnectedContainerProvider;
import com.arcadsoftware.afs.client.core.ui.containers.ViewManagedContainer;
import com.arcadsoftware.afs.client.core.ui.views.AbstractConnectedView;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.afs.client.ssh.internal.ui.views.SSHKeyListView;

public class SSHKeyContainer extends ViewManagedContainer {

	public SSHKeyContainer() {
		super(null);

	}

	public SSHKeyContainer(final AbstractConnectedContainerProvider parent) {
		super(parent);

	}

	@Override
	protected void doOnView(final AbstractConnectedView view) {
		if (view instanceof SSHKeyListView) {
			((SSHKeyListView) view).refreshKeys();
		}
	}

	@Override
	public Image getImage() {
		return AFSIcon.SSHKEY.image();
	}

	@Override
	public String getLabel() {
		return Activator.resString("navigationView.node.sshkeys"); //$NON-NLS-1$
	}

	@Override
	public String getManagementViewId() {
		return SSHKeyListView.ID;
	}

	@Override
	public String getUniqueKey() {
		return getParent().getUniqueKey().concat("/SSHKEY"); //$NON-NLS-1$
	}

}
