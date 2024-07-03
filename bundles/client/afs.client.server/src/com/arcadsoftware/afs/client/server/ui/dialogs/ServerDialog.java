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
package com.arcadsoftware.afs.client.server.ui.dialogs;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.internals.ui.composites.ServerMainComposite;
import com.arcadsoftware.afs.framework.ui.dialogs.AbstractAFSDialog;

public class ServerDialog extends AbstractAFSDialog {

	private ServerMainComposite serverComposite;
	private final Server server;

	public ServerDialog(Shell parentShell, Server server) {
		super(parentShell);
		this.server = server;
	}

	@Override
	public String getTitle() {
		return Activator.resString("server.dialog.properties.title"); //$NON-NLS-1$
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final Group serverGroup = GuiFormatTools.createGroup(composite, Activator.resString("server.group"));
		serverComposite = new ServerMainComposite(serverGroup, SWT.NONE, server);
		return composite;
	}

	@Override
	protected void okPressed() {
		if (serverComposite.checkData()) {
			server.setName(serverComposite.getServerName());
			server.setUrl(serverComposite.getUrl());
			super.okPressed();
		} else {
			final String serverMessage = serverComposite.getErrorMessage();
			if ((serverMessage != null) && !serverMessage.isEmpty()) {
				MessageDialog.openError(getShell(), Activator.resString("label.error"), serverMessage);
			}
		}
	}

	@Override
	public int getHeight() {
		return 400;
	}

	@Override
	public int getWidth() {
		return 500;
	}

	public static boolean updateServer(Server server) {
		final Server dupServer = server.clone();
		if (new ServerDialog(Activator.getInstance().getPluginShell(), dupServer).open() == 0) {
			server.setName(dupServer.getName());
			server.setUrl(dupServer.getUrl());
			return true;
		}
		return false;
	}
}
