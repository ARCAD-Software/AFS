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

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.server.IServerVersionProvider;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.framework.ui.dialogs.AbstractAFSDialog;

public class ServerVersionDialog extends AbstractAFSDialog {

	private final IServerVersionProvider versionProvider;

	protected ServerVersionDialog(Shell parentShell, IServerVersionProvider provider) {
		super(parentShell, true);
		versionProvider = provider;
	}

	@Override
	protected Image getDialogImage() {
		if (versionProvider == null) {
			return super.getDialogImage();
		}
		final Image di = versionProvider.getDialogImage();
		if (di == null) {
			return super.getDialogImage();
		}
		return di;
	}

	@Override
	public int getHeight() {
		return 100;
	}

	@Override
	public int getWidth() {
		return 300;
	}

	@Override
	public String getTitle() {
		if (versionProvider != null) {
			return versionProvider.getDialogTitle();
		}
		return "Server Version";
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite c = (Composite) super.createDialogArea(parent);
		c.setLayout(new GridLayout(3, false));
		String version = "N/A";
		String description = "AFS Server";
		if (versionProvider != null) {
			version = versionProvider.getServerVersion();
			description = versionProvider.getServerDescription();
		}
		final Label versionLabel = GuiFormatTools.createLabelledLabel(c, description);
		versionLabel.setText(version);
		return c;
	}

	public static void show(IServerVersionProvider provider) {
		new ServerVersionDialog(Activator.getInstance().getPluginShell(), provider).open();
	}
}
