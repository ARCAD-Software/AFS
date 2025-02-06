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
package com.arcadsoftware.afs.client.ssh.ui.dialogs;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.ssh.model.SSHKey;
import com.arcadsoftware.ssh.model.SSHRoutes;

public class SSHPublicKeyDialog extends AbstractAFSDialog {
	private final SSHKey sshKey;
	private String publicKey;

	public SSHPublicKeyDialog(final ServerConnection connection, final SSHKey sshKey) {
		super(Activator.getDefault().getPluginShell(), true, true, true);
		this.sshKey = sshKey;
		try {
			publicKey = connection.getDataAccess().getWebServicesAccess()
					.get(SSHRoutes.PUBLIC_KEY.replace("{id}", String.valueOf(sshKey.getId())));
		} catch (final ServerErrorException e) {
			publicKey = e.toString();
		}
		if (publicKey == null || publicKey.isEmpty()) {
			publicKey = Activator.resString("sshkey.dialog.publickey.error", sshKey);
		}
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		final Composite dialogArea = (Composite) super.createDialogArea(parent);

		final Label description = new Label(dialogArea, SWT.NONE);
		description.setText(Activator.resString("sshkey.action.open.public.key.dialog.description"));
		description.setLayoutData(GridDataFactory.swtDefaults().grab(true, false).create());

		final Text publicKeyText = new Text(dialogArea, SWT.READ_ONLY | SWT.BORDER | SWT.MULTI | SWT.WRAP);
		publicKeyText.setText(publicKey);
		publicKeyText.setLayoutData(GridDataFactory.swtDefaults().align(SWT.FILL, SWT.FILL).grab(true, true).create());
		publicKeyText.setFocus();
		publicKeyText.selectAll();

		return dialogArea;
	}

	@Override
	public Point getSize() {
		return new Point(500, 300);
	}

	@Override
	public String getTitle() {
		return Activator.resString("sshkey.action.open.public.key.dialog.title", sshKey.getName());
	}
}
