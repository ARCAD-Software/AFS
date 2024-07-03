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
package com.arcadsoftware.afs.framework.ui.messages;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.afs.framework.ui.internal.Activator;

public class UserMessageDialog extends AbstractAFSDialog {

	private final UserMessage um;
	private Label messageLabel;

	protected UserMessageDialog(Shell parentShell, UserMessage um) {
		super(parentShell);
		this.um = um;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gl = new GridLayout(2, false);
		gl.marginHeight = gl.marginWidth = 5;
		gl.marginLeft = gl.marginTop = gl.marginRight = gl.marginBottom = 0;
		composite.setLayout(gl);
		final Label image = new Label(composite, SWT.NONE);
		final GridData gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
		gd.heightHint = 32;
		gd.widthHint = 32;
		image.setLayoutData(gd);
		image.setBackgroundImage(AFSIcon.ARCAD.image());
		messageLabel = new Label(composite, SWT.WRAP);
		messageLabel.setLayoutData(new GridData(GridData.FILL_BOTH));
		messageLabel.setText(um.toString());
		return composite;
	}

	@Override
	public int getHeight() {
		return 200;
	}

	@Override
	public int getWidth() {
		return 400;
	}

	@Override
	public String getTitle() {
		return "Message";
	}

	public static void show(UserMessage um) {
		new UserMessageDialog(Activator.getDefault().getPluginShell(), um).open();
	}

}
