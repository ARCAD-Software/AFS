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
package com.arcadsoftware.afs.client.server.internals.ui.dialogs;

import java.net.PasswordAuthentication;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

public class UserAuthenticationDialog extends Dialog {

	private Text usernameField;
	private Text passwordField;
	private final String scheme;
	private final String host;
	private final String message;
	private PasswordAuthentication userAuthentication;
	
	public UserAuthenticationDialog(Shell parentShell, String scheme, String host, String message) {
		super(parentShell);
		this.scheme = scheme;
		this.host = host;
		this.message = message;
		setBlockOnOpen(true);
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(host);
	}

	@Override
	public void create() {
		super.create();
		usernameField.selectAll();
		usernameField.setFocus();
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite main = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 3;
		main.setLayout(layout);
		main.setLayoutData(new GridData(GridData.FILL_BOTH));
		Label label = new Label(main, SWT.WRAP);
		label.setText(host + " [" + scheme + "]:\n\n" + message); //$NON-NLS-1$ //$NON-NLS-2$
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.horizontalSpan = 3;
		label.setLayoutData(data);
		createUsernameFields(main);
		createPasswordFields(main);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(main, "org.eclipse.update.ui.UserValidationDialog"); //$NON-NLS-1$
		return main;
	}

	protected void createPasswordFields(Composite parent) {
		new Label(parent, SWT.NONE).setText("Password:");
		passwordField = new Text(parent, SWT.BORDER | SWT.PASSWORD);
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.ENTRY_FIELD_WIDTH);
		passwordField.setLayoutData(data);
		new Label(parent, SWT.NONE); //spacer
	}

	protected void createUsernameFields(Composite parent) {
		new Label(parent, SWT.NONE).setText("Login:");
		usernameField = new Text(parent, SWT.BORDER);
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.ENTRY_FIELD_WIDTH);
		usernameField.setLayoutData(data);
		new Label(parent, SWT.NONE); //spacer
	}

	/**
	 * Returns the UserAuthentication entered by the user, or null if the user
	 * canceled.
	 * @return the authentication information
	 */
	public PasswordAuthentication getAuthentication() {
		return userAuthentication;
	}

	@Override
	protected void okPressed() {
		userAuthentication = new PasswordAuthentication(usernameField.getText(), passwordField.getText().toCharArray());
		super.okPressed();
	}

}
