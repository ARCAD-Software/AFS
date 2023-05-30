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

import java.util.HashMap;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.dialogs.DialogConstantProvider;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.rest.ServerErrorException;

/**
 * This dialog manage the modification of the current user password, according to the server constraints.
 * 
 * @author ARCAD Software
 */
public class ChangePasswordDialog extends AbstractAFSDialog {

	/**
	 * Open the Change password dialog and return true if the user has effectively changed its password on the server connection.
	 * 
	 * <p>
	 * The <code>serverConnection</code> object is updated with the new password.
	 * 
	 * @param parentShell
	 * @param serverConnection
	 * @return
	 */
	public static boolean open(Shell parentShell, ServerConnection serverConnection) {
		return new ChangePasswordDialog(parentShell, serverConnection).open() == Window.OK;
	}

	private final ServerConnection server;
	private Label message;
	private Text password;
	private Text repassword;
	private Button okButton;

	public ChangePasswordDialog(Shell parentShell, ServerConnection server) {
		super(parentShell, false, true);
		setShellStyle(SWT.APPLICATION_MODAL | SWT.BORDER);
		this.server = server;
	}

	@Override
	public Point getSize() {
		return new Point(450, 300);
	}

	@Override
	public String getTitle() {
		return Activator.resString("server.dialog.changePassword.title"); //$NON-NLS-1$
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		parent = (Composite) super.createDialogArea(parent);
		((GridLayout) parent.getLayout()).numColumns = 2;
		// introducing message.
		Label label = new Label(parent, SWT.WRAP);
		label.setText(Activator.resString("server.dialog.changePassword.text")); //$NON-NLS-1$
		label.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, 2, 1));
		// An error Message.
		message = new Label(parent, SWT.WRAP);
		message.setForeground(getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
		message.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, 2, 3));
		message.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				if (message.getText().length() > 50) {
					MessageDialog.openInformation(getParentShell(), getTitle(), message.getText());
				}
			}
		});
		label = new Label(parent, SWT.NONE);
		label.setText(Activator.resString("server.dialog.changePassword.newPassword")); //$NON-NLS-1$
		label.setLayoutData(new GridData());
		password = new Text(parent, SWT.PASSWORD | SWT.BORDER);
		password.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
		label = new Label(parent, SWT.NONE);
		label.setText(Activator.resString("server.dialog.changePassword.retypeNePassword")); //$NON-NLS-1$
		label.setLayoutData(new GridData());
		repassword = new Text(parent, SWT.PASSWORD | SWT.BORDER);
		repassword.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false));
		ModifyListener listener = new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				if ((password.getText().length() == 0) || (repassword.getText().length() == 0)) {
					message.setText(Activator.resString("server.dialog.changePassword.empty")); //$NON-NLS-1$
					okButton.setEnabled(false);
				} else if (password.getText().equals(repassword.getText())) {
					message.setText(""); //$NON-NLS-1$
					okButton.setEnabled(true);
				} else {
					okButton.setEnabled(false);
					message.setText(Activator.resString("server.dialog.changePassword.differentPasswords")); //$NON-NLS-1$
				}
			}
		};
		password.addModifyListener(listener);
		repassword.addModifyListener(listener);
		return parent;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		// create OK and Cancel buttons by default
		okButton = createButton(parent, IDialogConstants.CLIENT_ID, DialogConstantProvider.getInstance().OK_LABEL, true);
		okButton.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						// Call the /currentuser/{password} web-service.
						final String newPassword = password.getText();
						boolean cxn;
						if (server.getServer().getUrl().toLowerCase().startsWith("https")) { //$NON-NLS-1$
							cxn = server.connectWithCertificats(server.getServer().getLastLogin(), server.getServer().getLastPassword(), false, false);
						} else {
							cxn = server.connectWithoutCertificats(server.getServer().getLastLogin(), server.getServer().getLastPassword(), false, false);
						}
						if (!cxn) {
							message.setText(Activator.resString("server.dialog.changePassword.connectionerror")); //$NON-NLS-1$
						} else {
							final HashMap<String, Object> parameters = new HashMap<String, Object>(2);
							parameters.put("oldpassword", server.getServer().getLastPassword()); //$NON-NLS-1$
							parameters.put("newpassword", newPassword); //$NON-NLS-1$
							try {
								server.getDataAccess().getWebServicesAccess().post("/currentuser", parameters); //$NON-NLS-1$
								server.setPassword(newPassword);
								server.getServer().setLastPassword(newPassword);
								okPressed();
							} catch (ServerErrorException ex) {
								Activator.getDefault().error(ex.getErrorMessage(), ex);
								message.setText(ex.getDescription());
								message.getParent().layout();
							}
						}
					}
				});
		okButton.setEnabled(false);
		createButton(parent, IDialogConstants.CANCEL_ID, DialogConstantProvider.getInstance().CANCEL_LABEL, false);
	}
}
