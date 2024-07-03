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
package com.arcadsoftware.afs.client.server.internals.ui.dialogs;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.security.TrustStoreProviderExtensionManager;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.afs.client.server.ISRVIconConsts;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.afs.framework.ui.images.ImageManager;

public class ConnectionDialog extends AbstractAFSDialog {

	private Text loginText;
	private Text passwordText;
	private Button savePassword;
	private Label messageLabel;
	private boolean isTransparent;
	private int topPosition;
	private int leftPosition;
	private int rightPosition;
	private int bottomPosition;
	private String imagePath;
	private Composite mainComposite;
	private final Server server;
	private final boolean manageUser;

	public ConnectionDialog(Shell parentShell, Server server, boolean manageUser) {
		super(parentShell, false, true);
		setShellStyle(SWT.NO_TRIM | SWT.APPLICATION_MODAL);
		this.server = server;
		this.manageUser = manageUser;
	}

	public ConnectionDialog(Shell parentShell, Server server) {
		this(parentShell, server, true);
	}

	@Override
	protected Control createButtonBar(Composite parent) {
		return null;
	}

	@Override
	public Point getSize() {
		return new Point(521, 390);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite c = (Composite) super.createDialogArea(parent);
		final GridLayout gl = (GridLayout) c.getLayout();
		gl.marginBottom = gl.marginHeight = gl.marginLeft = gl.marginRight = gl.marginTop = gl.marginWidth = 0;
		createContent(c);
		return c;
	}

	private void initSettings() {
		final IConfigurationElement[] elements = Platform.getExtensionRegistry()
				.getConfigurationElementsFor("com.arcadsoftware.afs.client.branding.splashscreen"); //$NON-NLS-1$
		IConfigurationElement selectElement = null;
		if ((elements != null) && (elements.length > 0)) {
			selectElement = elements[0];
		}
		if (selectElement != null) {
			String bundleId = selectElement.getAttribute("bundleid"); //$NON-NLS-1$
			final String path = selectElement.getAttribute("path"); //$NON-NLS-1$
			if ((bundleId == null) || (bundleId.length() == 0)) {
				bundleId = Activator.getInstance().getBundle().getSymbolicName();
			}
			imagePath = bundleId + ':' + path;
			topPosition = Integer.valueOf(selectElement.getAttribute("top")); //$NON-NLS-1$
			leftPosition = Integer.valueOf(selectElement.getAttribute("left")); //$NON-NLS-1$
			rightPosition = Integer.valueOf(selectElement.getAttribute("right")); //$NON-NLS-1$
			bottomPosition = Integer.valueOf(selectElement.getAttribute("bottom")); //$NON-NLS-1$
			isTransparent = Boolean.valueOf(selectElement.getAttribute("transparent")); //$NON-NLS-1$
		} else {
			isTransparent = true;
			imagePath = ISRVIconConsts.SPLASH;
			topPosition = 160;
			leftPosition = 150;
			rightPosition = -20;
			bottomPosition = -70;
		}
	}

	private void createContent(Composite parent) {
		initSettings();
		final Composite imageComposite = new Composite(parent, SWT.BORDER);
		if (isTransparent) {
			imageComposite.setBackgroundMode(SWT.INHERIT_FORCE);
		}
		final FormLayout fl = new FormLayout();
		fl.marginBottom = fl.marginHeight = fl.marginLeft = fl.marginRight = fl.marginTop = 0;
		imageComposite.setLayout(new FormLayout());
		@SuppressWarnings("deprecation")
		final Image arcadImage = ImageManager.getInstance().getImage(imagePath);
		imageComposite.setBackgroundImage(arcadImage);
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		imageComposite.setLayoutData(gridData);
		mainComposite = new Composite(imageComposite, SWT.NONE);
		mainComposite.setLayout(new FormLayout());
		if (!isTransparent) {
			mainComposite.setBackground(getShell().getDisplay().getSystemColor(SWT.COLOR_WHITE));
		} else {
			mainComposite.setBackgroundMode(SWT.INHERIT_FORCE);
		}
		FormData data = new FormData();
		mainComposite.setLayoutData(data);
		data.top = new FormAttachment(0, topPosition);
		data.left = new FormAttachment(0, leftPosition);
		data.right = new FormAttachment(100, rightPosition);
		data.bottom = new FormAttachment(100, bottomPosition);
		final SelectionListener enterPressedListener = new SelectionAdapter() {
			@Override
			public void widgetDefaultSelected(SelectionEvent paramSelectionEvent) {
				doConnect();
			}
		};
		final Label loginLabel = new Label(mainComposite, SWT.NONE);
		if (!isTransparent) {
			loginLabel.setBackground(mainComposite.getBackground());
		}
		loginLabel.setText(Activator.resString("server.dialog.connection.login.label")); //$NON-NLS-1$
		data = new FormData();
		loginLabel.setLayoutData(data);
		data.top = new FormAttachment(0, 0);
		data.left = new FormAttachment(0, 0);
		data.width = 75;
		loginText = new Text(mainComposite, SWT.BORDER);
		loginText.setBackground(mainComposite.getBackground());
		data = new FormData();
		loginText.setLayoutData(data);
		data.top = new FormAttachment(0, 0);
		data.left = new FormAttachment(loginLabel, 3, SWT.RIGHT);
		data.right = new FormAttachment(100, 0);
		loginText.addSelectionListener(enterPressedListener);
		final Label passwordLabel = new Label(mainComposite, SWT.NONE);
		if (!isTransparent) {
			passwordLabel.setBackground(mainComposite.getBackground());
		}
		passwordLabel.setText(Activator.resString("server.dialog.connection.password.label")); //$NON-NLS-1$
		data = new FormData();
		passwordLabel.setLayoutData(data);
		data.top = new FormAttachment(loginLabel, 7);
		data.left = new FormAttachment(0, 0);
		data.width = 75;
		passwordText = new Text(mainComposite, SWT.BORDER);
		passwordText.setBackground(mainComposite.getBackground());
		data = new FormData();
		passwordText.setLayoutData(data);
		data.top = new FormAttachment(loginLabel, 7);
		data.left = new FormAttachment(passwordLabel, 3, SWT.RIGHT);
		data.right = new FormAttachment(100, 0);
		passwordText.setEchoChar('*');
		passwordText.addSelectionListener(enterPressedListener);
		savePassword = new Button(mainComposite, SWT.CHECK);
		if (!isTransparent) {
			savePassword.setBackground(mainComposite.getBackground());
		}
		data = new FormData();
		savePassword.setLayoutData(data);
		data.top = new FormAttachment(passwordText, 7);
		data.left = new FormAttachment(passwordText, 0, SWT.LEFT);
		data.right = new FormAttachment(100, 0);
		savePassword.setText(Activator.resString("server.dialog.connection.rememberPassword.label")); //$NON-NLS-1$
		messageLabel = new Label(mainComposite, SWT.WRAP);
		if (!isTransparent) {
			messageLabel.setBackground(mainComposite.getBackground());
		}
		data = new FormData();
		messageLabel.setLayoutData(data);
		data.top = new FormAttachment(savePassword, 5);
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(100, 0);
		data.height = 40;
		messageLabel.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				if (messageLabel.getText().length() > 100) {
					MessageDialog.openInformation(getParentShell(), getTitle(), messageLabel.getText());
				}
			}
		});
		final Composite buttonBar = new Composite(mainComposite, SWT.NONE);
		if (!isTransparent) {
			buttonBar.setBackground(mainComposite.getBackground());
		}
		data = new FormData();
		buttonBar.setLayoutData(data);
		data.top = new FormAttachment(messageLabel, 5);
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(100, 0);
		buttonBar.setLayout(new FormLayout());
		final Button cancelButton = new Button(buttonBar, SWT.PUSH);
		cancelButton.setText(Activator.resString("server.connection.dialog.button.cancel")); //$NON-NLS-1$
		cancelButton.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						cancelPressed();
					}
				});
		data = new FormData();
		cancelButton.setLayoutData(data);
		data.right = new FormAttachment(100, 0);
		data.width = 100;
		final Button connectButton = new Button(buttonBar, SWT.PUSH);
		connectButton.setText(Activator.resString("server.connection.dialog.button.connect")); //$NON-NLS-1$
		connectButton.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						doConnect();
					}
				});
		data = new FormData();
		connectButton.setLayoutData(data);
		data.right = new FormAttachment(cancelButton, -5, SWT.LEFT);
		data.width = 100;
		loginText.setText(server.getLastLogin());
		passwordText.setText(server.getLastPassword());
		savePassword.setSelection(server.isRememberPassword());
		buttonBar.setTabList(new Control[] { connectButton, cancelButton });
	}

	public void doConnect() {
		if (connection()) {
			okPressed();
		}
	}

	private boolean connection() {
		server.setLastLogin(loginText.getText());
		server.setLastPassword(passwordText.getText());
		server.setRememberPassword(savePassword.getSelection());
		final ServerConnection serverConnection = new ServerConnection(server);
		serverConnection.setMessageManager(UserMessageManager.getInstance());
		serverConnection.setTrustStoreprovider(TrustStoreProviderExtensionManager.getTrustStoreProvider());
		if (!serverConnection.connect(manageUser, true)) {
			messageLabel.setForeground(getShell().getDisplay().getSystemColor(SWT.COLOR_RED));
			if (serverConnection.getErrorMessage() != null) {
				String msg = serverConnection.getErrorMessage().getTextLevel1();
				msg = Activator.resString(msg);
				messageLabel.setText(msg);
			}
			return false;
		}
		return doAfterConnection(serverConnection);
	}

	private boolean doAfterConnection(ServerConnection serverConnection) {
		// Operation to proceed just After Connection.
		// This operation can cancel a correct connection.
		if ((serverConnection.getUser() != null) && //
				serverConnection.getUser().isChangePWD() && //
				serverConnection.getUser().isCanChangePWD()) {
			return ChangePasswordDialog.open(getShell(), serverConnection);
		}
		return true;
	}

	@Override
	public String getTitle() {
		return ""; //$NON-NLS-1$
	}

	public static boolean connect(Server server, boolean manageUser) {
		final Server s = server.duplicate();
		final ConnectionDialog dialog = new ConnectionDialog(Activator.getInstance().getPluginShell(), s, manageUser);
		if (dialog.open() == Window.OK) {
			server.setLastLogin(s.getLastLogin());
			server.setLastPassword(s.getLastPassword());
			server.setRememberPassword(s.isRememberPassword());
			return true;
		}
		return false;
	}
}
