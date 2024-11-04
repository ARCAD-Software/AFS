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

import java.io.File;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.afs.client.server.connection.SSLKeyStoreManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.framework.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.crypt.CertificateInformation;

public class SSLSettingsDialog extends AbstractAFSDialog {

	public static void update(ITrustStoreProvider provider) {
		final SSLSettingsDialog dialog = new SSLSettingsDialog(Activator.getInstance().getPluginShell(), provider);
		dialog.open();
	}

	private Text trustStorePathText;
	private Text keyStorePathText;
	private Text trustStorePasswordText;
	private Text keyStorePasswordText;
	private final ITrustStoreProvider provider;
	private Button importCertificateButton;

	protected SSLSettingsDialog(Shell parentShell, ITrustStoreProvider provider) {
		super(parentShell);
		this.provider = provider;
	}

	@Override
	public int getHeight() {
		return 350;
	}

	@Override
	public int getWidth() {
		return 500;
	}

	@Override
	public String getTitle() {
		return Activator.resString("sslsettings.dialog.title"); //$NON-NLS-1$
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite c = (Composite) super.createDialogArea(parent);
		final GridLayout gl = (GridLayout) c.getLayout();
		gl.numColumns = 3;
		gl.makeColumnsEqualWidth = false;
		gl.marginHeight = gl.marginWidth = 5;
		final Group gTS = GuiFormatTools.createGroup(c, Activator.resString("sslsettings.dialog.truststore.group")); //$NON-NLS-1$
		trustStorePathText = GuiFormatTools.createLabelledTextWithFileSelector( //
				gTS, //
				Activator.resString("sslsettings.dialog.file.name"), false, //$NON-NLS-1$
				Activator.resString("sslsettings.dialog.file.selector"), //$NON-NLS-1$
				new String[] { "*.*" }); //$NON-NLS-1$
		trustStorePathText.addModifyListener(this::trustStoreFieldsModified);
		trustStorePasswordText = GuiFormatTools.createLabelledText(gTS, //
				Activator.resString("sslsettings.dialog.file.password")); //$NON-NLS-1$
		trustStorePasswordText.setEchoChar('*');
		trustStorePasswordText.addModifyListener(this::trustStoreFieldsModified);
		final Group gKS = GuiFormatTools.createGroup(c, Activator.resString("sslsettings.dialog.keystore.group")); //$NON-NLS-1$
		keyStorePathText = GuiFormatTools.createLabelledTextWithFileSelector( //
				gKS, Activator.resString("sslsettings.dialog.file.name"), //$NON-NLS-1$
				false, //
				Activator.resString("sslsettings.dialog.file.selector"), //$NON-NLS-1$
				new String[] { "*.*" }); //$NON-NLS-1$
		keyStorePasswordText = GuiFormatTools.createLabelledText(gKS, //
				Activator.resString("sslsettings.dialog.file.password"));  //$NON-NLS-1$
		keyStorePasswordText.setEchoChar('*');
		final Composite bar = new Composite(c, SWT.NONE);
		final GridLayout layout = new GridLayout(3, false);
		bar.setLayout(layout);
		layout.marginHeight = layout.marginWidth = gl.marginLeft = 0;
		final GridData gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = 3;
		bar.setLayoutData(gd);
		importCertificateButton = new Button(gTS, SWT.PUSH);
		importCertificateButton.setText(Activator.resString("sslsettings.dialog.button.import.certificate")); //$NON-NLS-1$
		importCertificateButton.setLayoutData(GridDataFactory.swtDefaults().span(3, 1).create());
		importCertificateButton.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						final SSLKeyStoreManager manager = new SSLKeyStoreManager(trustStorePathText.getText(),
								trustStorePasswordText.getText());
						final File certificateFile = manager.chooseCertificateFile();
						if (certificateFile != null) {
							final List<CertificateInformation> certificates = manager.listCertificates(certificateFile);
							final CertificateInformation certificateInformation = SSLCertificateSelectorDialog
									.select(certificates);
							if (certificateInformation != null) {
								if (!manager.trustCertificate(certificateInformation)) {
									MessageDialog.openError(getShell(), //
											Activator.resString("sslsettings.dialog.button.import.error"), //$NON-NLS-1$
											Activator.resString("sslsettings.dialog.button.import.error.detail", //$NON-NLS-1$
													manager.getLastError(), manager.getLastException()));
								}
							}
						}
					}
				});
		final Label t = new Label(bar, SWT.NONE);
		GridData gdata = new GridData(GridData.FILL_HORIZONTAL);
		gdata.grabExcessHorizontalSpace = true;
		gdata.horizontalAlignment = GridData.FILL_HORIZONTAL;
		t.setLayoutData(gdata);
		final Button setDefaultButton = new Button(bar, SWT.PUSH);
		setDefaultButton.setText(Activator.resString("sslsettings.dialog.button.reset")); //$NON-NLS-1$
		gdata = new GridData();
		gdata.horizontalAlignment = GridData.END;
		gdata.widthHint = 120;
		setDefaultButton.setLayoutData(gdata);
		setDefaultButton.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						provider.resetToDefault();
						trustStorePathText.setText(provider.getTrustStorePath());
						trustStorePasswordText.setText(new String(provider.getTrustStorePassword()));
						keyStorePathText.setText(provider.getKeyStorePath());
						keyStorePasswordText.setText(new String(provider.getKeyStorePassword()));
					}
				});
		trustStorePathText.setText(provider.getTrustStorePath());
		trustStorePasswordText.setText(new String(provider.getTrustStorePassword()));
		keyStorePathText.setText(provider.getKeyStorePath());
		keyStorePasswordText.setText(new String(provider.getKeyStorePassword()));
		trustStoreFieldsModified(null);
		return c;
	}

	private void trustStoreFieldsModified(final ModifyEvent e) {
		importCertificateButton.setEnabled(
				!trustStorePathText.getText().isEmpty() &&
						!trustStorePasswordText.getText().isEmpty() &&
						new File(trustStorePathText.getText()).exists());
	}

	@Override
	protected void okPressed() {
		provider.setKeyStorePath(keyStorePathText.getText());
		provider.setKeyStorePassword(keyStorePasswordText.getText().toCharArray());
		provider.setTrustStorePassword(trustStorePasswordText.getText().toCharArray());
		provider.setTrustStorePath(trustStorePathText.getText());
		if (provider.save()) {
			super.okPressed();
		}
	}
}
