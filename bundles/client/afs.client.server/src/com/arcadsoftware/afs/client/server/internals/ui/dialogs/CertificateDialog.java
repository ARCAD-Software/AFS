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
/**
 * This dialog allows user to accept certificate from server.
 * Certificate can be accepted from server or loaded from a given keystore or form certificate file
 */
package com.arcadsoftware.afs.client.server.internals.ui.dialogs;

import java.io.File;
import java.security.cert.X509Certificate;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.crypt.CertificateInformation;
import com.arcadsoftware.crypt.InstallCertificates;

public class CertificateDialog extends AbstractAFSDialog {

	// Certificate : origin choices
	private static final int ACCEPT_FROM_SERVER = 0;
	private static final int LOAD_CERTIFFILE = 1;
	private static final String[] CHOICES = { "server.connection.certificate.choice.acceptfromserver",
			"server.connection.certificate.choice.loadcertificatefile" };
	private static final String[] CERTIFFILE_FILTERS = { "*.*" };
	private static final String HTTPSPREFIX = "https://"; //$NON-NLS-1$

	Composite mainComposite;
	Button[] choices;
	Server server;

	public CertificateDialog(Shell parentShell, Server server) {
		super(parentShell, true, true);
		this.server = server;
	}

	@Override
	public Point getSize() {
		return new Point(520, 390);
	}

	@Override
	protected void okPressed() {
		if (choices[ACCEPT_FROM_SERVER].getSelection()) {
			if (!getCertificate(null)) {
				cancelPressed();
				return;
			}
		} else {
			// Open File selector
			final String certifFile = GuiFormatTools
					.choosefile(Activator.resString("server.connection.certificate.accept.title"), CERTIFFILE_FILTERS);
			if ((certifFile == null) || certifFile.isEmpty()) {
				// keep dialog open. User will cancel to exit
				return;
			} else {
				if (!getCertificate(new File(certifFile))) {
					cancelPressed();
					return;
				}
			}
		}

		super.okPressed();
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		final GridLayout gd = (GridLayout) composite.getLayout();
		gd.marginHeight = gd.marginWidth = 0;
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;

		mainComposite = new Composite(composite, SWT.NONE);
		final GridData griddata = new GridData(GridData.FILL_BOTH);
		griddata.grabExcessHorizontalSpace = true;
		griddata.grabExcessVerticalSpace = true;
		mainComposite.setLayoutData(griddata);
		mainComposite.setLayout(new GridLayout(1, false));

		choices = GuiFormatTools.createRadioButtonGroup(mainComposite, "",
				new String[] { Activator.resString(CHOICES[ACCEPT_FROM_SERVER]),
						Activator.resString(CHOICES[LOAD_CERTIFFILE]) },
				0);

		choices[ACCEPT_FROM_SERVER].getParent().setLayout(new GridLayout(1, true));

		return composite;
	}

	@Override
	public String getTitle() {
		return Activator.resString("server.connection.certificate.title.getCertificate"); //$NON-NLS-1$
	}

	private String getKeyStore() {
		return System.getProperty("javax.net.ssl.trustStore");
	}

	private char[] getKeyStorePassword() {
		final String s = System.getProperty("javax.net.ssl.trustStorePassword");
		if (s == null) {
			return new char[0];
		}
		return s.toCharArray();
	}

	private String[] getHostAndPort(String hostUrl) {
		final String[] result = new String[2];
		if ((hostUrl != null)) {
			// Get the server address
			String host = hostUrl.substring(HTTPSPREFIX.length(), hostUrl.length());
			int i = host.indexOf('/');
			if (i > -1) {
				host = host.substring(0, i);
			}
			String port = "443";
			i = host.indexOf(':');
			if (i > -1) {
				port = host.substring(i + 1);
				host = host.substring(0, i);
			}
			result[0] = host;
			result[1] = port;
		}
		return result;
	}

	private InstallCertificates getInstallCertificate(String host) {
		final InstallCertificates ic = new InstallCertificates(getKeyStore(), getKeyStorePassword(), null);
		ic.setAliasPrefix(host + '-');
		return ic;
	}

	/**
	 * Get certificate
	 *
	 * @param certifFile
	 *            if null, get certificate from server, else load certificate file
	 * @return
	 */
	private boolean getCertificate(File certifFile) {

		final String[] hostAndPort = getHostAndPort(server.getUrl());
		final String host = hostAndPort[0];
		int port = 443;
		try {
			port = Integer.parseInt(hostAndPort[1]);
		} catch (final Throwable t) {
		}

		final InstallCertificates ic = getInstallCertificate(host);
		List<CertificateInformation> certificates = null;

		if (certifFile == null) {
			certificates = ic.getUntrustedCertificates(host, port);
		} else if (certifFile.isFile()) {
			certificates = ic.getUntrustedCertificates(certifFile);
		}

		if ((ic.getLastError() > 0) || (certificates == null) || (certificates.size() == 0)) {
			MessageDialog.openError(getShell(),
					Activator.resString("server.connection.certificate.error.title.nocertificate"),
					String.format(Activator.resString("server.connection.certificate.error.description.nocertificate"),
							server.getUrl()));
			Activator.getInstance().log(
					String.format(Activator.resString("server.connection.certificate.error.nocertificate"), host, port),
					null);
			return false;
		} else {
			final StringBuilder message = new StringBuilder();
			for (final CertificateInformation certificateInformation : certificates) {
				String sb = Activator.resString("server.connection.certificate.none");

				final X509Certificate cert = certificateInformation.getCertificate();
				if (cert.getSubjectDN() != null) {
					sb = cert.getSubjectDN().toString();
				}
				String is = Activator.resString("server.connection.certificate.none");
				if (cert.getIssuerDN() != null) {
					is = cert.getIssuerDN().toString();
				}
				message.append(String.format(Activator.resString("server.connection.certificate.detail"), sb, is));
			}
			if (MessageDialog.openQuestion(getShell(),
					Activator.resString("server.connection.certificate.title.update"),
					String.format(Activator.resString("server.connection.certificate.description"), host,
							message.toString()))) {
				ic.clear();
				ic.trustCertificates(certificates);

				if (ic.getLastError() == InstallCertificates.ERROR_CERTIFICATE_IMPORT) {
					MessageDialog.openError(getShell(),
							Activator.resString("server.connection.certificate.title.storeCertificate"),
							String.format(Activator.resString("server.connection.certificate.error.save"), host, port));
					Activator.getInstance().log(
							String.format(Activator.resString("server.connection.certificate.error.save"), host, port),
							null);
					return false;
				}
				return true;
			}
		}

		return false;
	}
}
