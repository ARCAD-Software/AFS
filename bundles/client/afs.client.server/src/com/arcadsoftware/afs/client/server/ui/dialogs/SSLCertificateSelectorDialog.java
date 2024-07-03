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

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.internals.ui.composites.SSLLocalCertificateListComposite;
import com.arcadsoftware.afs.framework.ui.dialogs.AbstractAFSDialog;
import com.arcadsoftware.crypt.CertificateInformation;

public class SSLCertificateSelectorDialog extends AbstractAFSDialog {

	private SSLLocalCertificateListComposite composite;
	private final List<CertificateInformation> input;

	protected SSLCertificateSelectorDialog(final Shell parentShell, final List<CertificateInformation> input) {
		super(parentShell);
		this.input = input;
	}

	@Override
	public int getHeight() {
		return 200;
	}

	@Override
	public int getWidth() {
		return 500;
	}

	@Override
	public String getTitle() {
		return "Select the certificate to import";
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite c = (Composite) super.createDialogArea(parent);
		final GridLayout gl = (GridLayout) c.getLayout();
		gl.marginHeight = gl.marginWidth = 1;
		composite = new SSLLocalCertificateListComposite(c, SWT.NONE);
		final GridData gd = new GridData(GridData.FILL_BOTH);
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		composite.setLayoutData(gd);
		composite.setInput(input);
		return c;
	}

	private CertificateInformation getSelected() {
		return composite.getSelectedCertificate();
	}

	public static CertificateInformation select(final List<CertificateInformation> input) {
		final SSLCertificateSelectorDialog dialog = new SSLCertificateSelectorDialog(
				Activator.getInstance().getPluginShell(), input);
		if (dialog.open() == 0) {
			return dialog.getSelected();
		}
		return null;
	}
}
