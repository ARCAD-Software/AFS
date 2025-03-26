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
package com.arcadsoftware.afs.client.server.internals.ui.composites;

import java.text.SimpleDateFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSStandardComposite;
import com.arcadsoftware.crypt.CertificateInformation;

public class SSLLocalCertificateListComposite extends AbstractAFSStandardComposite {

	private static final SimpleDateFormat DF = new SimpleDateFormat("yyyyMMdd - HH:mm:sss");
	
	private List certificateList;
	private java.util.List<CertificateInformation> input;
	private CertificateInformation selectedCertificate;

	public SSLLocalCertificateListComposite(Composite parent, int style) {
		super(parent, style, false);
		createControlPage();
	}

	@Override
	public void createControlPage() {
		certificateList = new List(this, SWT.V_SCROLL | SWT.H_SCROLL);
		final GridData gd = new GridData(GridData.FILL_BOTH);
		gd.grabExcessHorizontalSpace = true;
		gd.grabExcessVerticalSpace = true;
		certificateList.setLayoutData(gd);
		certificateList.addSelectionListener(
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent arg0) {
						updateSelectedCertificate();
					}
				});
	}

	public void setInput(java.util.List<CertificateInformation> input) {
		this.input = input;
		// ""server.connection.certificate.detail=Subject: %s, Issuer: %s.\n""
		for (final CertificateInformation ci : input) {
			final StringBuilder sb = new StringBuilder();
			sb.append("[").append(ci.getAlgorithm()).append("] - [");
			sb.append(ci.getFingerPrint()).append("] - [");
			sb.append(DF.format(ci.getStartDate())).append(" / ");
			sb.append(DF.format(ci.getExpirationDate())).append("]");
			certificateList.add(sb.toString());
		}
		if (!input.isEmpty()) {
			certificateList.select(0);
			updateSelectedCertificate();
		}
	}

	public CertificateInformation getSelectedCertificate() {
		return selectedCertificate;
	}

	private void updateSelectedCertificate() {
		selectedCertificate = input.get(certificateList.getSelectionIndex());
	}
}
