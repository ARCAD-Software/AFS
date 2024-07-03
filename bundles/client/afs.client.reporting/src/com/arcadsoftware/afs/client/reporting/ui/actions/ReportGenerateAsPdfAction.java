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
package com.arcadsoftware.afs.client.reporting.ui.actions;

import java.util.List;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.reporting.Activator;

public class ReportGenerateAsPdfAction extends AbstractPreviewAction {

	public ReportGenerateAsPdfAction(ServerConnection connection) {
		super(connection);
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("report.action.generate.pdf.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("report.action.generate.pdf.text")); //$NON-NLS-1$
		setImageDescriptor(AFSIcon.REPORT_PDF.imageDescriptor());
	}

	@Override
	public String getViewerType() {
		return REPORT_FORMAT_PDF;
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return null;
	}

}
