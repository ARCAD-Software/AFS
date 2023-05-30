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
package com.arcadsoftware.afs.client.reporting.ui.actions;

import java.util.List;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.reporting.Activator;

public class ReportGenerateAsHTMLAction extends AbstractPreviewAction {
	
	public ReportGenerateAsHTMLAction(ServerConnection connection) {
		super(connection);
	}

	@Override
	protected void setInterface() {
		this.setText(Activator.resString("report.action.preview.text")); //$NON-NLS-1$
		this.setToolTipText(Activator.resString("report.action.preview.text")); //$NON-NLS-1$
		this.setImageDescriptor(AFSIcon.REPORT_HTML.imageDescriptor());
	}

	@Override
	public String getViewerType() {
		return REPORT_FORMAT_HTML;
	}

	@Override
    public String getViewerExtraParameters(){
        return "&__svg=false"; //$NON-NLS-1$
    }

	public List<Integer> getExpectedRigths() {
		return null;
	}
	
}
