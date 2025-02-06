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
package com.arcadsoftware.afs.client.reporting.ui.actions;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBasicAction;
import com.arcadsoftware.afs.client.reporting.core.ReportHeader;
import com.arcadsoftware.afs.client.reporting.ui.editors.ReportPreviewEditorInput;

public abstract class AbstractPreviewAction extends AbstractConnectedBasicAction {

	public AbstractPreviewAction(ServerConnection connection) {
		super(connection);
	}

	public static final String REPORT_FORMAT_PDF = "pdf"; //$NON-NLS-1$
	public static final String REPORT_FORMAT_HTML = "html"; //$NON-NLS-1$

	ReportHeader report = null;

	@Override
	protected boolean canExecute() {
		report = getReport();
		return (report != null);

	}

	@Override
	protected boolean execute() {
		if (isAllowed()) {
			try {
				final ReportPreviewEditorInput input = new ReportPreviewEditorInput(connection, getReport(),
						getViewerType(), getViewerExtraParameters());
				ReportPreviewEditorInput.openEditor(input);
			} catch (final Exception e) {
				MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
			}
			return true;
		}
		return false;
	}

	protected ReportHeader getReport() {
		return null;
	}

	public abstract String getViewerType();

	public String getViewerExtraParameters() {
		return "";
	}

}
