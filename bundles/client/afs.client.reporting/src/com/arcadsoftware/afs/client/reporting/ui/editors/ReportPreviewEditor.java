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
package com.arcadsoftware.afs.client.reporting.ui.editors;

import java.util.Locale;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.EditorPart;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.reporting.Activator;
import com.arcadsoftware.afs.client.reporting.core.ReportHeader;

public class ReportPreviewEditor extends EditorPart {

	private ReportPreviewEditorInput reportInput;
	private ServerConnection connection;
	private String currentUrl = null;
	private ReportHeader editedReport;
	private Browser browser;

	public ReportPreviewEditor() {
	}

	@Override
	public void doSave(IProgressMonitor progressMonitor) {
	}

	@Override
	public void doSaveAs() {
	}

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	@Override
	public void setFocus() {
	}

	@Override
	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		if ((input == null) || !(input instanceof ReportPreviewEditorInput)) {
			throw new PartInitException(Activator.resString("error.services.initializeEditor")); //$NON-NLS-1$
		}
		setInput(input);
		reportInput = (ReportPreviewEditorInput) input;
		editedReport = reportInput.getReport();
		connection = reportInput.getConnection();
		if (editedReport != null) {
			final String partName = editedReport.getName();
			setPartName((new StringBuilder(String.valueOf(getPartName()))).append(": ").append(partName).toString()); //$NON-NLS-1$
			firePropertyChange(1);
		}
		setSite(site);
	}

	@Override
	public void createPartControl(Composite parent) {
		browser = new Browser(parent, IWorkbenchBrowserSupport.AS_EDITOR | IWorkbenchBrowserSupport.LOCATION_BAR
				| IWorkbenchBrowserSupport.STATUS);
		browser.setLayoutData(new GridData(4, 4, true, true, 3, 1));
		preview();
	}

	public void preview() {
		try {
			final String previewType = ((ReportPreviewEditorInput) getEditorInput()).getViewerType();
			final String previewExtraParams = ((ReportPreviewEditorInput) getEditorInput()).getViewerExtraParameters();

			final String reportUrl = editedReport.getUrlOrFilename();
			final String proxyUrl = reportInput.getHelper().getRedirection(Activator.PROXYURL);

			currentUrl = String.format(ReportPreviewEditorInput.URL_FORMAT,
					proxyUrl,
					reportUrl,
					previewType,
					(previewExtraParams == null) ? "" : "&" + previewExtraParams,
					Locale.getDefault());

			// Internal Browser
			browser.setUrl(currentUrl);

		} catch (final Exception e) {
			MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
		}
	}

	public void refreshReport() {
		if (currentUrl != null) {
			browser.setUrl(currentUrl);
		}
	}

	public ServerConnection getConnection() {
		return connection;
	}

}
