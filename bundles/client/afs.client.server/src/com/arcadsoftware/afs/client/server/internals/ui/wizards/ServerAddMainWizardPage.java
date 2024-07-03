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
package com.arcadsoftware.afs.client.server.internals.ui.wizards;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.ui.wizards.AbstractAFSSimpleItemWizardPage;
import com.arcadsoftware.afs.client.server.internals.ui.composites.ServerMainComposite;

public class ServerAddMainWizardPage extends AbstractAFSSimpleItemWizardPage {

	private ServerMainComposite serverMainComposite;

	public ServerAddMainWizardPage(String pageName, String title, String description) {
		super(pageName, title, description);
	}

	@Override
	protected boolean checkData() {
		final boolean result = serverMainComposite.checkData();
		if (result) {
			setErrorMessage(null);
		}
		return result;
	}

	@Override
	protected void createControlPage(Composite parent) {
		serverMainComposite = new ServerMainComposite(parent, SWT.NONE, null);
	}

	public String getServerName() {
		String serverName = serverMainComposite.getServerName();
		if ((serverName == null) || (serverName.trim().length() == 0)) {
			try {
				serverName = new URL(serverMainComposite.getUrl()).getHost();
			} catch (final MalformedURLException e) {
			}
		}
		return serverName;
	}

	public String getUrl() {
		return serverMainComposite.getUrl();
	}

}
