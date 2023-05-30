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
package com.arcadsoftware.afs.client.server.ui.actions;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.afs.client.core.security.TrustStoreProviderExtensionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.dialogs.SSLSettingsDialog;

public class EditTLSSettingsAction extends ArcadAction {
	
	@Override
	protected void setInterface() {
		setText(Activator.resString("sslsettings.action.edit.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("sslsettings.action.edit.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.TLS.imageDescriptor());
	}
	
	@Override
	protected boolean execute() {
		final ITrustStoreProvider provider = getTrustStoreProvider();	
		if (provider != null) {
			SSLSettingsDialog.update(provider);
			return true;
		}
		return false;
	}
	
	private ITrustStoreProvider getTrustStoreProvider() {
		return TrustStoreProviderExtensionManager.getTrustStoreProvider();
	}	
}
