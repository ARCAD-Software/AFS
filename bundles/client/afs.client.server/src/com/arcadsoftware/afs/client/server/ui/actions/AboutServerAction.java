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

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.ServerErrorException;

/**
 * This Action is add to the server connection context menu.
 * 
 * <p>
 * It retrieve the current connection information from the server, using the web-services /about and /currentuser.
 * 
 * @author ARCAD Software
 */
public class AboutServerAction extends AbstractConnectedAction {
	
	@Override
	protected boolean execute() {
		try {
			MessageDialog.openInformation(Display.getDefault().getActiveShell(), Activator.resString("about.dialog.title"), //$NON-NLS-1$
											Activator.resString("about.dialog.text", getAbout(), getCurrentUser(), getServerConnection().getServer().getUrl())); //$NON-NLS-1$
			return true;
		} catch (Exception e) {
			Activator.getInstance().error(e.getLocalizedMessage(), e);
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Exception", e.getMessage() + "\n\n" + ExceptionUtils.getStackTrace(e)); //$NON-NLS-1$  //$NON-NLS-2$
			return false;
		}
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("about.server.action")); //$NON-NLS-1$
		setToolTipText(Activator.resString("about.server.action.tooltip")); //$NON-NLS-1$
		setImageDescriptor(AFSIcon.SERVER_INFO.imageDescriptor());
	}
	
	private String getCurrentUser() throws ServerErrorException {
		return getServerConnection().getDataAccess() //
					.get("currentuser", "user") //$NON-NLS-1$ //$NON-NLS-2$
					.getString("name", "?"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	private String getAbout() throws ServerErrorException {
		final BeanMap about = getServerConnection().getDataAccess().get("about", "about"); //$NON-NLS-1$ //$NON-NLS-2$
		if (about != null) {
			final BeanMap application = about.getBeanMap("application"); //$NON-NLS-1$
			if (application != null) {
				return Activator.resString("about.dialog.application", //$NON-NLS-1$ 
						application.getString("name", "No name"), //$NON-NLS-1$ //$NON-NLS-2$
						application.getString("version", "No version")); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		return "Application not found /about";
	}
}