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
package test.rcp;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;

public class ConnectHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		Activator.server = new Server();
		Activator.server.setDescription("Le serveur de la mort !");
		Activator.server.setLastLogin("admin@quadra");
		Activator.server.setLastPassword("quadra");
		Activator.server.setUrl("http://localhost:5252/");
		//Activator.server.setUrl("https://192.168.2.101:5253/");
		
		ServerConnection sc = ConnectionManager.getInstance().connect(Activator.server, true);
		if (sc == null) {
			MessageDialog.openInformation(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Error NULL Connection", "Connection error or cancel...");
			
		} else if (sc.isConnected()) {
			MessageDialog.openInformation(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Connection OK", "Successful connection");
		} else {
			String error = "";
			if (sc.getErrorMessage() != null) {
				error = sc.getErrorMessage().getLabel();
			}
			if (sc.getErrorCause() != null) {
				error += "\n" + sc.getErrorCause().getLocalizedMessage();
			}
			MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Connection Error", error);
		}
		return null;
	}

}
