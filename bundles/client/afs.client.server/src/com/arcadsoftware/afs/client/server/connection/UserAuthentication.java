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
package com.arcadsoftware.afs.client.server.connection;

import java.net.PasswordAuthentication;

import org.eclipse.swt.widgets.Display;

import com.arcadsoftware.afs.client.core.connection.IUserAuthentication;
import com.arcadsoftware.afs.client.server.internals.ui.dialogs.UserAuthenticationDialog;

public class UserAuthentication implements IUserAuthentication {

	private class UIOperation implements Runnable {

		private PasswordAuthentication authentication;
		private final String host;
		private final String message;
		private final String scheme;

		public UIOperation(String scheme, String host, String message) {
			super();
			this.scheme = scheme;
			this.host = host;
			this.message = message;
		}

		@Override
		public void run() {
			final UserAuthenticationDialog dialog = new UserAuthenticationDialog(null, scheme, host, message);
			dialog.open();
			authentication = dialog.getAuthentication();
		}
	}

	@Override
	public PasswordAuthentication getUserAuthentication(String scheme, String host, String message) {
		final UIOperation uio = new UIOperation(scheme, host, message);
		if (Display.getCurrent() != null) {
			uio.run();
		} else {
			Display.getDefault().syncExec(uio);
		}
		return uio.authentication;
	}

}
