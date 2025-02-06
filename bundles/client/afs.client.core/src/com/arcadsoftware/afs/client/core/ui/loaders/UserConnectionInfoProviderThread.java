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
package com.arcadsoftware.afs.client.core.ui.loaders;

public class UserConnectionInfoProviderThread extends Thread
		implements IUserConnectionInfoProvider {

	private String login;
	private String password;

	public UserConnectionInfoProviderThread(String name) {
		super(name);
		final Thread thread = Thread.currentThread();
		if (thread instanceof IUserConnectionInfoProvider) {
			login = ((IUserConnectionInfoProvider) thread).getLogin();
			password = ((IUserConnectionInfoProvider) thread).getPassword();
		}
	}

	@Override
	public void setLogin(String login) {
		this.login = login;
	}

	@Override
	public void setPassword(String password) {
		this.password = password;
	}

	@Override
	public String getLogin() {
		return login;
	}

	@Override
	public String getPassword() {
		return password;
	}

}
