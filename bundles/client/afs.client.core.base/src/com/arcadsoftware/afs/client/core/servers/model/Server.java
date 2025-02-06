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
package com.arcadsoftware.afs.client.core.servers.model;

import com.arcadsoftware.aev.core.model.ArcadEntity;

/**
 * This class is used to describe an ARCAD Foundation Server.<br>
 * It contains all the necessary information to connect to the server located at the defined URL.<br>
 * During the connection, the user can request for saving his login/password to be able to get automatically get them
 * when choosing the server.
 *
 * @author ARCAD Software
 */
public class Server extends ArcadEntity implements IServer, Cloneable {

	private final BasicServer internalServer = new BasicServer();

	@Override
	public String getLabel() {
		return internalServer.getName();
	}

	@Override
	public String getUrl() {
		return internalServer.getUrl();
	}

	@Override
	public void setUrl(String url) {
		internalServer.setUrl(url);
	}

	@Override
	public String getName() {
		return internalServer.getName();
	}

	@Override
	public void setName(String name) {
		internalServer.setName(name);
	}

	@Override
	public String getDescription() {
		return internalServer.getDescription();
	}

	@Override
	public void setDescription(String description) {
		internalServer.setDescription(description);
	}

	/**
	 * @see #clone()
	 */
	@Override
	@Deprecated
	public Server duplicate() {
		final Server result = new Server();
		result.setName(getName());
		result.assign(this);
		return result;
	}

	@Override
	public Server clone() {
		return duplicate();
	}

	@Override
	public void assign(IServer source) {
		setUrl(source.getUrl());
		setLastLogin(source.getLastLogin());
		setRememberPassword(source.isRememberPassword());
		if (source.isRememberPassword()) {
			setLastPassword(source.getLastPassword());
		}
		setProxyHost(source.getProxyHost());
		setProxyPort(source.getProxyPort());
		setProxyLogin(source.getProxyLogin());
		setProxyPassword(source.getProxyPassword());
	}

	@Override
	public String getIconID() {
		return "SERVER"; //$NON-NLS-1$
	}

	@Override
	public String getLastLogin() {
		return internalServer.getLastLogin();
	}

	@Override
	public void setLastLogin(String lastLogin) {
		internalServer.setLastLogin(lastLogin);
	}

	@Override
	public String getLastPassword() {
		return internalServer.getLastPassword();
	}

	@Override
	public void setLastPassword(String lastPassword) {
		internalServer.setLastPassword(lastPassword);
	}

	@Override
	public boolean isRememberPassword() {
		return internalServer.isRememberPassword();
	}

	@Override
	public void setRememberPassword(boolean rememberPassword) {
		internalServer.setRememberPassword(rememberPassword);
	}

	@Override
	@Deprecated
	public String getProxyHost() {
		return internalServer.getProxyHost();
	}

	@Override
	@Deprecated
	public void setProxyHost(String proxyHost) {
		internalServer.setProxyHost(proxyHost);
	}

	@Override
	@Deprecated
	public String getProxyPort() {
		return internalServer.getProxyPort();
	}

	@Override
	@Deprecated
	public void setProxyPort(String proxyPort) {
		internalServer.setProxyPort(proxyPort);
	}

	@Override
	public String getProxyLogin() {
		return internalServer.getProxyLogin();
	}

	@Override
	public void setProxyLogin(String proxyLogin) {
		internalServer.setProxyLogin(proxyLogin);
	}

	@Override
	public String getProxyPassword() {
		return internalServer.getProxyPassword();
	}

	@Override
	public void setProxyPassword(String proxyPassword) {
		internalServer.setProxyPassword(proxyPassword);
	}
}
