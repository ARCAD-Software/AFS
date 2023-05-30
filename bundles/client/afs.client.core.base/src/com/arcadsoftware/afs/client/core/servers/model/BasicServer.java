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
package com.arcadsoftware.afs.client.core.servers.model;

public class BasicServer implements IServer, Cloneable {

	private String url;
	private String name;
	private String description;
	private String lastLogin = ""; //$NON-NLS-1$
	private String lastPassword = ""; //$NON-NLS-1$
	private boolean rememberPassword;
	private String proxyHost = "";//$NON-NLS-1$
	private String proxyPort = "";//$NON-NLS-1$
	private String proxyLogin = "";//$NON-NLS-1$
	private String proxyPassword = "";//$NON-NLS-1$
	
	@Override
	public String getUrl() {
		return url;
	}

	@Override
	public void setUrl(String url) {
		this.url = url;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	@Deprecated
	public Server duplicate() {
		Server result = new Server();
		result.setName(name);
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
		if (source.isRememberPassword()){
			setLastPassword(source.getLastPassword());
		}
		setProxyHost(source.getProxyHost());
		setProxyPort(source.getProxyPort());
		setProxyLogin(source.getProxyLogin());
		setProxyPassword(source.getProxyPassword());
	}	
	
	@Override
	public String getLastLogin() {
		return lastLogin;
	}

	@Override
	public void setLastLogin(String lastLogin) {
		this.lastLogin = lastLogin;
	}

	@Override
	public String getLastPassword() {
		return lastPassword;
	}

	@Override
	public void setLastPassword(String lastPassword) {
		//TODO [SSC] Crypter le password
		this.lastPassword = lastPassword;
	}

	@Override
	public boolean isRememberPassword() {
		return rememberPassword;
	}

	@Override
	public void setRememberPassword(boolean rememberPassword) {
		this.rememberPassword = rememberPassword;
	}

	@Override
	@Deprecated
	public String getProxyHost() {
		return proxyHost;
	}

	@Override
	@Deprecated
	public void setProxyHost(String proxyHost) {
		this.proxyHost = proxyHost;
	}

	@Override
	@Deprecated
	public String getProxyPort() {
		return proxyPort;
	}

	@Override
	@Deprecated
	public void setProxyPort(String proxyPort) {
		this.proxyPort = proxyPort;
	}

	@Override
	public String getProxyLogin() {
		return proxyLogin;
	}

	@Override
	public void setProxyLogin(String proxyLogin) {
		this.proxyLogin = proxyLogin;
	}

	@Override
	public String getProxyPassword() {
		return proxyPassword;
	}

	@Override
	public void setProxyPassword(String proxyPassword) {
		this.proxyPassword = proxyPassword;
	}
}
