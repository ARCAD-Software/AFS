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

/**
 * FIXME Undocumented API is not an API.
 * 
 * @author ARCAD Software
 */
public interface IServer {

	/**
	 * Get the connection URL of the related server.
	 * @return a string representation of the server URL.
	 */
	public String getUrl();
	
	/**
	 * Define the connection URL of the related server.
	 */
	public void setUrl(String url);

	/**
	 * Return the user-defined name of the related server
	 * @return the name of the related server
	 */
	public String getName();
	
	/**
	 * Define the user-defined name of the related server.<br>
	 * This name is used as a mnemonic to identify the server to connect to.
	 * @param name The user-defined name of the server
	 */
	public void setName(String name);

	/**
	 * Return the user-defined description of the related server 
	 * @return The user-defined description of the related server
	 */
	public String getDescription();
	
	/**
	 * Define the user-defined description of the related server.<br>
	 * @param description
	 */
	public void setDescription(String description);

	/**
	 * Use this method to get a duplicated instance of the current instance
	 * @return
	 * @deprecated use Clone !
	 */
	public IServer duplicate();
	
	/**
	 * Assign the current instance with the value that come from the instance
	 * passed as a parameter
	 * @param source Server instance from which you want to get the values.
	 */
	public void assign(IServer source);
	
	/**
	 * Return the last login used to connect to the related server.
	 * @return The last login
	 */
	public String getLastLogin();
	
	/**
	 * Define the last login used to connect to the related server
	 * @param lastLogin
	 */
	public void setLastLogin(String lastLogin);
	
	/**
	 * Return the last password used to connect to the related server.
	 * @return The last password
	 */
	public String getLastPassword();
	
	/**
	 * Define the last password used to connect to the related server
	 * @param the last password
	 */
	public void setLastPassword(String lastPassword);

	/**
	 * Indicate if the user request for saving the last login and password.
	 * @return true is the login/password have to be saved
	 */
	public boolean isRememberPassword();
	
	/**
	 * Define if you want to save the login/password used to connect to the related server.
	 * @param rememberPassword
	 */
	public void setRememberPassword(boolean rememberPassword);

	/**
	 * 
	 * @return
	 * @deprecated On RCP use the Eclipse Proxy facility, on server side... do not use AFS client !!!
	 */
	public String getProxyHost();

	/**
	 * 
	 * @param proxyHost
	 * @deprecated On RCP use the Eclipse Proxy facility, on server side... do not use AFS client !!!
	 */
	public void setProxyHost(String proxyHost);

	/**
	 * 
	 * @return
	 * @deprecated On RCP use the Eclipse Proxy facility, on server side... do not use AFS client !!!
	 */
	public String getProxyPort();

	/**
	 * 
	 * @param proxyPort
	 * @deprecated On RCP use the Eclipse Proxy facility, on server side... do not use AFS client !!!
	 */
	public void setProxyPort(String proxyPort);

	/**
	 * 
	 * @return
	 */
	public String getProxyLogin();

	/**
	 * 
	 * @param proxyLogin
	 */
	public void setProxyLogin(String proxyLogin);

	/**
	 * 
	 * @return
	 */
	public String getProxyPassword();

	/**
	 * 
	 * @param proxyPassword
	 */
	public void setProxyPassword(String proxyPassword);	
}
