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
package com.arcadsoftware.cli.model;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.servers.model.BasicServer;
import com.arcadsoftware.afs.client.core.servers.model.IServer;

/**
 * This class represents a <i>handle</i> to an ARCAD-Anonymizer - Server
 * <p>
 * After defining the connection property, you have to use the {@link #connect()} method
 * the physically connect to the server.
 * </p>
 *  <p>
 *  You can get the connection status using the {@link #isConnected() } method
 *  </p>
 *  <p>
 *  This class also provides a {@link DataAccessHelper} helper Object to execute
 *  data manipulation on the server.
 *  </p>
 
   @since 1.0
 	
 
 * @author ARCAD Software
 *
 */
public class ServerHandle {
	
	private String url = "";
	private String login = "";
	private String password = "";
	
	
	private ServerConnection connection;
	private DataAccessHelper helper; 
	private boolean connected = false;	
	
	private ITrustStoreProvider trustStoreProvider = null;
	
	/**
	 * Constructs a new <code>ServerHandle</code> object using the
	 * given connection properties
	 * @param url The URL of the ARCAD-Anonymizer - Server including port number
	 * @param login A valid ARCAD-Anonymizer - Server login
	 * @param password A related password
	 */
	public ServerHandle(String url, String login, String password) {
		this.url = url;
		this.login = login;
		this.password = password;
	}
	
	/**
	 * Returns the URL of the related ARCAD-Anonymizer - Server
	 * @return the URL of the related ARCAD-Anonymizer - Server
	 */
	public String getUrl() {
		return url;
	}
	
	/**
	 * Sets the URL of the related ARCAD-Anonymizer - Server you want to reach
	 * @param url  Sets the URL of the related ARCAD-Anonymizer - Server 
	 */
	public void setUrl(String url) {
		this.url = url;
	}
	
	/**
	 * Returns the login used to connect the server
	 * @return - the login used to connect the server
	 */
	public String getLogin() {
		return login;
	}
	
	/**
	 * Sets the login used to connect the server
	 * @param login - A valid ARCAD-Anonymizer - Server login
	 */
	public void setLogin(String login) {
		this.login = login;
	}
	
	/**
	 * Gets the related password used to connect the server 
	 * @return - the related password used to connect the server
	 */
	public String getPassword() {
		return password;
	}
	
	/**
	 * Sets the related password used to connect the server 
	 * @param password - the related password used to connect the server 
	 */
	public void setPassword(String password) {
		this.password = password;
	}
	
    private boolean afsConnect(){
		IServer server = new BasicServer();
		server.setUrl(url);		
		connection = new ServerConnection(server);
		connection.setTrustStoreprovider(trustStoreProvider);
		boolean connected = connection.connect(login, password, true);
		if (connected) {
			helper = new DataAccessHelper(connection);
			return true;
		}
		return false;
    }

	/**
	 * Connects to the AFS Server.
	 * <p></p>
	 * @return True if connection succeeds, false otherwise
	 */
	public boolean connect() { 
		connected =  afsConnect();
		return connected;
	}
	
	/**
	 * Returns the connection status
	 * @return - true if the server is connected, false otherwise.
	 */
	public boolean isConnected() {
		return connected;
	}
	
	public DataAccessHelper getHelper() {
		return helper;
	}
	/**
	 * Return the server connection
	 * @returnThe related server Connection
	 */
	public ServerConnection getConnection() {
		return connection;
	}
	
	public void setTrustStoreProvider(ITrustStoreProvider trustStoreProvider) {
		this.trustStoreProvider = trustStoreProvider;
	}
	
	public ITrustStoreProvider getTrustStoreProvider() {
		return trustStoreProvider;
	}
}
