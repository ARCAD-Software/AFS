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
package com.arcadsoftware.rest.connection;

import java.awt.Desktop;
import java.awt.Desktop.Action;
import java.io.IOException;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.rest.WebServiceAccess;

/**
 * This class help the "client" program through the connection process when different type 
 * of authentication services are provided by the "server".
 * 
 * <p>
 * A classic walk through should be:
 * 
 * <ul>
 * <li>Create the ConnectionHelper with your configured WebServiceAccess object. this object need to be configured to access
 * the targeted HTTP Server (with server URL, SSL certificate, proxy and any required debug facilities).
 * <li>You should then first test for a SSO ready connection, calling <b>isSSOLogged()</b> method.
 * <li>Then you may try an OAuth authentication (see details below) call the <b>loginOAuth(...)</b> method.
 * <li>Of the previous operations fail then ask the user about Login and password and call <b>login(login,pwd)</b> method.
 * <li>As soon as one of the previous method is a success you may call <b>getCurrentUserInfo()</b> method to get the 
 * current user access right.
 * </ul>
 * 
 * <p>
 * The OAuth authentication process involve a web browser. default implementation call the default web browser of this
 * workstation. You may have to override the <b>launchBrowser(URL)</b> method to use another browser. If you plan to use an
 * embedded browser into your java application be sure to call the <b>loginOAuth(...)</b> method from a non GUI thread as 
 * this method is a blocking one.
 * 
 * <p>
 * If you want to list the Authentication schema activated into the targeted serve, call the <b>getAuthServices()</b> method, 
 * the returned list contain one or many of the AUTH_* constants defined into this class. Note that all of these schema expect 
 * AUTH_OAUTH and AUTH_SSO require a login and a password from the user...
 * 
 * @author ARCAD Software
 */
public class ConnectionHelper {
	
	public static final String AUTH_SSO = "spnego"; //$NON-NLS-1$
	public static final String AUTH_OAUTH = "oauth"; //$NON-NLS-1$
	public static final String AUTH_LDAP = "ldapauth"; //$NON-NLS-1$
	public static final String AUTH_CONFIG = "configauth"; //$NON-NLS-1$
	public static final String AUTH_DB = "localauth"; //$NON-NLS-1$
	public static final String AUTH_IBMI = "ibmiauth"; //$NON-NLS-1$
	public static final String AUTH_SPRING = "springauth"; //$NON-NLS-1$
	public static final String AUTH_JAAS = "jaasauth"; //$NON-NLS-1$
	public static final String AUTH_WINDOWS = "winauth"; //$NON-NLS-1$
	
	private final WebServiceAccess webServiceAccess;
	private final ILoggedPlugin activator;
	private final XmlCurrentUserStream xstream;
	private final String oAuthKey;
	private ArrayList<String> authServices;
	private IConnectionUserBean userInfo;

	public ConnectionHelper(WebServiceAccess webServiceAccess, ILoggedPlugin activator) {
		super();
		this.activator = activator;
		this.webServiceAccess = webServiceAccess;
		xstream = new XmlCurrentUserStream();
		xstream.alias("list", ArrayList.class); //$NON-NLS-1$
		xstream.alias("code", String.class); //$NON-NLS-1$
		String hn = System.getProperty("user.name", "noname") + '/'; //$NON-NLS-1$ //$NON-NLS-1$
		try {
			hn += InetAddress.getLocalHost().getHostName();
		} catch (Exception e) {
			if (activator != null) {
				activator.error(e.getLocalizedMessage(), e);
			}
		}
		hn += '/' + System.currentTimeMillis();
		oAuthKey = Crypto.fog(hn.toCharArray());
	}

	/**
	 * Get the Web-service access.
	 * 
	 * @return
	 */
	public WebServiceAccess getWebServiceAccess() {
		return webServiceAccess;
	}
	
	/**
	 * Get the list of supported authentication services published onto the server.
	 * 
	 * <p>
	 * If caller know that an unpublished service is provided by the server ha may add it to the given list.
	 * 
	 * <p>
	 * 
	 * @return never return null.
	 * @throws ServerErrorException
	 */
	public List<String> getAuthServices() throws ServerErrorException {
		if (authServices != null) {
			return authServices;
		}
		Object o = xstream.fromXML(webServiceAccess.get("/authservices")); //$NON-NLS-1$
		authServices = new ArrayList<>();
		if (o instanceof List) {
			for(Object x: (List<?>) o) {
				if (x != null) {
					authServices.add(x.toString());
				}
			}
		}
		return authServices;
	}
	
	/**
	 * Check that SSO is activated and is the current user is already connected with it.
	 * 
	 * <p>
	 * This method check that SSO is supported and then activate the facility and force a HTTP call (getting current User rights).
	 * If it fail then desactivate the option.
	 *  
	 * @return true if the SSO log is completed.
	 * @throws ServerErrorException
	 */
	public boolean isSSOLogged() throws ServerErrorException {
		if (!getAuthServices().contains(AUTH_SSO)) {
			return false;
		}
		webServiceAccess.setUseSSO(true);
		try {
			if (getCurrentUserInfo() != null) {
				return true;
			}
			webServiceAccess.setUseSSO(false);
			return false;
		} catch (ServerErrorException e) {
			if (e.isClientError()) {
				if (activator != null) {
					activator.info("Invalid SSO connection parameters.", e);
				}
				return false;
			}
			throw e;
		}
	}

	/**
	 * Engage a OAuth identification process involving the AFS server facilities.
	 * 
	 * <p>
	 * This method is blocking during the authentication process that involve an external web browser.
	 * If you use the application itself with an embedded browser (see {@link #launchBrowser(URL)}) 
	 * you must <b>not</b> call this method from the GUI thread.
	 * 
	 * @param timeout maximal duration (ms) of the process completion, values less than 3000ms are ignored.
	 * @param monitor may be null.
	 * @return true is the process complete.
	 * @throws ServerErrorException
	 */
	public boolean loginOAuth(long timeout, IProgressMonitor monitor) throws ServerErrorException {
		if (!getAuthServices().contains(AUTH_OAUTH)) {
			return false;
		}
		SubMonitor sb = SubMonitor.convert(monitor, 5000);
		sb.newChild(500);
		if (sb.isCanceled()) {
			return false;
		}
		URL url = getOAuthURL();
		launchBrowser(url);
		long t = System.currentTimeMillis();
		if (sb.isCanceled()) {
			return false;
		}
		sb.newChild(3000);
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {
			return false;
		}
		if (sb.isCanceled()) {
			return false;
		}
		while (!getOAuthResponse()) {
			sb.setWorkRemaining(1500).split(500);
			if (sb.isCanceled() || (System.currentTimeMillis() - t > timeout)) {
				return false;
			}
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				return false;
			}
		}
		sb.setWorkRemaining(0);
		return true;
	}

	/**
	 * Get the OAuth authentication URL for external connection.
	 * @return null if the WebServiceAccess is not correctly configured.
	 */
	public URL getOAuthURL() {
		try {
			return webServiceAccess.getURL("/oauth/redirect/" + oAuthKey); //$NON-NLS-1$
		} catch (MalformedURLException e) {
			return null;
		}
	}
	
	/**
	 * 
	 * @return
	 * @throws ServerErrorException
	 */
	public boolean getOAuthResponse() throws ServerErrorException {
		try {
			String xml = webServiceAccess.delete("/oauth/redirect/" + oAuthKey, null); //$NON-NLS-1$
			Object o = xstream.fromXML(xml);
			if (o instanceof String) {
				webServiceAccess.setToken((String) o);
				return true;
			}
			return false;
		} catch (ServerErrorException e) {
			if (e.isClientError()) {
				if (activator != null) {
					activator.info("Invalid SSO connection parameters.", e);
				}
				return false;
			}
			throw e;
		}
	}
	
	/**
	 * Login with a classic login+password form.
	 * @param userLogin
	 * @param password
	 * @return
	 */
	public boolean login(String userLogin, char[] password) throws ServerErrorException {
		webServiceAccess.setUseSSO(false);
		webServiceAccess.setLogin(userLogin);
		webServiceAccess.setPassword(password);
		try {
			return getCurrentUserInfo() != null;
		} catch (ServerErrorException e) {
			if (e.isClientError()) {
				if (activator != null) {
					activator.info("Invalid SSO connection parameters.", e);
				}
				return false;
			}
			throw e;
		}
	}
	
	/**
	 * Get the current user informations.
	 * 
	 * <p>
	 * This contains the user access's rights, the user internal ID (UIID) and eventually the full name for presentation purposes.
	 *   
	 * @return null if the current user is not yet logged on.
	 * @throws ServerErrorException
	 */
	public IConnectionUserBean getCurrentUserInfo() throws ServerErrorException {
		if (userInfo == null) {
			Object o = xstream.fromXML(webServiceAccess.get("/currentuser")); //$NON-NLS-1$
			if (o instanceof IConnectionUserBean) {
				userInfo = (IConnectionUserBean) o;
			}
		}
		return userInfo;
	}
	
	/**
	 * User may override this method to manage the browser delegation inside the application context.
	 * 
	 * <p>
	 * Default implementation launch the default browser of this workstation.
	 * 
	 * <p>
	 * This method is non stopping, the ConnectionHelper will continue to wait for a 
	 * complete OAuth connection to complete.
	 * 
	 * @param url
	 * @see #loginOAuth(long)
	 */
	protected void launchBrowser(URL url) {
		if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Action.BROWSE)) {
			try {
				Desktop.getDesktop().browse(url.toURI());
				return;
			} catch (IOException e) {
				if (activator != null) {
					activator.info(e.getLocalizedMessage(), e);
				}
			} catch (URISyntaxException e) {
				if (activator != null) {
					activator.warn(e.getLocalizedMessage(), e);
				}
			}
		}
		try {
			Runtime.getRuntime().exec("xdg-open " + url);
		} catch (IOException e) {
			if (activator != null) {
				activator.info(e.getLocalizedMessage(), e);
			}
		}
	}
}