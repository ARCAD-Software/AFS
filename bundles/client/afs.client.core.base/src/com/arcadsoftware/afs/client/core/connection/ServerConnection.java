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
package com.arcadsoftware.afs.client.core.connection;

import java.net.Authenticator;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

import com.arcadsoftware.afs.client.core.IACCMessages;
import com.arcadsoftware.afs.client.core.internal.BaseActivator;
import com.arcadsoftware.afs.client.core.internal.ProxyAuthenticator;
import com.arcadsoftware.afs.client.core.servers.model.IServer;
import com.arcadsoftware.afs.framework.messages.AbstractUserMessageManager;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.client.DataAccess;
import com.arcadsoftware.rest.RestConnectionParameters;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.rest.WebServiceAccess;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.XmlCurrentUserStream;

/**
 */
public class ServerConnection {
	// FIXME Beaucoup de problème dans la gestion des login et mots de passe dans cette classe !!!!
	// FIXME This class is not thread safe !!!!!!!!!!!!!!

	private static final String HTTPPARAMETER_CLIENT_TRANSLATION = "localtranslate"; //$NON-NLS-1$

	private final IServer server;
	private DataAccess dataAccess;
	private UserMessage errorMessage;
	private Throwable errorCause;
	private boolean connected;
	private boolean connectionLost;
	private int maxConnectionAttempt = 50;
	private int connectionDelay = 2000;
	private String login; // FIXME This field duplicates IServer
	private String password; // FIXME This field duplicates IServer
	private ConnectionUserBean user;
	private AbstractUserMessageManager messageManager;
	private ITrustStoreProvider trustStoreprovider;

	public ServerConnection(IServer server) {
		super();
		this.server = server;
	}

	public void setMessageManager(AbstractUserMessageManager messageManager) {
		this.messageManager = messageManager;
	}

	public IServer getServer() {
		return server;
	}

	public UserMessage getErrorMessage() {
		return errorMessage;
	}

	public Throwable getErrorCause() {
		return errorCause;
	}

	public boolean isConnected() {
		return connected;
	}

	public ConnectionUserBean getUser() {
		return user;
	}

	public void manageError(String code, Object... var) {
		if (messageManager != null) {
			errorMessage = messageManager.getMessage(code, var);
		}
	}

	public void manageErrorException(ServerErrorException e) {
		// FIXME DO not call this method "Manage" it does not manage it transform the error
		// FIXME and it hides the error into a field which MUST BE MANAGED by the caller !
		// FIXME and this process is never explained, nowhere !!!
		// FIXME NO WHERE !!!!!!
		if (messageManager != null) {
			switch (e.getHTTPCode()) {
			case 401:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_401);
				break;
			case 403:
				String s = e.getMessage();
				if ((s != null) && (s.length() > 0)) {
					errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_403, s);
				} else {
					errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_403);
				}
				break;
			case 404:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_404, e.getDescription());
				break;
			case 412:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_412, e.getDescription());
				break;
			case 423:
				s = e.getMessage();
				if ((s != null) && (s.length() > 0)) {
					errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_423, s);
				} else {
					errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_423);
				}
				break;
			case 500:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_500, e.getDescription());
				break;
			case 503:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_503);
				break;
			case 1000:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_1000,
						(e.getDescription() != null) ? e.getDescription() : "");
				connectionLost = true;
				break;
			case 1001:
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_1001,
						(e.getDescription() != null) ? e.getDescription() : "");
				break;
			default:
				final int httpcode = e.getHTTPCode();
				String title;
				if ((e.getTitle() == null) || (e.getTitle().length() == 0)) {
					title = ""; //$NON-NLS-1$
				} else {
					title = e.getTitle();
				}
				String message;
				if ((e.getMessage() == null) || (e.getMessage().length() == 0)) {
					message = "";//$NON-NLS-1$
				} else {
					message = e.getMessage();
				}
				errorMessage = messageManager.getMessage(ISRVMessages.ERR_HTTP_X, httpcode, title, message);
				break;
			}
		} else {
			if (e.getHTTPCode() == 1000) {
				connectionLost = true;
			}
		}
		errorCause = e.getCause() != null ? e.getCause() : e;
	}

	/**
	 * Note that the protocol used is HTTP and the connection concept do not have any reality here !!!!
	 *
	 * @return
	 */
	public boolean isConnectionLost() {
		return connectionLost;
	}

	public int getMaxAttempt() {
		return maxConnectionAttempt;
	}

	public int getDelay() {
		return connectionDelay;
	}

	public void setMaxConnectionAttempt(int maxConnectionAttempt) {
		this.maxConnectionAttempt = maxConnectionAttempt;
	}

	public void setConnectionDelay(int connectionDelay) {
		this.connectionDelay = connectionDelay;
	}

	public static void sleepForAWhile(long millisecond) {
		final Object o = new Object();
		try {
			synchronized (o) { // FIXME Pourquoi synchroniser sur un objet créé à chaque appel de la méthode ?
				o.wait(millisecond, 0);
			}
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Reconnect to the HTPP server... just without listener
	 * <p>
	 * Because the HTTP protocol is stateless there is NO CONNECTION !!!! So this method allow just to test the
	 * connection parameter
	 *
	 * @return
	 */
	public boolean reconnect() {
		return reconnect(null);
	}

	public boolean reconnect(IConnectionAttemptListener listener) {
		boolean connectedAttempt = false;
		if (connectionLost) {
			int attempt = 0;
			final int maxAttempt = getMaxAttempt();
			while (!connectedAttempt && ((maxAttempt == -1) || (attempt < maxAttempt))) {
				attempt++;
				// FIXME Quel est le bon login/password ????
				connectedAttempt = connect(login, password, true, true);
				if (listener != null) {
					listener.OnConnectionAttempt(attempt, connectedAttempt);
				}
				if (connectedAttempt) {
					connectionLost = false;
				} else {
					sleepForAWhile(getDelay());
				}
			}
		}
		return connectedAttempt;
	}

	/**
	 * Test connection to the server with last login and password.
	 *
	 * @param manageUser
	 *            if true retrieve current user profile.
	 * @return True if connection parameters are correct.
	 */
	public boolean connect(boolean manageUser) {
		// FIXME Quel est le bon login/password ????
		return (server != null) && connect(server.getLastLogin(), server.getLastPassword(), manageUser);
	}

	/**
	 * Test connection to the server with last login and password.
	 *
	 * @param manageUser
	 *            if true retrieve current user profile.
	 * @param localtranslate
	 *            if true, messages from server will be not be translated remotely
	 * @return True if connection parameters are correct.
	 */
	public boolean connect(boolean manageUser, boolean localtranslate) {
		// FIXME Quel est le bon login/password ????
		return (server != null) && connect(server.getLastLogin(), server.getLastPassword(), manageUser, localtranslate);
	}

	/**
	 * Test connection to the server with given login and password.
	 *
	 * @param login
	 * @param password
	 * @param manageUser
	 *            if true retrieve current user profile.
	 * @return True if the "connection" succeed !
	 */
	public boolean connect(String login, String password, boolean managerUser) {
		return connect(login, password, managerUser, false);
	}

	/**
	 * Request connection. Get Non-translated messages
	 *
	 * @param login
	 * @param password
	 * @param managerUser
	 * @param localtranslate
	 *            if true, server will return non-translated message
	 * @return True if the "connection" succeed !
	 */
	public boolean connect(String login, String password, boolean managerUser, boolean localtranslate) {
		try {
			if (new URL(server.getUrl()).getProtocol().equals("https")) { //$NON-NLS-1$
				return connectWithCertificats(login, password, managerUser, localtranslate);
			}
			return connectWithoutCertificats(login, password, managerUser, localtranslate);
		} catch (final MalformedURLException e) {
			if (messageManager != null) {
				errorMessage = messageManager.getMessage(IACCMessages.ERR_CONNECTION_INVALID_URL, server.getUrl());
			}
			// FIXME si pas de messageManager l'erreur n'est jamais remonté !
			return false;
		}
	}

	/**
	 * Request connection. Get Non-translated messages.
	 *
	 * @param login
	 * @param password
	 * @param managerUser
	 * @param localtranslate
	 *            if true, server will return non-translated message
	 * @return
	 */
	public boolean connectWithoutCertificats(String login, String password, boolean managerUser,
			boolean localtranslate) {
		if (server != null) {
			final DataAccess da = new DataAccess(server.getUrl(), login, password.toCharArray());
			da.getWebServicesAccess().setXStream(new XmlBeanMapStream());
			setProxy(da.getWebServicesAccess());
			try {
				callCurrentUser(da, managerUser, localtranslate);
				// FIXME Not thread safe !
				this.login = login;
				this.password = password;
				connected = true;
				dataAccess = da; // on ne change pas l'éventuel DataAccess par un autre qui ne fonctionne pas.
				return true;
			} catch (final ServerErrorException e) {
				manageErrorException(e);
			}
		}
		return false;
	}

	private void setProxy(WebServiceAccess wsa) {
		if ((BaseActivator.getDefault() == null) || (BaseActivator.getDefault().getBundle() == null)) {
			return;
		}
		final BundleContext bc = BaseActivator.getDefault().getBundle().getBundleContext();
		if (bc == null) {
			return;
		}
		final ServiceReference<IProxyService> sr = bc.getServiceReference(IProxyService.class);
		if (sr == null) {
			return;
		}
		final IProxyService proxyService = bc.getService(sr);
		if ((proxyService == null) || !proxyService.isProxiesEnabled()) {
			return;
		}
		try {
			// Eclipse already configure the java system properties for ProxyHost and ProxyPort.
			// We only have to set the proxy login and password, if any.
			final IProxyData[] proxyDataForHost = proxyService.select(new URI(server.getUrl()));
			if (proxyDataForHost != null) {
				String proxytype;
				if (server.getUrl().trim().toLowerCase().startsWith("https")) { //$NON-NLS-1$
					proxytype = IProxyData.HTTPS_PROXY_TYPE;
				} else {
					proxytype = IProxyData.HTTP_PROXY_TYPE;
				}
				for (final IProxyData pd : proxyDataForHost) {
					if (proxytype.equals(pd.getType())) {
						char[] pwd = null;
						if (pd.getPassword() != null) {
							pwd = pd.getPassword().toCharArray();
						}
						Authenticator.setDefault(ProxyAuthenticator.getInstance());
						wsa.setProxy(null, 0, pd.getUserId(), pwd);
						return;
					}
				}
			}
		} catch (final URISyntaxException e) {
			errorMessage = new UserMessage("ProxyError", "Incorrect server URL: " + server.getUrl(),
					e.getLocalizedMessage());
		}
	}

	public ITrustStoreProvider getTrustStoreProvider() {
		return trustStoreprovider;
	}

	public void setTrustStoreprovider(ITrustStoreProvider trustStoreprovider) {
		this.trustStoreprovider = trustStoreprovider;
	}

	/**
	 * Request connection. Get Non-translated messages
	 *
	 * @param login
	 * @param password
	 * @param managerUser
	 * @param localtranslate
	 *            if true, server will return non-translated message
	 * @return
	 */
	public boolean connectWithCertificats(String login, String password, boolean managerUser, boolean localtranslate) {

		if (server != null) {
			final ITrustStoreProvider trustStoreProvider = getTrustStoreProvider();
			if (trustStoreProvider != null) {
				final String trustStorePath = trustStoreProvider.getTrustStorePath();
				final char[] trustStorePassword = trustStoreProvider.getTrustStorePassword();
				final String keyStorePath = trustStoreProvider.getKeyStorePath();
				final char[] keyStorePassword = trustStoreProvider.getKeyStorePassword();

				final RestConnectionParameters parameters = new RestConnectionParameters(BaseActivator.getDefault());
				parameters.setTrustStore(trustStorePath, trustStorePassword, trustStoreProvider.getTrustStoreType(),
						trustStoreProvider.getTrustManagerAlgorithm());
				parameters.setKeyStore(keyStorePath, keyStorePassword, trustStoreProvider.getKeyPassword(),
						trustStoreProvider.getKeyStoreType(), trustStoreProvider.getKeyManagerAlgorithm());
				parameters.setDisabledCipherSuites(trustStoreProvider.getDisabledCipherSuites());
				parameters.setDisabledProtocols(trustStoreProvider.getDisabledProtocols());
				parameters.setEnabledCipherSuites(trustStoreProvider.getEnabledCipherSuites());
				parameters.setEnabledProtocols(trustStoreProvider.getEnabledProtocols());
				parameters.setProtocol(trustStoreProvider.getProtocol());
				parameters.setSecureRandomAlgorithm(trustStoreProvider.getSecureRandomAlgorithm());

				final WebServiceAccess webServiceAccess = new WebServiceAccess(BaseActivator.getDefault(), parameters);
				// FIXME Login et Password ne sont pas affecté aux champ de cette classe !!!!!!
				webServiceAccess.setLogin(login);
				webServiceAccess.setPassword(password.toCharArray());
				webServiceAccess.setDefaultServeraddress(server.getUrl());
				setProxy(webServiceAccess);
				final DataAccess da = new DataAccess(webServiceAccess);
				da.getWebServicesAccess().setXStream(new XmlBeanMapStream());
				try {
					callCurrentUser(da, managerUser, localtranslate);
					// [ML] login and password was not stored here...
					this.login = login;
					this.password = password;
					connected = true;
					// FIXME Not thread safe !
					dataAccess = da;
					return true;
				} catch (final ServerErrorException e) {
					manageErrorException(e);
				}
			} else {
				manageError(IACCMessages.ERR_CONNECTION_INVALID_TRUSTSTORE);
			}
		}
		return false;
	}

	/**
	 * Reload the information relative to the current user.
	 * <p>
	 * This is much more better to call this method than any other after a modification of the current user !
	 *
	 * @param localtranslate
	 *            if true, server will return non-translated message
	 * @throws ServerErrorException
	 *             When there is an HTTP Error with the server, this is the caller responsibility to manage this error
	 *             and give to the user the feedback he/she require.
	 */
	public void updateCurrentUser(boolean localtranslate) throws ServerErrorException {
		callCurrentUser(dataAccess, true, localtranslate);
	}

	private void callCurrentUser(DataAccess da, boolean manageUser, boolean localtranslate)
			throws ServerErrorException {
		if (da != null) {
			final Map<String, Object> params = new HashMap<>();
			if (localtranslate) {
				params.put(HTTPPARAMETER_CLIENT_TRANSLATION, localtranslate);
			}
			if (manageUser) {
				user = (ConnectionUserBean) new XmlCurrentUserStream().fromXML( //
						da.getWebServicesAccess().get("/currentuser", true, false, params)); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Get the server DataAccess interface.
	 * <p>
	 * Valid only after a call to <code>connect</code> method.
	 *
	 * @return null if not connected.
	 * @see #connect(boolean)
	 * @see #connect(String, String, boolean)
	 */
	public DataAccess getDataAccess() {
		return dataAccess;
	}

	public boolean executeAction(IServerRunnableAction action) {
		try {
			return action.run();
		} catch (final ServerErrorException e) {
			manageErrorException(e);
		}
		return false;
	}

	public String getLogin() {
		return login;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public boolean isAllowed(List<Integer> expectedRights) {
		if (expectedRights != null) {
			for (final Integer right : expectedRights) {
				if ((right == -1) || !user.getProfile().hasRight(right)) {
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * Test if the current connected user possess all the given access rigths.
	 *
	 * @param expectedRights
	 * @return
	 */
	public boolean isAllowed(int... expectedRights) {
		if (!connected || (user == null) || (user.getProfile() == null)) {
			return false;
		}
		for (final int right : expectedRights) {
			if ((right == -1) || !user.getProfile().hasRight(right)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * @return false
	 * @deprecated DO NOT USE THIS METHOD.
	 */
	@Deprecated
	public boolean testAbout() {
		return false;
	}
}
