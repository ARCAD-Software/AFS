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
package com.arcadsoftware.restful.connection.ldap;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import javax.net.SocketFactory;

import org.restlet.Request;
import org.restlet.data.ChallengeResponse;

import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IBasicAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.unboundid.asn1.ASN1OctetString;
import com.unboundid.ldap.sdk.BindRequest;
import com.unboundid.ldap.sdk.BindResult;
import com.unboundid.ldap.sdk.CRAMMD5BindRequest;
import com.unboundid.ldap.sdk.Control;
import com.unboundid.ldap.sdk.DIGESTMD5BindRequest;
import com.unboundid.ldap.sdk.DereferencePolicy;
import com.unboundid.ldap.sdk.EXTERNALBindRequest;
import com.unboundid.ldap.sdk.FailoverServerSet;
import com.unboundid.ldap.sdk.Filter;
import com.unboundid.ldap.sdk.GSSAPIBindRequest;
import com.unboundid.ldap.sdk.GSSAPIBindRequestProperties;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPConnectionOptions;
import com.unboundid.ldap.sdk.LDAPConnectionPool;
import com.unboundid.ldap.sdk.LDAPException;
import com.unboundid.ldap.sdk.Modification;
import com.unboundid.ldap.sdk.ModificationType;
import com.unboundid.ldap.sdk.ModifyRequest;
import com.unboundid.ldap.sdk.PLAINBindRequest;
import com.unboundid.ldap.sdk.ResultCode;
import com.unboundid.ldap.sdk.SCRAMSHA1BindRequest;
import com.unboundid.ldap.sdk.SCRAMSHA256BindRequest;
import com.unboundid.ldap.sdk.SCRAMSHA512BindRequest;
import com.unboundid.ldap.sdk.SearchRequest;
import com.unboundid.ldap.sdk.SearchResult;
import com.unboundid.ldap.sdk.SearchResultEntry;
import com.unboundid.ldap.sdk.SearchScope;
import com.unboundid.ldap.sdk.SimpleBindRequest;
import com.unboundid.ldap.sdk.StartTLSPostConnectProcessor;
import com.unboundid.ldap.sdk.controls.PasswordExpiredControl;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.crypt.ConfiguredSSLContext;
import com.arcadsoftware.crypt.ConfiguredSSLContextException;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;

/* TODO identifier AD !  https://ldapwiki.com/wiki/Determine%20LDAP%20Server%20Vendor
 */
public class LdapAuthentificationService implements IBasicAuthentificationService, Closeable {

	private static final String PROP_HOST = "host"; //$NON-NLS-1$
	private static final String PROP_PORT = "port"; //$NON-NLS-1$
	private static final String PROP_SERVER = "server"; //$NON-NLS-1$
	private static final String PROP_SERVERS = "servers"; //$NON-NLS-1$
	private static final String PROP_REORDERFAILOVERLIST = "reorderfailoverlist"; //$NON-NLS-1$
	private static final String PROP_POOLINITSIZE = "connection.pool.initial.size"; //$NON-NLS-1$
	private static final String PROP_POOLSIZE = "connection.pool.max.size"; //$NON-NLS-1$
	private static final String PROP_POOLAVAILABLEGOAL = "connection.pool.available.goal"; //$NON-NLS-1$
	private static final String PROP_CONNECTIONMAXAGE = "connection.pool.max.age"; //$NON-NLS-1$
	private static final String PROP_CONNECTIONFAILOVERMAXAGE = "connection.failover.max.age"; //$NON-NLS-1$
	private static final String PROP_TIMEOUT = "timeout"; //$NON-NLS-1$
	private static final String PROP_BINDTYPE = "bind.type"; //$NON-NLS-1$
	protected static final String PROP_PWDATTRIBUTE = "attribute.password"; //$NON-NLS-1$
	protected static final String PROP_LOGINATTRIBUTE = "attribute.login"; //$NON-NLS-1$
	protected static final String PROP_DNLOGIN = "dn.login.pattern"; //$NON-NLS-1$
	protected static final String PROP_SEARCHUSER = "search.user.dn"; //$NON-NLS-1$
	protected static final String PROP_SEARCHPWD = "search.user.password"; //$NON-NLS-1$
	private static final String PROP_SEARCHSSO = "search.user.sso"; //$NON-NLS-1$
	private static final String PROP_DNBASE = "dn.base"; //$NON-NLS-1$
	private static final String PROP_MAP = "ldapmap."; //$NON-NLS-1$
	private static final String PROP_USEPOLICYHINTS = "use.policy.hints"; //$NON-NLS-1$
	private static final String PROP_CANCHANGEPWD = "password.modify"; //$NON-NLS-1$
	private static final String PROP_USERIMPORT_ENABLED = "userimportenabled"; //$NON-NLS-1$
	private static final String PROP_BUSYRETRY = "busy.retry"; //$NON-NLS-1$
	private static final String PROP_REALM = "realm"; //$NON-NLS-1$
	private static final String PROP_KDCADDRESS = "kerberos.kdc"; //$NON-NLS-1$
	private static final String PROP_USERAUTOIMPORT = "autoimport.enabled"; //$NON-NLS-1$
	private static final String PROP_USERAUTOIMPORTPROFILE = "autoimport.profile"; //$NON-NLS-1$
	private static final int BINDKIND_SIMPLE = 1;
	private static final int BINDTYPE_CRAMMD5 = 2;
	private static final int BINDTYPE_DIGESTMD5 = 3;
	private static final int BINDTYPE_PLAIN = 4;
	private static final int BINDTYPE_SCRAMSHA1 = 5;
	private static final int BINDTYPE_SCRAMSHA256 = 6;
	private static final int BINDTYPE_SCRAMSHA512 = 7;
	private static final int BINDTYPE_GSSAPI = 8;
	private static final Control POLICYHINTSCONTROL = new Control("1.2.840.113556.1.4.2066", false, //$NON-NLS-1$
			new ASN1OctetString(new byte[] { 48, (byte) 132, 0, 0, 0, 3, 2, 1, 1 }));

	/**
	 * Return true if the configuration contain the minimal set of option to work correctly.
	 * 
	 * @param props
	 * @return
	 */
	public static boolean isConfigurationComplete(Dictionary<String, Object> props) {
		return (!getProp(props, PROP_HOST, "").trim().isEmpty() || //$NON-NLS-1$
				!getProp(props, PROP_SERVER, "").trim().isEmpty() || //$NON-NLS-1$
				!getProp(props, PROP_SERVERS, "").trim().isEmpty()) && //$NON-NLS-1$
				!getProp(props, PROP_DNBASE, "").trim().isEmpty() && //$NON-NLS-1$
				(!getProp(props, PROP_LOGINATTRIBUTE, "").trim().isEmpty() || //$NON-NLS-1$
				 !getProp(props, PROP_DNLOGIN, "").trim().isEmpty()); //$NON-NLS-1$
	}

	protected static String getADErrorCode(String detail) {
		if ((detail != null) && !detail.isEmpty()) {
			int i = detail.indexOf(", data "); //$NON-NLS-1$
			if (i > 0) {
				i += 7;
				int j = detail.indexOf(',', i);
				if (j > i) {
					return detail.substring(i, j).trim();
				}
			}
		}
		return null;
	}

	private static String getProp(Dictionary<String, Object> props, String name, String defValue) {
		Object o = props.get(name);
		if (o != null) {
			return o.toString().trim();
		}
		return defValue;
	}

	private static Integer getProp(Dictionary<String, Object> props, String name, int defValue) {
		Object o = props.get(name);
		if (o instanceof Integer) {
			return (Integer) o;
		}
		if (o != null) {
			try {
				return Integer.valueOf(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		return defValue;
	}

	private static Long getProp(Dictionary<String, Object> props, String name, long defValue) {
		Object o = props.get(name);
		if (o instanceof Long) {
			return (Long) o;
		}
		if (o != null) {
			try {
				return Long.valueOf(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		return defValue;
	}

	private static Boolean getProp(Dictionary<String, Object> props, String name, boolean defValue) {
		Object o = props.get(name);
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		if (o != null) {
			try {
				return Boolean.valueOf(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		return defValue;
	}

	private final Activator activator;
	private final LDAPConnectionPool connectionPool;
	private final int bindKind;
	private final String loginPattern;
	private final String loginAttribute;
	private final boolean canChangePWD;
	private final boolean userImportEnabled;
	private final String passwordAttribute;
	private final String base;
	private final HashMap<String, List<String>> userMaps = new HashMap<String, List<String>>();
	private final boolean usePolicyHints;
	private final boolean alreadybinded;
	private final int busy;
	private final String realm;
	private final String kdc;
	private final boolean autoImport;
	private final OrCriteria autoImportProfiles;

	public LdapAuthentificationService(Activator activator, Dictionary<String, Object> props)
			throws ConfiguredSSLContextException, LDAPException {
		super();
		this.activator = activator;
		base = getProp(props, PROP_DNBASE, null);
		if (base == null) { // Blindage !
			throw new NullPointerException("The property configuration \"dn.base\" must be set.");
		}
		int initialConnections = getProp(props, PROP_POOLINITSIZE, 1);
		if (initialConnections < 1) {
			initialConnections = 1;
		}
		int maxConnections = getProp(props, PROP_POOLSIZE, initialConnections);
		if (maxConnections < initialConnections) {
			maxConnections = initialConnections;
		}
		int minAvailableGoal = getProp(props, PROP_POOLAVAILABLEGOAL, initialConnections);
		if (minAvailableGoal > maxConnections) {
			if (maxConnections <= 1) {
				minAvailableGoal = 0;
			} else {
				minAvailableGoal = maxConnections;
			}
		}
		passwordAttribute = getProp(props, PROP_PWDATTRIBUTE, "userPassword"); //$NON-NLS-1$
		loginPattern = getProp(props, PROP_DNLOGIN, null);
		loginAttribute = getProp(props, PROP_LOGINATTRIBUTE, null);
		usePolicyHints = getProp(props, PROP_USEPOLICYHINTS, false);
		canChangePWD = getProp(props, PROP_CANCHANGEPWD, false);
		autoImport = getProp(props, PROP_USERAUTOIMPORT, false);
		kdc = getProp(props, PROP_KDCADDRESS, (String) null);
		realm = getProp(props, PROP_REALM, (String) null);
		userImportEnabled = (getProp(props, PROP_USERIMPORT_ENABLED, false)) && (loginAttribute != null) && (base != null);
		autoImportProfiles = new OrCriteria();
		for (String p: getProp(props, PROP_USERAUTOIMPORTPROFILE, "").split(" ")) { //$NON-NLS-1$ //$NON-NLS-2$
			if ((p != null) && !p.isEmpty()) {
				if ((p.charAt(0) == '"') && (p.charAt(p.length() - 1) == '"')) {
					autoImportProfiles.add(new EqualCriteria("code", p.substring(1, p.length() - 1))); //$NON-NLS-1$
				} else {
					try {
						autoImportProfiles.add(new IdEqualCriteria(Integer.parseInt(p)));
					} catch (NumberFormatException e) {
						autoImportProfiles.add(new EqualCriteria("code", p)); //$NON-NLS-1$
					}
				}
			}
		}
		bindKind = getBindKind(getProp(props, PROP_BINDTYPE, "direct"));
		Enumeration<String> keys = props.keys();
		while (keys.hasMoreElements()) {
			String key = keys.nextElement();
			if ((key != null) && key.startsWith(PROP_MAP)) {
				addUserMap(userMaps, key.substring(PROP_MAP.length()), (String) props.get(key));
			}
		}
		int i = getProp(props, PROP_BUSYRETRY, 1);
		if (i <= 0) {
			busy = 0;
		} else if (i > 10) {
			activator.warn("The Property \"bury.rety\" is too high. It is limited to 10 retries.");
			busy = 10;
		} else {
			busy = i;
		}
		final ConfiguredSSLContext sslConf = new ConfiguredSSLContext(props);
		StartTLSPostConnectProcessor startTLSProcessor = null;
		if (sslConf.isStartTLS()) {
			startTLSProcessor = new StartTLSPostConnectProcessor(sslConf.getContext());
		}
		BindRequest bindRequest = null;
		final String user = getProp(props, PROP_SEARCHUSER, "").trim();
		final String pwd = getProp(props, PROP_SEARCHPWD, "").trim();
		final String sso = getProp(props, PROP_SEARCHSSO, "").trim();
		if (!user.isEmpty() && !pwd.isEmpty()) {
			activator.warn("LDAP connection using a permanent user binded to the server (" + user + ") this require a permanent synchronization and aviability of this user and may lead to security problem. Consult documentation to switch to another connection mode.");
			if (sso.equalsIgnoreCase("kerberos")) {
				bindRequest = getBindRequest(BINDTYPE_GSSAPI, user, Crypto.decrypt(pwd));
			} else {
				bindRequest = getBindRequest(user, Crypto.decrypt(pwd));
			}
			alreadybinded = true;
		} else if (sso.equalsIgnoreCase("kerberos")) {
			// Bind with an active Kerberos authentication.
			if (user.isEmpty()) {
				bindRequest = getBindRequest(BINDTYPE_GSSAPI, null, null);
			} else {
				bindRequest = getBindRequest(BINDTYPE_GSSAPI, user, null);
			}
			alreadybinded = true;
		} else if (sso.equalsIgnoreCase("external")) {
			if (user.isEmpty()) {
				bindRequest = new EXTERNALBindRequest();
			} else {
				bindRequest = new EXTERNALBindRequest(user);
			}
			alreadybinded = true;
		} else {
			alreadybinded = false;
		}
		connectionPool = new LDAPConnectionPool(getServerSet(props, sslConf, bindRequest), bindRequest , initialConnections, maxConnections, startTLSProcessor, false);
		connectionPool.setConnectionPoolName("AFS: REST Web-service Authentication"); //$NON-NLS-1$
		connectionPool.setMaxConnectionAgeMillis(getProp(props, PROP_CONNECTIONMAXAGE, 120000L));
		connectionPool.setMaxDefunctReplacementConnectionAgeMillis(getProp(props, PROP_CONNECTIONFAILOVERMAXAGE, 60000L));
		connectionPool.setMaxWaitTimeMillis(getProp(props, PROP_TIMEOUT, 5000L));
		connectionPool.setRetryFailedOperationsDueToInvalidConnections(true);
		connectionPool.setHealthCheckIntervalMillis(5000L);
		connectionPool.setMinimumAvailableConnectionGoal(minAvailableGoal);
		connectionPool.setCreateIfNecessary(true);
	}

	private int getBindKind(String bk) {
		if (bk == null) {
			return BINDKIND_SIMPLE;
		}
		switch (bk.toLowerCase()) { //$NON-NLS-1$
		case "1": //$NON-NLS-1$
		case "s": //$NON-NLS-1$
		case "smpl": //$NON-NLS-1$
		case "simple": //$NON-NLS-1$
			return BINDKIND_SIMPLE;
		case "2": //$NON-NLS-1$
		case "plain": //$NON-NLS-1$
			return BINDTYPE_PLAIN;
		case "3": //$NON-NLS-1$
		case "dmd5": //$NON-NLS-1$
		case "digest-md5": //$NON-NLS-1$
			return BINDTYPE_DIGESTMD5;
		case "4": //$NON-NLS-1$
		case "cmd5": //$NON-NLS-1$
		case "cram-md5": //$NON-NLS-1$
			return BINDTYPE_CRAMMD5;
		case "5": //$NON-NLS-1$
		case "ssha1": //$NON-NLS-1$
		case "scram-sha-1": //$NON-NLS-1$
			return BINDTYPE_SCRAMSHA1;
		case "6": //$NON-NLS-1$
		case "ssha256": //$NON-NLS-1$
		case "scram-sha-256": //$NON-NLS-1$
			return BINDTYPE_SCRAMSHA256;
		case "7": //$NON-NLS-1$
		case "ssha512": //$NON-NLS-1$
		case "scram-sha-512": //$NON-NLS-1$
			return BINDTYPE_SCRAMSHA512;
		case "8": //$NON-NLS-1$
		case "gssapi": //$NON-NLS-1$
		case "kerberos": //$NON-NLS-1$
			return BINDTYPE_GSSAPI;
		default: // direct
			return BINDKIND_SIMPLE;
		}
	}

	private FailoverServerSet getServerSet(final Dictionary<String, Object> props, final ConfiguredSSLContext sslConf, BindRequest bindRequest) {
		SocketFactory sslSocketFactory = sslConf.getSocketFactory();
		Integer defPort;
		if ((sslSocketFactory != null) && !sslConf.isStartTLS()) {
			defPort = 636;
		} else {
			sslSocketFactory = null; // StartTLS is configured on a non secure socket...
			defPort = 389;
		}
		defPort = getProp(props, PROP_PORT, defPort);
		final ArrayList<Integer> ports = new ArrayList<Integer>();
		final ArrayList<String> servers = new ArrayList<String>();
		String s = getProp(props, PROP_HOST, null);
		if ((s != null) && !s.isEmpty()) {
			servers.add(s);
			ports.add(defPort);
		}
		s = getProp(props, PROP_SERVER, null);
		if ((s != null) && !s.isEmpty()) {
			int i = s.indexOf(':');
			if (i <= 0) {
				servers.add(s);
				ports.add(defPort);
			} else {
				servers.add(s.substring(0, i).trim());
				try {
					ports.add(Integer.valueOf(s.substring(i + 1).trim()));
				} catch (NumberFormatException e) {
					ports.add(defPort);
				}
			}
		}
		s = getProp(props, PROP_SERVERS, null);
		if ((s != null) && !s.isEmpty()) {
			for (String ss : s.trim().split(" ")) { //$NON-NLS-1$
				if (!ss.isEmpty()) {
					int i = ss.indexOf(':');
					if (i <= 0) {
						servers.add(ss);
						ports.add(defPort);
					} else {
						servers.add(ss.substring(0, i).trim());
						try {
							ports.add(Integer.valueOf(ss.substring(i + 1).trim()));
						} catch (NumberFormatException e) {
							ports.add(defPort);
						}
					}
				}
			}
		}
		String[] svrs = new String[servers.size()];
		int[] prts = new int[servers.size()];
		for (int i = 0; i < svrs.length; i++) {
			svrs[i] = servers.get(i);
			prts[i] = ports.get(i);
		}
		// Add the knowns server names to the SSL context verifier.
		if (sslConf.isVerifyHostname()) {
			sslConf.setPeerHostnames(svrs);
		}
		// TODO Use other kind of Servers set: https://docs.ldap.com/ldap-sdk/docs/getting-started/failover-load-balancing.html
		// TODO RoundRobinServerSet
		// TODO FastestConnectServerSet
		// TODO RoundRobinDNSServerSet
		// TODO DNSSRVRecordServerSet (in that case the list of DNS/IP address become optional !)
		FailoverServerSet serverSet = new FailoverServerSet(svrs, prts, sslSocketFactory, getConnectionOptions(props), bindRequest, null);
		serverSet.setReOrderOnFailover(getProp(props, PROP_REORDERFAILOVERLIST, false));
		return serverSet;
	}

	private LDAPConnectionOptions getConnectionOptions(Dictionary<String, Object> props) {
		LDAPConnectionOptions ldapOptions = new LDAPConnectionOptions();
		ldapOptions.setAbandonOnTimeout(true);
		ldapOptions.setBindWithDNRequiresPassword(false);
		ldapOptions.setResponseTimeoutMillis(60000);
		ldapOptions.setUsePooledSchema(true);
		ldapOptions.setPooledSchemaTimeoutMillis(60000);
		ldapOptions.setUseSynchronousMode(true);
		return ldapOptions;
	}

	public boolean isLoginCaseSensitive() {
		return activator.isLoginCaseSensitive();
	}

	@Override
	public void close() {
		connectionPool.close();
	}

	@Override
	public List<String> getUserLogins(String userType, int userId) {
		ArrayList<String> result = new ArrayList<String>();
		if ("user".equals(userType)) {
			BeanMap auth = activator.getAuth(userId);
			if (auth != null) {
				result.add(auth.getString(Activator.LDAPAUTH_LOGIN));
			}
		}
		return result;
	}

	@Override
	public IConnectionCredential generateCredential(Request request, String identifier) {
		int id = activator.getAuth(identifier);
		if (id > 0) {
			return new LdapConnectionCredential(this, identifier, id);
		}
		if (autoImport) {
			id = autoImportUser(identifier, request.getChallengeResponse());
			if (id > 0) {
				return new LdapConnectionCredential(this, identifier, id);
			}
		} else if (ConnectionUserBean.STANDALONECONNECTIONS != null) {
			return new LdapConnectionCredential(this, identifier, -1);
		}
		return null;
	}

	protected LDAPConnection getConnection() {
		try {
			return connectionPool.getConnection();
		} catch (LDAPException e) {
			activator.error("There is an error when asking for a connection in the LDAP connection pool (Check your configuration or the LDAP Server may be down): "+ e.getLocalizedMessage(), e);
			activator.info("LDAP Diagnostic Message: " + e.getDiagnosticMessage());
			return null;
		}
	}

	protected void closeConnection(LDAPConnection connection, LDAPException error) {
		if (error != null) {
			connectionPool.releaseConnectionAfterException(connection, error);
		} else if (alreadybinded) {
			connectionPool.releaseAndReAuthenticateConnection(connection);
		} else {
			connectionPool.releaseConnection(connection);
		}
	}
	
	private BindRequest getBindRequest(String userdn, char[] secret) throws LDAPException {
		return getBindRequest(bindKind, userdn, secret);
	}
	
	private BindRequest getBindRequest(int bt, String userdn, char[] secret) throws LDAPException {
		switch (bt) {
		case BINDTYPE_CRAMMD5:
			return new CRAMMD5BindRequest(userdn, new String(secret));
		case BINDTYPE_DIGESTMD5:
			// TODO Use Alternate authorization ID ! (SASL)
			// TODO Use the "realm" parameter, in configuration or through aunthentication parameters !
			return new DIGESTMD5BindRequest(userdn, null, new String(secret), realm);
		case BINDTYPE_PLAIN:
			return new PLAINBindRequest(userdn, new String(secret));
		case BINDTYPE_SCRAMSHA1:
			return new SCRAMSHA1BindRequest(userdn, new String(secret));
		case BINDTYPE_SCRAMSHA256:
			return new SCRAMSHA256BindRequest(userdn, new String(secret));
		case BINDTYPE_SCRAMSHA512:
			return new SCRAMSHA512BindRequest(userdn, new String(secret));
		case BINDTYPE_GSSAPI:
			GSSAPIBindRequestProperties props = new GSSAPIBindRequestProperties(userdn, new String(secret));
			if (kdc != null) {
				props.setKDCAddress(kdc);
			}
			if (realm != null) {
				props.setRealm(realm);
			}
			return new GSSAPIBindRequest(props);
		default:
			return new SimpleBindRequest(userdn, new String(secret));
		}
	}
	
	protected BindResult bind(final LDAPConnection cn, String userlogin, char[] secret) throws LDAPException {
		final String dn = getUserDN(cn, userlogin);
		if ((dn == null) || (cn == null)) {
			return null;
		}
		final BindRequest br = getBindRequest(dn, secret);
		if (br == null) {
			return null;
		}
		try {
			return cn.bind(br);
		} catch (LDAPException e) {
			if (e.getResultCode() == ResultCode.BUSY) {
				try {
					for (int i = busy; i > 0; i--) { 
						Thread.sleep(2000);
						try {
							return cn.bind(br);
						} catch (LDAPException ee) {
							if (ee.getResultCode() != ResultCode.BUSY) {
								e = ee;
								break;
							}
						}
					}
				} catch (InterruptedException ie) {
					// ok, we end here...
				}
			}
			throw e;
		}
	}

	/**
	 * Get an acceptable Distinguished Name (DN) to be able to bind the given user login.
	 * 
	 * @param cn This connection must be already binded, Thi connection is used only if the "loginPattern" is not used and the "alreadybinded" flag is on. 
	 * @param login A non null user login.
	 * @return null if there is not DN corresponding to this user on the LDAP server.
	 * @throws LDAPException
	 */
	public String getUserDN(LDAPConnection cn, String login) throws LDAPException {
		if (alreadybinded && (loginAttribute != null) && ((loginPattern == null) || loginPattern.isEmpty())) {
			// Try to use the permanent connection to find the real user DN.
			// We must be sure that the current connection is currently binded with the default "admin" user !
			SearchResult sr = cn.search(new SearchRequest(null, base, SearchScope.SUB, DereferencePolicy.ALWAYS, 3, 0, false, 
					Filter.create('(' + loginAttribute + '=' + login + ')'), SearchRequest.ALL_USER_ATTRIBUTES));
			if (sr.getEntryCount() == 0) {
				return null;
			}
			if (sr.getEntryCount() > 1) {
				activator.warn("LDAP Search of user authenticated by \"" + login + "\" returned multiple results onto the LDAP server.");
			}
			return sr.getSearchEntries().get(0).getDN();
		}
		if (loginPattern == null) {
			if ((loginAttribute == null) || (base == null)) {
				return login;
			}
			return loginAttribute + '=' + login + ',' + base;
		}
		if (loginPattern.isEmpty()) {
			return login;
		}
		if (loginPattern.contains("%s") || loginPattern.contains("%S")) {
			return String.format(loginPattern, login);
		}
		if (base == null) {
			return loginPattern + '=' + login;
		}
		return loginPattern + '=' + login + ',' + base;
	}

	public String changePWD(LDAPConnection cn, String dn, char[] oldPassword, char[] newPassword) throws LDAPException {
		byte[] oldPass = null;
		byte[] newPass;
		if ("unicodepwd".equalsIgnoreCase(passwordAttribute)) { //$NON-NLS-1$
			if (oldPassword != null) {
				try {
					oldPass = ('"' + new String(oldPassword) + '"').getBytes("UTF-16LE"); //$NON-NLS-1$
				} catch (UnsupportedEncodingException e) {
					activator.error(e);
					return "The Java VM is unable to generate an UTF-16LE encoding for Unicode Password.";
				}
			}
			try {
				newPass = ('"' + new String(newPassword) + '"').getBytes("UTF-16LE"); //$NON-NLS-1$
			} catch (UnsupportedEncodingException e) {
				activator.error(e);
				return "The Java VM is unable to generate an UTF-16LE encoding for Unicode Password.";
			}
		} else {
			if (oldPassword != null) {
				oldPass = Crypto.getBytes(oldPassword);
			}
			newPass = Crypto.getBytes(newPassword);
		}
		final ArrayList<Modification> modifications = new ArrayList<Modification>();
		if (oldPassword != null) {
			modifications.add(new Modification(ModificationType.DELETE, passwordAttribute, oldPass));
			modifications.add(new Modification(ModificationType.ADD, passwordAttribute, newPass));
		} else {
			modifications.add(new Modification(ModificationType.REPLACE, passwordAttribute, newPass));
		}
		ModifyRequest modifyRequest = new ModifyRequest(dn, modifications);
		if (usePolicyHints) {
			modifyRequest.addControl(POLICYHINTSCONTROL);
		}
		cn.modify(modifyRequest);
		return ""; // = Success //$NON-NLS-1$
	}

	public boolean isCanChangePWD() {
		return canChangePWD;
	}

	@Override
	public void purgeConnectionCache() {
		activator.cacheClear();
	}

	@Override
	public void purgeConnectionCache(int id) {
		// TODO Partial reload of the LDAP connections informations...
		activator.cacheClear();
	}

	private void addUserMap(Map<String, List<String>> map, String key, String value) {
		if ((key != null) && (value != null) && !value.isEmpty()) {
			int i = 0;
			final int e = value.length();
			List<String> values = map.get(key);
			if (values == null) {
				values = new ArrayList<String>();
				map.put(key, values);
			}
			StringBuilder sb = new StringBuilder();
			boolean quoted = false;
			while (i < e) {
				char ch = value.charAt(i);
				if (quoted) {
					if (ch == '"') {
						if ((i + 1 < e) && (value.charAt(i + 1) == '"')) {
							sb.append('"');
							i++;
						} else {
							if (sb.length() > 0) {
								values.add(sb.toString());
								sb = new StringBuilder();
							}
							quoted = false;
						}
					} else {
						sb.append(ch);
					}
				} else if (Character.isWhitespace(ch)) {
					if (sb.length() > 0) {
						values.add(sb.toString());
						sb = new StringBuilder();
					}
				} else if (ch == '"') {
					if (sb.length() > 0) {
						values.add(sb.toString());
						sb = new StringBuilder();
					}
					quoted = true;
				} else if ((ch == '-') && (sb.length() == 0)) {
					values.clear();
				} else {
					sb.append(ch);
				}
				i++;
			}
			if (sb.length() > 0) {
				values.add(sb.toString());
			}
		}
	}

	protected Activator getActivator() {
		return activator;
	}

	/* @see LdapAuthLoginRestlet
	 */
	protected boolean isLoginExists(LDAPConnection cn, String login) throws LDAPException {
		if ((loginAttribute == null) || loginAttribute.isEmpty() || (base == null)) {
			// Feature not configured...
			return false;
		}
		Filter filter = Filter.createEqualityFilter(loginAttribute, login);
		SearchResult searchResult = cn.search(new SearchRequest(base, SearchScope.SUB, filter, loginAttribute));
		return !searchResult.getSearchEntries().isEmpty();
	}
	
	protected boolean isUserImportEnabled() {
		return userImportEnabled;
	}

	private File getMapConfFile() {
		// TODO ajouter une configuration...
		return new File("./configuration/ldap.usermap.ini");
	}

	protected BeanMapList listSelectableUsers(LDAPConnection cn, String searchPattern, int countLimit, int timeLimit, Map<String, String> specialUserMap) throws LDAPException {
		// Load attributes mapping...
		HashMap<String, List<String>> map = getUserMapping();
		// Add parameter map...
		if (specialUserMap != null) {
			for (Entry<String, String> e : specialUserMap.entrySet()) {
				addUserMap(map, e.getKey(), e.getValue().trim());
			}
		}
		// If there is no user mapping then do not try to import something !
		if (map.isEmpty()) {
			return new BeanMapList();
		}
		// list users...
		Filter filter = Filter.create(searchPattern);
		// |ML] why if loginpatter= "%s" use ONE as scope ???
		final SearchScope scope = SearchScope.SUB;
		SearchResult sr = cn.search(new SearchRequest(null, base, scope, DereferencePolicy.ALWAYS, //
				countLimit, timeLimit, false, filter, SearchRequest.ALL_USER_ATTRIBUTES));
		BeanMapList result = new BeanMapList(sr.getEntryCount());
		int userID = 1;
		for (SearchResultEntry entry: sr.getSearchEntries()) {
			String login = entry.getAttributeValue(loginAttribute);
			// Ignore login already used.
			if ((login != null) && (activator.getAuth(login) == 0)) {
				BeanMap bm = new BeanMap(Activator.TYPE_USER, userID++);
				for(Entry<String, List<String>> e: map.entrySet()) {
					for(String att : e.getValue()) {
						// Add constants values (String, boolean or integer).
						if (att.charAt(0) == '"') { // Constant String Value
							bm.put(e.getKey(),att.substring(1, att.length() -1));
							break;
						} else if ("true".equalsIgnoreCase(att) || "false".equalsIgnoreCase(att)) { //$NON-NLS-1$ //$NON-NLS-2$
							bm.put(e.getKey(), "true".equalsIgnoreCase(att)); //$NON-NLS-1$
							break;
						} else {
							try {
								bm.put(e.getKey(),Integer.parseInt(att));
								break;
							} catch (NumberFormatException ee) {}
							String val = entry.getAttributeValue(att);
							if (val != null) {
								bm.put(e.getKey(), val);
								break;
							}
						}
					}
				}
				bm.put("ldap.login", login); //$NON-NLS-1$
				bm.put("ldapauth.login", login); //$NON-NLS-1$
				result.add(bm);
			}
		}
		return result;
	}

	private HashMap<String, List<String>> getUserMapping() {
		// Load attributes mapping...
		// Initialize with OSGi configuration...
		final HashMap<String, List<String>> map = new HashMap<String, List<String>>(userMaps);
		// Patch with User entity declaration...
		MetaDataEntity user = MetaDataEntity.loadEntity(Activator.TYPE_USER);
		if (user == null) {
			return map;
		}
		for (MetaDataAttribute a: user.getAttributes().values()) {
			String ldapa = a.getMetadata().getString("ldap"); //$NON-NLS-1$
			if (ldapa != null) {
				addUserMap(map, a.getCode(), ldapa.trim());
			}
		}
		// Patch with the new configuration file...
		File conf = getMapConfFile();
		if (conf.isFile()) {
			try (FileInputStream fis = new FileInputStream(conf)) {
				Properties props = new Properties();
				props.load(fis);
				for(Entry<Object, Object> e: props.entrySet()) {
					if ((e.getKey() != null) && (e.getValue() != null)) {
						addUserMap(map, e.getKey().toString(), e.getValue().toString().trim());
					}
				}
			} catch (IOException e) {
				activator.error("Error while loading User Entity Maping file: " + e.getLocalizedMessage(), e);
			}
		}
		return map;
	}

	protected boolean isAlreadybinded() {
		return alreadybinded;
	}

	private int autoImportUser(final String identifier, final ChallengeResponse challengeResponse) {
		synchronized (this) {
			if ((identifier == null) || (challengeResponse == null)) {
				return 0;
			}
			final char[] secret = challengeResponse.getSecret();
			LDAPConnection cn = getConnection();
			if (cn == null) {
				return 0;
			}
			LDAPException e = null;
			try {
				BindResult br = bind(cn, identifier, secret);
				if (br == null) {
					return 0;
				}
				// research the user information for importation.
				String dn = br.getMatchedDN();
				if (dn == null) {
					dn = getUserDN(cn, identifier);
					if (dn == null) {
						activator.error("Unable to auto-import the user \"" + identifier + "\". The LDAP congifuration is unable to identify the LDAP user only from his \"login\"."); 
						return 0;
					}
				}
				SearchResultEntry entry = cn.getEntry(dn);
				if (entry == null) {
					activator.error("Unable to auto-import the user \"" + identifier + "\". The user may not have the permission to read this own entry."); 
					return 0;
				}
				String login = entry.getAttributeValue(loginAttribute);
				// Ignore login already used... this should not append except in a concurrent access...
				if ((login == null) || (activator.getAuth(login) > 0)) {
					activator.info("User auto-import cancelled due to concurent operation..."); 
					return 0;
				}
				// Initialize User object.
				BeanMap user = new BeanMap(Activator.TYPE_USER);
				for (Entry<String, List<String>> m: getUserMapping().entrySet()) {
					for (String att : m.getValue()) {
						// Add constants values (String, boolean or integer).
						if (att.charAt(0) == '"') { // Constant String Value
							user.put(m.getKey(), att.substring(1, att.length() -1));
							break;
						}
						if ("true".equalsIgnoreCase(att) || "false".equalsIgnoreCase(att)) { //$NON-NLS-1$ //$NON-NLS-2$
							user.put(m.getKey(), "true".equalsIgnoreCase(att)); //$NON-NLS-1$
							break;
						}
						try {
							user.put(m.getKey(), Integer.parseInt(att));
							break;
						} catch (NumberFormatException ee) {}
						String val = entry.getAttributeValue(att);
						if (val != null) {
							user.put(m.getKey(), val);
							break;
						}
					}
				}
				if (user.isEmpty()) {
					activator.warn("Unable to auto-import the user \"" + identifier + "\". No attribute mapping found for this entry."); 
					return 0;
				}
				// Store the new user in the database...
				MetaDataEntity entityUser = MetaDataEntity.loadEntity(Activator.TYPE_USER);
				if (entityUser == null) {
					activator.info("User auto-import cancelled due to unexistant USER entity int he database..."); 
					return 0;
				}
				MetaDataEntity entityLdapauth = MetaDataEntity.loadEntity(Activator.LDAPAUTH);
				if (entityLdapauth == null) {
					activator.info("User auto-import cancelled due to unexistant LDAPAUTH entity int he database... Check your server configuration."); 
					return 0;
				}
				user = entityUser.dataCreate(user);
				// Associate the configured profiles...
				MetaDataEntity entityProfile = MetaDataEntity.loadEntity("profile"); //$NON-NLS-1$
				if ((entityProfile != null) && !autoImportProfiles.isEmpty()) {
					BeanMapList profiles = entityProfile.dataSelection(new ArrayList<ReferenceLine>(), false, autoImportProfiles, true, null, null, 0, -1);
					if ((profiles != null) && !profiles.isEmpty()) {
						for (BeanMap profile: profiles) {
							entityUser.dataLinkTo(user, "profiles", profile);
							// TODO add a method dataLinkTo(BeanMap, linkCode, ISearchCriteria) to create a multi-association in one operation.
						}
					}
				}
				// TODO Define a mapping between database profiles and LDAP groups and use the Entry linked groups to link the user to the corresponding profile.
				// Store the LDAP identifier...
				BeanMap auth =  new BeanMap(Activator.LDAPAUTH);
				auth.put(Activator.LDAPAUTH_USERID, user.getId());
				auth.put(Activator.LDAPAUTH_LOGIN, login);
				entityLdapauth.dataCreate(auth);
				// Update the cache...
				activator.setAuth(auth, null, 0);
				return user.getId();
			} catch (LDAPException ee) {
				e = ee;
				boolean locked = false;
				try {
					if (PasswordExpiredControl.get(e.toLDAPResult()) != null) {
						locked = true;
					}
				} catch (LDAPException e1) {
					activator.info(e1);
				}
				ResultCode rc = e.getResultCode();
				if (rc == ResultCode.INVALID_CREDENTIALS) {
					// https://ldapwiki.com/wiki/Common%20Active%20Directory%20Bind%20Errors
					String hex = getADErrorCode(e.getDiagnosticMessage());
					if ("533".equals(hex) || "773".equals(hex) || "80090346".equals(hex) || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							"701".equals(hex) ||  "530".equals(hex)) { //$NON-NLS-1$ //$NON-NLS-2$
						locked = true;
					}
				}
				if (locked) {
					activator.info("LDAP User auto-import error, the user is locked: " + ee.getLocalizedMessage());
					activator.debug(ee);
				} else {
					activator.error("LDAP User auto-import error, unexpected error: " + ee.getLocalizedMessage());
				}
				return 0;
			} finally {
				closeConnection(cn, e);
			}
		}
	}
	
	public void bind(String login, String password) {
		bind(Integer.toString(bindKind), login, password);
	}
	
	public void bind(String type, String login, String password) {
		if ((type == null) || (login == null) || (password == null)) {
			System.out.println("Usage: bind <type> <login> <password>");
		}
		System.out.println("OS charset: " + Charset.defaultCharset().toString());
		System.out.println("Binding user: >" + login + "<.");
		System.out.println("With password: >" + password + "<.");
		int bk = getBindKind(type);
		try {
			BindRequest br = getBindRequest(bk, login, password.toCharArray());
			if (br == null) {
				System.out.println("Unable o create a Bind request.");
				return;
			}
			LDAPConnection cn = getConnection();
			LDAPException exception = null;
			try {
				BindResult result = null;
				try {
					result = cn.bind(br);
				} catch (LDAPException e) {
					if (e.getResultCode() == ResultCode.BUSY) {
						try {
							for (int i = busy; i > 0; i--) {
								System.out.println("LDAP server is busy... wait 2 sec...");
								Thread.sleep(2000);
								try {
									result = cn.bind(br);
								} catch (LDAPException ee) {
									if (ee.getResultCode() != ResultCode.BUSY) {
										e = ee;
										break;
									}
								}
							}
						} catch (InterruptedException ie) {
							// ok, we end here...
						}
					}
					exception = e;
				}
				if (result != null) {
					System.out.println("Bind successfull...");
					System.out.println("Details:");
					System.out.println(result.toString());
				}
			} finally {
				closeConnection(cn, exception);
			}
			if (exception != null) {
				System.out.println("An error occurs during the LDAP binding: "  + exception.getLocalizedMessage());
				System.out.println(exception.getDiagnosticMessage());
			}
		} catch (LDAPException e) {
			System.out.println("An error occurs during the LDAP binding pre-process: "  + e.getLocalizedMessage());
			activator.info(e);
		}
		
	}
}
