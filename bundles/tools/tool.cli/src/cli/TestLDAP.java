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
package cli;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import javax.net.SocketFactory;

import com.arcadsoftware.crypt.ConfiguredSSLContext;
import com.arcadsoftware.crypt.ConfiguredSSLContextException;
import com.arcadsoftware.tool.cli.Command;
import com.unboundid.ldap.sdk.BindRequest;
import com.unboundid.ldap.sdk.BindResult;
import com.unboundid.ldap.sdk.CRAMMD5BindRequest;
import com.unboundid.ldap.sdk.DIGESTMD5BindRequest;
import com.unboundid.ldap.sdk.DereferencePolicy;
import com.unboundid.ldap.sdk.FailoverServerSet;
import com.unboundid.ldap.sdk.Filter;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPConnectionOptions;
import com.unboundid.ldap.sdk.LDAPConnectionPool;
import com.unboundid.ldap.sdk.LDAPException;
import com.unboundid.ldap.sdk.PLAINBindRequest;
import com.unboundid.ldap.sdk.ResultCode;
import com.unboundid.ldap.sdk.SCRAMSHA1BindRequest;
import com.unboundid.ldap.sdk.SCRAMSHA256BindRequest;
import com.unboundid.ldap.sdk.SCRAMSHA512BindRequest;
import com.unboundid.ldap.sdk.SearchRequest;
import com.unboundid.ldap.sdk.SearchResult;
import com.unboundid.ldap.sdk.SearchScope;
import com.unboundid.ldap.sdk.SimpleBindRequest;
import com.unboundid.ldap.sdk.StartTLSPostConnectProcessor;
import com.unboundid.ldap.sdk.controls.PasswordExpiredControl;
import com.unboundid.ldap.sdk.controls.PasswordExpiringControl;

public class TestLDAP extends Command {

	private static final String PROP_HOST = "host"; //$NON-NLS-1$
	private static final String PROP_PORT = "port"; //$NON-NLS-1$
	private static final String PROP_SERVER = "server"; //$NON-NLS-1$
	private static final String PROP_SERVERS = "servers"; //$NON-NLS-1$
	private static final String PROP_REORDERFAILOVERLIST = "reorderfailoverlist"; //$NON-NLS-1$
	private static final String PROP_CONNECTIONMAXAGE = "connection.pool.max.age"; //$NON-NLS-1$
	private static final String PROP_CONNECTIONFAILOVERMAXAGE = "connection.failover.max.age"; //$NON-NLS-1$
	private static final String PROP_TIMEOUT = "timeout"; //$NON-NLS-1$
	private static final String PROP_BINDTYPE = "bind.type";
	private static final String PROP_LOGINATTRIBUTE = "dn.login"; //$NON-NLS-1$
	private static final String PROP_DNLOGIN = "dn.login.pattern"; //$NON-NLS-1$
	private static final String PROP_DNBASE = "dn.base"; //$NON-NLS-1$
	private static final int BINDKIND_SIMPLE = 1;
	private static final int BINDTYPE_CRAMMD5 = 2;
	private static final int BINDTYPE_DIGESTMD5 = 3;
	private static final int BINDTYPE_PLAIN = 4;
	private static final int BINDTYPE_SCRAMSHA1 = 5;
	private static final int BINDTYPE_SCRAMSHA256 = 6;
	private static final int BINDTYPE_SCRAMSHA512 = 7;

	public static void main(String[] args) {
		System.exit(new TestLDAP(args).exec());
	}

	public TestLDAP() {
		super();
	}

	public TestLDAP(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		String login = getArgumentValue(new String[] {"-l", "-login"}, "").trim(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		char[] pwd = getArgumentValue(new String[] {"-p", "-password"}, new char[0]); //$NON-NLS-1$ //$NON-NLS-2$
		if (login.isEmpty()) {
			login = read("Enter the LDAP user login to test the connection: ");
			if (login.isEmpty()) {
				printError("The LDAP Login is required to launch the test."); 
				return ERROR_MISSING_PARAMETER;
			}
		}
		if (pwd.length == 0) {
			pwd = readSecret("Enter the LDAP user password: ");
			if (pwd.length == 0) {
				printError("The LDAP user password is required to launch the test."); 
				return ERROR_MISSING_PARAMETER;
			}
		}
		final Hashtable<String, Object> conf = getOSGiConfiguration("com.arcadsoftware.server.restful.connection.ldap");
		if (!isConfigurationComplete(conf)) {
			printError("The LDAP configuration is not correctly completed, the property \""+PROP_DNBASE+"\" and at least one of the properties \"" + PROP_HOST + "\", \"" + PROP_SERVER + "\" or \"" + PROP_SERVERS + "\" must be set.");
			return ERROR_INVALID_CONFIGURATION;
		}
		String base = getProperty(conf, PROP_DNBASE, null);
		if (base == null) {
			return ERROR_INVALID_CONFIGURATION;
		}
		int bindKind = BINDKIND_SIMPLE;
		switch (getProperty(conf, PROP_BINDTYPE, "direct").toLowerCase()) { //$NON-NLS-1$
		case "1": //$NON-NLS-1$
		case "s": //$NON-NLS-1$
		case "smpl": //$NON-NLS-1$
		case "simple": //$NON-NLS-1$
			bindKind = BINDKIND_SIMPLE;
			break;
		case "2": //$NON-NLS-1$
		case "plain": //$NON-NLS-1$
			bindKind = BINDTYPE_PLAIN;
			break;
		case "3": //$NON-NLS-1$
		case "dmd5": //$NON-NLS-1$
		case "direct-md5": //$NON-NLS-1$
			bindKind = BINDTYPE_DIGESTMD5;
			break;
		case "4": //$NON-NLS-1$
		case "cmd5": //$NON-NLS-1$
		case "cram-md5": //$NON-NLS-1$
			bindKind = BINDTYPE_CRAMMD5;
			break;
		case "5": //$NON-NLS-1$
		case "ssha1": //$NON-NLS-1$
		case "scram-sha-1": //$NON-NLS-1$
			bindKind = BINDTYPE_SCRAMSHA1;
			break;
		case "6": //$NON-NLS-1$
		case "ssha256": //$NON-NLS-1$
		case "scram-sha-256": //$NON-NLS-1$
			bindKind = BINDTYPE_SCRAMSHA256;
			break;
		case "7": //$NON-NLS-1$
		case "ssha512": //$NON-NLS-1$
		case "scram-sha-512": //$NON-NLS-1$
			bindKind = BINDTYPE_SCRAMSHA512;
			break;
		default:
			bindKind = BINDKIND_SIMPLE;
			String k = getProperty(conf, PROP_BINDTYPE, (String) null);
			if (k == null) {
				print("No Bind Kind specified assuming DIRECT binding...");
			} else {
				print("Unsupported Bind Kind \"" + k + "\", using DIRECT binding instead");
			}
		}
		final ConfiguredSSLContext sslConf;
		try {
			sslConf = new ConfiguredSSLContext(conf);
		} catch (ConfiguredSSLContextException e) {
			printError("Invalid TLS Configuration: " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			return ERROR_INVALID_CONFIGURATION;
		}
		StartTLSPostConnectProcessor startTLSProcessor = null;
		if (sslConf.isStartTLS()) {
			startTLSProcessor = new StartTLSPostConnectProcessor(sslConf.getContext());
		}
		LDAPConnectionPool connectionPool = null;
		try {
			connectionPool = new LDAPConnectionPool(getServerSet(conf, sslConf), null, 1, 1,
					startTLSProcessor, false);
			connectionPool.setConnectionPoolName("AFS: REST Web-service Authentication"); //$NON-NLS-1$
			connectionPool.setMaxConnectionAgeMillis(getProperty(conf, PROP_CONNECTIONMAXAGE, 120000L));
			connectionPool.setMaxDefunctReplacementConnectionAgeMillis(getProperty(conf, PROP_CONNECTIONFAILOVERMAXAGE, 60000L));
			connectionPool.setMaxWaitTimeMillis(getProperty(conf, PROP_TIMEOUT, 20000L));
			connectionPool.setRetryFailedOperationsDueToInvalidConnections(true);
			connectionPool.setHealthCheckIntervalMillis(5000L);
			connectionPool.setMinimumAvailableConnectionGoal(0);
			connectionPool.setCreateIfNecessary(true);
			// Try a binding...
			LDAPConnection cn = connectionPool.getConnection();
			println("LDAP Connection successful.");
			LDAPException e = null;
			try {
				BindResult br = bind(bindKind, conf, cn, login, pwd);
				try {
					if (PasswordExpiringControl.get(br) != null) {
						println("The given LDAP User have to change its password soon (Password Expiring Control).");
					}
				} catch (LDAPException e1) {
					printWarn("Error during expiration estimation: " + e1.getLocalizedMessage());
					if (isArgument("-debug")) { //$NON-NLS-1$
						e1.printStackTrace();
					}
				}
				println("LDAP Binding successful.");
				// TODO Try a selection of objects with given attributes names...
				try {
					String searchPattern = getArgumentValue(new String[] {"-sp", "-search"}, "*").trim(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					Filter filter = Filter.create(searchPattern);
					// |ML] why if loginpatter= "%s" use ONE as scope ???
					SearchResult sr = cn.search(new SearchRequest(null, base, SearchScope.SUB, DereferencePolicy.ALWAYS, 
							0, 0, false, filter, SearchRequest.ALL_USER_ATTRIBUTES));
					println("The Search pattern \"" + searchPattern + "\" returned " + sr.getEntryCount() + " results.");
					println("LDAP connection test completed.");
				} catch (LDAPException ee) {
					e = ee;
					printError("LDAP Search error: " + ee.getLocalizedMessage());
					return ERROR_INVALID_CONFIGURATION;
				}
			} catch (LDAPException ee) {
				e = ee;
				printWarn("LDAP Binding error: " + ee.getLocalizedMessage());
				if (isArgument("-debug")) { //$NON-NLS-1$
					ee.printStackTrace();
				}
				try {
					if (PasswordExpiredControl.get(e.toLDAPResult()) != null) {
						println("The given LDAP User is locked.");
					}
				} catch (LDAPException e1) {
					printWarn("Error during User lock control: " + e1.getLocalizedMessage());
					if (isArgument("-debug")) { //$NON-NLS-1$
						e1.printStackTrace();
					}
				}
				ResultCode rc = e.getResultCode();
				if (rc == ResultCode.INVALID_CREDENTIALS) {
					// https://ldapwiki.com/wiki/Common%20Active%20Directory%20Bind%20Errors
					String hex = getADErrorCode(e.getDiagnosticMessage());
					if ("533".equals(hex) || "773".equals(hex) || "80090346".equals(hex) || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							"701".equals(hex) ||  "530".equals(hex)) { //$NON-NLS-1$ //$NON-NLS-2$
						println("The given LDAP user is locked (Active Directory Lock Code).");
					}
				} else {
					printError("The LDAP Error is unexpected.");
					return ERROR_INVALID_CONFIGURATION;
				}
			} finally {
				if (e != null) {
					connectionPool.releaseConnectionAfterException(cn, e);
				} else {
					connectionPool.releaseConnection(cn);
				}
			}
		} catch (LDAPException e) {
			printError("LDAP Error while performing connection test.");
			print(e.getDiagnosticMessage());
			return ERROR_TEST_FAIL;
		} finally {
			if (connectionPool != null) {
				connectionPool.close();
			}
		}
		return 0;
	}
	
	private String getProp(Map<String, Object> props, String key, String value) {
		return getProperty(props, key, value);
	}

	// Following method are directly taken from LdapAuthentificationService (except minor modifications)
	public boolean isConfigurationComplete(Map<String, Object> props) {
		return (!getProp(props, PROP_HOST, "").trim().isEmpty() || //$NON-NLS-1$
				!getProp(props, PROP_SERVER, "").trim().isEmpty() || //$NON-NLS-1$
				!getProp(props, PROP_SERVERS, "").trim().isEmpty()) && //$NON-NLS-1$
				(!getProp(props, PROP_DNBASE, "").trim().isEmpty() || //$NON-NLS-1$
				 !getProp(props, PROP_DNLOGIN, "").trim().isEmpty()); //$NON-NLS-1$
	}

	private FailoverServerSet getServerSet(final Map<String, Object> props, final ConfiguredSSLContext sslConf) {
		SocketFactory sslSocketFactory = sslConf.getSocketFactory();
		Integer defPort;
		if ((sslSocketFactory != null) && !sslConf.isStartTLS()) {
			defPort = 636;
		} else {
			sslSocketFactory = null; // StartTLS is configured on a non secure socket...
			defPort = 389;
		}
		defPort = getProperty(props, PROP_PORT, defPort);
		final ArrayList<Integer> ports = new ArrayList<Integer>();
		final ArrayList<String> servers = new ArrayList<String>();
		String s = getProperty(props, PROP_HOST, null);
		if ((s != null) && !s.isEmpty()) {
			servers.add(s);
			ports.add(defPort);
		}
		s = getProperty(props, PROP_SERVER, null);
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
		s = getProperty(props, PROP_SERVERS, null);
		if ((s != null) && !s.isEmpty()) {
			for (String ss : s.trim().split(" ")) {
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
		FailoverServerSet serverSet = new FailoverServerSet(svrs, prts, sslSocketFactory, getConnectionOptions(props));
		serverSet.setReOrderOnFailover(getProperty(props, PROP_REORDERFAILOVERLIST, false));
		return serverSet;
	}

	private LDAPConnectionOptions getConnectionOptions(Map<String, Object> props) {
		LDAPConnectionOptions ldapOptions = new LDAPConnectionOptions();
		ldapOptions.setAbandonOnTimeout(true);
		ldapOptions.setBindWithDNRequiresPassword(false);
		ldapOptions.setResponseTimeoutMillis(60000);
		ldapOptions.setUsePooledSchema(true);
		ldapOptions.setPooledSchemaTimeoutMillis(60000);
		ldapOptions.setUseSynchronousMode(true);
		return ldapOptions;
	}
	
	public BindResult bind(final int bindKind, final Map<String, Object> props, final LDAPConnection cn, String userlogin, char[] secret) throws LDAPException {
		BindRequest br;
		switch (bindKind) {
		case BINDTYPE_CRAMMD5:
			br = new CRAMMD5BindRequest(getUserDN(props, userlogin), new String(secret));
			break;
		case BINDTYPE_DIGESTMD5:
			// TODO Use Alternate authorization ID ! (SASL)
			// TODO Use the "realm" parameter, in configuration or through aunthentication parameters !
			br = new DIGESTMD5BindRequest(getUserDN(props, userlogin), null, new String(secret), null);
			break;
		case BINDTYPE_PLAIN:
			br = new PLAINBindRequest(getUserDN(props, userlogin), new String(secret));
			break;
		case BINDTYPE_SCRAMSHA1:
			br = new SCRAMSHA1BindRequest(getUserDN(props, userlogin), new String(secret));
			break;
		case BINDTYPE_SCRAMSHA256:
			br = new SCRAMSHA256BindRequest(getUserDN(props, userlogin), new String(secret));
			break;
		case BINDTYPE_SCRAMSHA512:
			br = new SCRAMSHA512BindRequest(getUserDN(props, userlogin), new String(secret));
			break;
		default:
			br = new SimpleBindRequest(getUserDN(props, userlogin), new String(secret));
		}
		try {
			return cn.bind(br);
		} catch (LDAPException e) {
			if (e.getResultCode() == ResultCode.BUSY) {
				try {
					Thread.sleep(2000);
					return cn.bind(br);
				} catch (InterruptedException ie) {
					// ok, we end here...
				}
			}
			throw e;
		}
	}

	public String getUserDN(final Map<String, Object> props, String login) {
		String loginPattern = getProp(props, PROP_DNLOGIN, null);
		String loginAttribute = getProp(props, PROP_LOGINATTRIBUTE, null);
		String base = getProp(props, PROP_DNBASE, null);
		if (loginPattern == null) {
			if ((loginAttribute == null) || (base == null)) {
				return login;
			}
			return loginAttribute + '=' + login + ',' + base;
		}
		if (loginPattern.isEmpty()) {
			return login;
		}
		if (loginPattern.contains("%s")) {
			return String.format(loginPattern, login);
		}
		if (base == null) {
			return loginPattern + '=' + login;
		}
		return loginPattern + '=' + login + ',' + base;
	}

	private String getADErrorCode(String detail) {
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

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "testldap"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "Test the LDAP configuration parameters.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-l|-login <user login>]", //$NON-NLS-1$
				"The user login used to test the LDAP binding. This parameter is optional, if not given it will be asked during the test.");
		result.put("[-p|-password <pass phrase>]", //$NON-NLS-1$
				"The user password used to test the LDAP binding. This parameter is optional, if not given it will be asked during the test.");
		result.put("[-sp|-search <ldap search pattern>]", //$NON-NLS-1$
				"Define the search pattern to be used during the test. If not defined a global selection will be used (search pattern equals to \"*\").");
		return result;
	}

}
