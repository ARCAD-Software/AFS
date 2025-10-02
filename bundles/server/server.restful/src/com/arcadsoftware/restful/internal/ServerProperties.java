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
package com.arcadsoftware.restful.internal;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Dictionary;

import org.osgi.framework.Bundle;
import org.osgi.framework.Constants;

import com.arcadsoftware.crypt.ConfiguredSSLContext;
import com.arcadsoftware.crypt.Crypto;

public class ServerProperties {
	
	protected static final String PROP_DOMAINNAME = "domainname"; //$NON-NLS-1$;
	protected static final String PROP_PORTNUMBER = "port"; //$NON-NLS-1$;
	protected static final String PROP_PORTSSL = "portssl"; //$NON-NLS-1$;
	private static final String PROP_AUTHOR = "author"; //$NON-NLS-1$;
	private static final String PROP_NAME = "name"; //$NON-NLS-1$;
	private static final String PROP_VERSION = "version"; //$NON-NLS-1$;
	private static final String PROP_OWNER = "owner"; //$NON-NLS-1$;
	private static final String PROP_DESCRIPTION = "description"; //$NON-NLS-1$;
	private static final String PROP_KEYPATH = "keystore"; //$NON-NLS-1$;
	protected static final String PROP_KEYSTOREPWD = "keystorepwd"; //$NON-NLS-1$;
	protected static final String PROP_KEYPWD = "keypwd"; //$NON-NLS-1$;
	private static final String PROP_KEYTYPE = "keytype"; //$NON-NLS-1$;
	private static final String PROP_KEYALIAS = "keyalias"; //$NON-NLS-1$;
	private static final String PROP_CLIENTAUTH = "clientauth"; //$NON-NLS-1$;
	private static final String PROP_TRUSTPATH = "truststore"; //$NON-NLS-1$;
	protected static final String PROP_TRUSTSTOREPWD = "truststorepwd"; //$NON-NLS-1$;
	private static final String PROP_TRUSTSTORETYPE = "truststoretype"; //$NON-NLS-1$;
	private static final String PROP_LOGDISABLED = "log.disabled"; //$NON-NLS-1$;
	private static final String PROP_CONTACTEMAIL = "email"; //$NON-NLS-1$
	private static final String PROP_LICENSE_URL = "license.url"; //$NON-NLS-1$
	private static final String PROP_LICENSE_NAME = "license.name"; //$NON-NLS-1$
	private static final String PROP_COPYRIGHT = "copyright"; //$NON-NLS-1$
	private static final String PROP_URL = "website"; //$NON-NLS-1$
	private static final String PROP_INTERFACE = "interface"; //$NON-NLS-1$
	private static final String PROP_SYSTEMINTERFACE = "com.arcadsoftware.rest.interface"; //$NON-NLS-1$

	private final int port;
	private final int portssl;
	private final String name;
	private final String author;
	private final String version;
	private final String domainname;
	private final String owner;
	private final String description;
	private final String kStore;
	private final char[] kSPwd;
	private final String kSType;
	private final String keyAlias;
	private final char[] keyPwd;
	private final boolean keyClient;
	private final String tStore;
	private final char[] tSPwd;
	private final String tSType;
	private final String disabledCipherSuites;
	private final String disabledProtocols;
	private final String enabledCipherSuites;
	private final String enabledProtocols;
	private final String keyManagerAlgorithm;
	private final String protocol;
	private final String secureRandomAlgorithm;
	private final String trustManagerAlgorithm;
	private final boolean restletLogDisabled;
	private final String email;
	private final String license;
	private final String licenseURL;
	private final String termsOfService;
	private final String webSite;
	private final String ethernetInterface;
	
	public ServerProperties(final Dictionary<String, ?> properties, final Bundle brandingBundle, final Bundle thisBundle) {
		super();
		port = getPortNumber(properties, PROP_PORTNUMBER);
		portssl = getPortNumber(properties, PROP_PORTSSL);
		ethernetInterface = getProperty(properties, PROP_INTERFACE, System.getProperty(PROP_SYSTEMINTERFACE, null));
		// Branding bundle supersed name, author and version properties
		if (brandingBundle != null) {
			String s = brandingBundle.getHeaders().get(Constants.BUNDLE_NAME);
			if ((s != null) && !s.isEmpty()) {
				name = s;
			} else {
				name = getProperty(properties, PROP_NAME, thisBundle.getHeaders().get(Constants.BUNDLE_NAME));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_VENDOR);
			if ((s != null) && !s.isEmpty()) {
				author = s;
			} else {
				author = getProperty(properties, PROP_AUTHOR, thisBundle.getHeaders().get(Constants.BUNDLE_VENDOR));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_VERSION);
			if ((s != null) && !s.isEmpty()) {
				version = s;
			} else {
				version = getProperty(properties, PROP_VERSION, thisBundle.getVersion().toString());
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION);
			if ((s != null) && !s.isEmpty()) {
				description = s;
			} else {
				description = getProperty(properties, PROP_DESCRIPTION, thisBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS);
			if ((s != null) && !s.isEmpty()) {
				email = s;
			} else {
				email = getProperty(properties, PROP_CONTACTEMAIL, thisBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_LICENSE);
			if ((s != null) && !s.isEmpty()) {
				licenseURL = s;
			} else {
				licenseURL = getProperty(properties, PROP_LICENSE_URL, thisBundle.getHeaders().get(Constants.BUNDLE_LICENSE));
			}
			s = brandingBundle.getHeaders().get("Bundle-License-Name"); //$NON-NLS-1$
			if ((s != null) && !s.isEmpty()) {
				license = s;
			} else {
				license = getProperty(properties, PROP_LICENSE_NAME, thisBundle.getHeaders().get("Bundle-License-Name")); //$NON-NLS-1$
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT);
			if ((s != null) && !s.isEmpty()) {
				termsOfService = s;
			} else {
				termsOfService = getProperty(properties, PROP_COPYRIGHT, thisBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_DOCURL);
			if ((s != null) && !s.isEmpty()) {
				webSite = s;
			} else {
				webSite = getProperty(properties, PROP_URL, thisBundle.getHeaders().get(Constants.BUNDLE_DOCURL));
			}
		} else {
			name = getProperty(properties, PROP_NAME, thisBundle.getHeaders().get(Constants.BUNDLE_NAME));
			author = getProperty(properties, PROP_AUTHOR, thisBundle.getHeaders().get(Constants.BUNDLE_VENDOR));
			version = getProperty(properties, PROP_VERSION, thisBundle.getVersion().toString());
			description = getProperty(properties, PROP_DESCRIPTION, thisBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION));
			email = getProperty(properties, PROP_CONTACTEMAIL, thisBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS));
			licenseURL = getProperty(properties, PROP_LICENSE_URL, thisBundle.getHeaders().get(Constants.BUNDLE_LICENSE));
			license = getProperty(properties, PROP_LICENSE_NAME, thisBundle.getHeaders().get("Bundle-License-Name")); //$NON-NLS-1$
			termsOfService = getProperty(properties, PROP_COPYRIGHT, thisBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT));
			webSite = getProperty(properties, PROP_URL, thisBundle.getHeaders().get(Constants.BUNDLE_DOCURL));
		}
		String dn = getProperty(properties, PROP_DOMAINNAME, getLocalhostName());
		if (dn.equalsIgnoreCase("none")) { //$NON-NLS-1$
			if (ethernetInterface == null) {
				domainname = ""; //$NON-NLS-1$
			} else {
				domainname = ethernetInterface;
			}
		} else {
			domainname = dn;
		}
		owner = getProperty(properties, PROP_OWNER, author);
		kStore = getProperty(properties, PROP_KEYPATH, null);
		kSPwd = Crypto.decrypt(getProperty(properties, PROP_KEYSTOREPWD, null));
		kSType = getProperty(properties, PROP_KEYTYPE, null);
		keyAlias = getProperty(properties, PROP_KEYALIAS, null);
		keyPwd = Crypto.decrypt(getProperty(properties, PROP_KEYPWD, null));
		keyClient = getProperty(properties, PROP_CLIENTAUTH, false);
		tStore = getProperty(properties, PROP_TRUSTPATH, null);
		tSPwd = Crypto.decrypt(getProperty(properties, PROP_TRUSTSTOREPWD, null));
		tSType = getProperty(properties, PROP_TRUSTSTORETYPE, null);
		disabledCipherSuites = getProperty(properties, ConfiguredSSLContext.PROP_DISABLEDCIPHERSUITES, null);
		disabledProtocols = getProperty(properties, ConfiguredSSLContext.PROP_DISABLEDPROTOCOLS, null);
		enabledCipherSuites = getProperty(properties, ConfiguredSSLContext.PROP_ENABLEDCIPHERSUITES, null);
		enabledProtocols = getProperty(properties, ConfiguredSSLContext.PROP_ENABLEDPROTOCOLS, null);
		keyManagerAlgorithm = getProperty(properties, ConfiguredSSLContext.PROP_KEYSTORE_ALGO, null);
		protocol = getProperty(properties, ConfiguredSSLContext.PROP_SSL_PREFEREDPROTOCOL, "TLSv1.3"); //$NON-NLS-1$
		secureRandomAlgorithm = getProperty(properties, ConfiguredSSLContext.PROP_SECURERANDOM, null);
		trustManagerAlgorithm = getProperty(properties, ConfiguredSSLContext.PROP_TRUSTSTORE_ALGO, null);
		restletLogDisabled = getProperty(properties, PROP_LOGDISABLED, false);
	}

	public ServerProperties(final Bundle thisBundle) {
		super();
		port = initPort("com.arcadsoftware.rest.port", "5252"); //$NON-NLS-1$ //$NON-NLS-2$
		portssl = initPort("com.arcadsoftware.rest.sslport", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		ethernetInterface = System.getProperty(PROP_SYSTEMINTERFACE, null);
		name = thisBundle.getHeaders().get(Constants.BUNDLE_NAME);
		author = thisBundle.getHeaders().get(Constants.BUNDLE_VENDOR);
		version = thisBundle.getVersion().toString();
		email = thisBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS);
		description = thisBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION);
		licenseURL = thisBundle.getHeaders().get(Constants.BUNDLE_LICENSE);
		license = thisBundle.getHeaders().get("Bundle-License-Name"); //$NON-NLS-1$
		termsOfService = thisBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT);
		webSite = thisBundle.getHeaders().get(Constants.BUNDLE_DOCURL);
		domainname = getLocalhostName();
		owner = author;
		kStore = null;
		kSPwd = null;
		kSType = null;
		keyAlias = null;
		keyPwd = null;
		keyClient = false;
		tStore = null;
		tSPwd = null;
		tSType = null;
		disabledCipherSuites = null;
		disabledProtocols = null;
		enabledCipherSuites = null;
		enabledProtocols = null;
		keyManagerAlgorithm = null;
		protocol = null;
		secureRandomAlgorithm = null;
		trustManagerAlgorithm = null;
		restletLogDisabled = false;
	}
	
	public void close() {
		Crypto.clear(kSPwd);
		Crypto.clear(tSPwd);
		Crypto.clear(keyPwd);
	}
	
	private int initPort(String propName, String defValue) {
		try {
			return Integer.parseInt(System.getProperty(propName, defValue));
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	/**
	 * Return true if there is not any modifications that require a server restart.
	 * 
	 * <p>
	 * If there is modification they are related to properties that can be changed during the execution of the HTTP(S) server(s).
	 *  
	 * @param oldProperties
	 * @return
	 */
	public boolean isNoMajorModification(ServerProperties oldProperties) {
		return (oldProperties != null) && (port == oldProperties.port) && nullsOrEquals(kStore, oldProperties.kStore) && //
				nullsOrEquals(tStore, oldProperties.tStore) && (portssl == oldProperties.portssl) && nullsOrEquals(protocol, oldProperties.protocol);
	}

	private boolean nullsOrEquals(char[] s1, char[] s2) {
		if (s1 == null) {
			return s2 == null;
		}
		return Arrays.equals(s1, s2);
	}

	private boolean nullsOrEquals(Object s1, Object s2) {
		if (s1 == null) {
			return s2 == null;
		}
		return s1.equals(s2);
	}
	
	private String getLocalhostName() {
		try {
			return InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {
			// Localhost is always known (it return the local IP if not) !
			return ""; //$NON-NLS-N$
		}
	}

	private String getProperty(final Dictionary<String, ?> properties, final String key, final String defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		final String sv = value.toString();
		if (sv.isEmpty()) {
			return defaultValue;
		}
		return sv;
	}

	private boolean getProperty(final Dictionary<String, ?> properties, final String key, final boolean defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		final String sv = value.toString();
		if (sv.isEmpty()) {
			return defaultValue;
		}
		return sv.equalsIgnoreCase("true") || sv.equalsIgnoreCase("yes") || sv.equals("1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
	
	private Integer getPortNumber(final Dictionary<String, ?> properties, final String key) {
		try {
			int p = Integer.parseInt(getProperty(properties, key, "0")); //$NON-NLS-1$
			if ((p >= 0) && (p < 65536)) {
				return p;
			}
		} catch (NumberFormatException e) {}
		return 0;
	}

	public boolean hasDomainName() {
		return (domainname != null) && !domainname.trim().isEmpty();
	}

	public String[] getDomainNames() {
		if ((domainname == null) || domainname.trim().isEmpty()) {
			return new String[0];
		}
		return domainname.split(" "); //$NON-NLS-1$
	}
	
	public boolean isServerInactive() {
		return (port == 0) && (portssl == 0);
	}

	public boolean useHTTPS() {
		return (portssl > 0) &&	(((kStore != null) && (keyAlias != null)) || (keyClient && (tStore != null)));
	}

	@Override
	public String toString() {
		return "ServerProperties [port=" + port + //$NON-NLS-1$
				", portssl=" + portssl + //$NON-NLS-1$
				", name=" + name + //$NON-NLS-1$
				", author=" + author + //$NON-NLS-1$
				", version=" + version + //$NON-NLS-1$
				", domainname=" + domainname + //$NON-NLS-1$
				", owner=" + owner + //$NON-NLS-1$
				", description=" + description + //$NON-NLS-1$
				", kStore=" + kStore + //$NON-NLS-1$
				", keyAlias=" + keyAlias + //$NON-NLS-1$
				", keyClient=" + keyClient + //$NON-NLS-1$
				", tStore=" + tStore + ']'; //$NON-NLS-1$
	}

	@Override
	public int hashCode() {
		return 961 + (31 * port) + portssl;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof ServerProperties)) {
			return false;
		}
		ServerProperties sp = (ServerProperties) obj;
		return (this == obj) || ((sp.port == port) && //
					nullsOrEquals(sp.domainname, domainname) && //
					nullsOrEquals(sp.name, name) && //
					nullsOrEquals(sp.version, version) && //
					nullsOrEquals(sp.author, author) && //
					nullsOrEquals(sp.owner, owner) && //
					nullsOrEquals(sp.description, description) && //
					nullsOrEquals(sp.kStore, kStore) && //
					nullsOrEquals(sp.tStore, tStore) && //
					nullsOrEquals(sp.kSPwd, kSPwd) && //
					nullsOrEquals(sp.tSPwd, tSPwd) && //
					nullsOrEquals(sp.keyAlias, keyAlias) && //
					nullsOrEquals(sp.kSType, kSType) && //
					nullsOrEquals(sp.tSType, tSType) && //
					nullsOrEquals(sp.keyPwd, keyPwd) && //
					(sp.portssl == portssl) && //
					(sp.keyClient == keyClient) && //
					(sp.restletLogDisabled == restletLogDisabled) && //
					nullsOrEquals(sp.disabledCipherSuites, disabledCipherSuites) && //
					nullsOrEquals(sp.disabledProtocols, disabledProtocols) && //
					nullsOrEquals(sp.enabledCipherSuites, enabledCipherSuites) && //
					nullsOrEquals(sp.enabledProtocols, enabledProtocols) && //
					nullsOrEquals(sp.keyManagerAlgorithm, keyManagerAlgorithm) && //
					nullsOrEquals(sp.protocol, protocol) && //
					nullsOrEquals(sp.secureRandomAlgorithm, secureRandomAlgorithm) && //
					nullsOrEquals(sp.trustManagerAlgorithm, trustManagerAlgorithm));
	}

	public int getPort() {
		return port;
	}

	public int getPortssl() {
		return portssl;
	}

	public String getName() {
		return name;
	}

	public String getAuthor() {
		return author;
	}

	public String getVersion() {
		return version;
	}

	public String getDomainname() {
		return domainname;
	}

	public String getOwner() {
		return owner;
	}

	public String getDescription() {
		return description;
	}

	public String getkStore() {
		return kStore;
	}

	public char[] getkSPwd() {
		return kSPwd;
	}

	public String getkSType() {
		return kSType;
	}

	public String getKeyAlias() {
		return keyAlias;
	}

	public char[] getKeyPwd() {
		return keyPwd;
	}

	public boolean isKeyClient() {
		return keyClient;
	}

	public String gettStore() {
		return tStore;
	}

	public char[] gettSPwd() {
		return tSPwd;
	}

	public String gettSType() {
		return tSType;
	}

	public String getDisabledCipherSuites() {
		return disabledCipherSuites;
	}

	public String getDisabledProtocols() {
		return disabledProtocols;
	}

	public String getEnabledCipherSuites() {
		return enabledCipherSuites;
	}

	public String getEnabledProtocols() {
		return enabledProtocols;
	}

	public String getKeyManagerAlgorithm() {
		return keyManagerAlgorithm;
	}

	public String getProtocol() {
		return protocol;
	}

	public String getSecureRandomAlgorithm() {
		return secureRandomAlgorithm;
	}

	public String getTrustManagerAlgorithm() {
		return trustManagerAlgorithm;
	}

	public boolean isRestletLogDisabled() {
		return restletLogDisabled;
	}

	public String getEmail() {
		return email;
	}

	public String getLicense() {
		return license;
	}

	public String getTermsOfService() {
		return termsOfService;
	}

	public String getWebSite() {
		return webSite;
	}

	public String getLicenseURL() {
		return licenseURL;
	}

	public String getEthernetInterface() {
		return ethernetInterface;
	}

}
