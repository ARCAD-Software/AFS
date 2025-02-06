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

import java.security.PrivilegedAction;
import java.util.Base64;

import javax.security.auth.Subject;
import javax.security.auth.login.LoginContext;
import javax.security.auth.login.LoginException;

import org.ietf.jgss.GSSContext;
import org.ietf.jgss.GSSException;
import org.ietf.jgss.GSSManager;
import org.ietf.jgss.GSSName;
import org.ietf.jgss.Oid;
import org.restlet.data.ChallengeResponse;
import org.restlet.data.ChallengeScheme;

import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * helper class usable for Kerberos SSO.
 * 
 * @author ARCAD Software
 */
public class NegotiateAuthenticationHelper {
	
	/**
	 * OID for Kerberos 5 authentication
	 */
	private static final String GSS_KRB5_MECH_OID = "1.2.840.113554.1.2.2"; //$NON-NLS-1$

	/**
	 * SPNEGO ChallengeScheme.
	 */
	public static final ChallengeScheme SPNEGO = new ChallengeScheme("HTTP_Negotiate", "Negotiate"); //$NON-NLS-1$ //$NON-NLS-2$

	static {
    	// Mandatory to perform SSO in MS Windows:
        System.setProperty("javax.security.auth.useSubjectCredsOnly", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        // Debug mode:
        if ("true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.trace"))) { //$NON-NLS-1$ //$NON-NLS-2$
        	System.setProperty("sun.security.krb5.debug", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        	System.setProperty("sun.security.spnego.debug", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        }
	}
	
	private final String serviceName;
	private final ILoggedPlugin activator;
	
	public NegotiateAuthenticationHelper(ILoggedPlugin activator, String serverURL) {
		super();
		this.activator = activator;
		// Provoque aussi l'initialization statique requise à l'utilisation de cette classe. 
		String s = serverURL.toLowerCase();
		String serviceName;
		if (s.startsWith("https://")) { //$NON-NLS-1$
			serviceName = serverURL.substring(8); 
		} else { // "http://"
			serviceName = serverURL.substring(7); 
		}
		int i = serviceName.indexOf(':');
		if (i > 0) {
			serviceName = serviceName.substring(0, i);
		}
		i = serviceName.indexOf('/');
		if (i > 0) {
			serviceName = serviceName.substring(0, i);
		}
		this.serviceName = "HTTP/" + serviceName; //$NON-NLS-1$
	}
	
	/**
	 * Generate a ChallengeResponse that is globally usable for the given server connection.
	 * 
	 * @return
	 */
	public ChallengeResponse createChallengeResponse() {
		ChallengeResponse challenge = new ChallengeResponse(SPNEGO);
		challenge.setRawValue(createAuthToken("com.sun.security.jgss.krb5.initiate"));
		return challenge;
	}
	
	/**
	 * Create a GSSAPI token for Negotiate authentication in a request
	 * 
	 * This method must be called from a PrivilegedAction or a
	 * PrivilegedExceptionAction.
	 * 
	 * @param serviceName
	 *            GSSAPI service name to build the token for
	 * @return Base64 encoded GSSAPI token for Negotiate authentication
	 * @throws GSSException
	 */
	private String buildToken64() throws GSSException {
		GSSManager manager = GSSManager.getInstance();
		// Here we choose Kerberos 5 token
		Oid oid = new Oid(GSS_KRB5_MECH_OID);
		// Setup "parameters" for service token
		GSSName peerName = manager.createName(serviceName, null);
		GSSContext secContext = manager.createContext(peerName, oid, null, GSSContext.DEFAULT_LIFETIME);
		secContext.requestMutualAuth(false);
		secContext.requestCredDeleg(true);
		// Create token
		byte[] inToken = new byte[0];
		byte[] outToken = secContext.initSecContext(inToken, 0, inToken.length);
		// Cleanup
		secContext.dispose();
		// Encode result
		return Base64.getEncoder().encodeToString(outToken);
	}

	/**
	 * JAAS login...
	 * 
	 * @return authenticated Subject
	 */
	private Subject login(String jaasConfigName) {
		try {
			LoginContext lc = new LoginContext(jaasConfigName);
			// Attempt authentication...
			lc.login();
			return lc.getSubject();
		} catch (LoginException fle) {
			// Il peut y avoir une exception si JAAS ou Kerberos n'est pas bien configuré !!!
			if (activator != null) {
				activator.error(fle.getLocalizedMessage(), fle);
			}
			return null;
		}
	}

	/**
	 * Create a GSS-API token for the REST service.
	 * 
	 * @param jaasConfigName
	 */
	private String createAuthToken(final String jaasConfigName) {
		// JAAS login
		Subject subject = login(jaasConfigName);
		// Push the subject into the current ACC
		return Subject.doAsPrivileged(subject, new PrivilegedAction<String>() {
			@Override
			public String run() {
				try {
					return buildToken64();
				} catch (GSSException e) {
					if (activator != null) {
						activator.error(e.getLocalizedMessage(), e);
					}
					return null;
				}
			}
		}, null);
	}

}
