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
package com.arcadsoftware.crypt;

import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.net.ssl.X509TrustManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is an SSL Certificate manager that can be used to record Server 
 * certificates and eventually trust any certificate send from the server. 
 *
 * @author ARCAD Software
 */
public class TrustAnyCertificateManager  implements X509TrustManager {

	private static final Logger logger = LoggerFactory.getLogger(TrustAnyCertificateManager.class);
	
    private final X509TrustManager tm;
    private ArrayList<X509Certificate> chain;
    private boolean trustAll;

    /**
     * Create a new certificate manager that may allow to trust any server certificate.
     * 
     * @param tm
     * @param insecureTrustAll If this option is activated then this manager will accept any certificate without any other consideration.
     */
    public TrustAnyCertificateManager(X509TrustManager tm, boolean insecureTrustAll) {
        this.tm = tm;
        chain = new ArrayList<X509Certificate>();
        trustAll = insecureTrustAll;
        if (trustAll) {
        	logger.warn("Using a Certificate Manager allowing to trust any source in production may produce a risk of attack on this TCP connection.");
        }
    }

    /**
     * Create a new Trust Certificate Manager.
     * 
     * @param tm 
     */
    public TrustAnyCertificateManager(X509TrustManager tm) {
        this(tm, false);
    }

    public X509Certificate[] getAcceptedIssuers() {
        throw new UnsupportedOperationException();
    }

    public void checkClientTrusted(X509Certificate[] chain, String authType)
            throws CertificateException {
        throw new UnsupportedOperationException();
    }

    public void checkServerTrusted(X509Certificate[] chain, String authType)
            throws CertificateException {
        Collections.addAll(this.chain, chain);
        if (!trustAll) {
        	tm.checkServerTrusted(chain, authType);
        }
    }
    
    public List<X509Certificate> getChain() {
    	return chain;
    }
    
    public X509TrustManager getParentTrustManager() {
    	return tm;
    }
}