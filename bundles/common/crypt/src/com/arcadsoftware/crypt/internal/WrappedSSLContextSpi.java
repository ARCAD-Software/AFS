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
package com.arcadsoftware.crypt.internal;

import java.security.KeyManagementException;
import java.security.SecureRandom;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLContextSpi;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSessionContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

import com.arcadsoftware.crypt.ConfiguredSSLContext;

public class WrappedSSLContextSpi extends SSLContextSpi {

	private final SSLContext wrappedSSLContext;
	private final ConfiguredSSLContext parent;
	
	public WrappedSSLContextSpi(SSLContext wrappedSSLContext, ConfiguredSSLContext parent) {
		super();
		this.wrappedSSLContext = wrappedSSLContext;
		this.parent = parent;
	}

	@Override
	protected SSLEngine engineCreateSSLEngine() {
		return init(wrappedSSLContext.createSSLEngine());
	}

	@Override
	protected SSLEngine engineCreateSSLEngine(String host, int port) {
		return init(wrappedSSLContext.createSSLEngine(host, port));
	}

	private SSLEngine init(SSLEngine engine) {
		//engine.setSSLParameters(null);
		engine.setWantClientAuth(parent.isWantClientAuth());
		engine.setNeedClientAuth(parent.isNeedClientAuth());
		if (parent.isForceClientMode()) {
			engine.setUseClientMode(parent.isUseClientMode());
		}
		engine.setEnabledCipherSuites(parent.getEnabledCipherSuites(engine.getSupportedCipherSuites()));
		engine.setEnabledProtocols(parent.getEnabledProtocols(engine.getSupportedProtocols()));
		return engine;
	}

	@Override
	protected SSLSessionContext engineGetClientSessionContext() {
		return wrappedSSLContext.getClientSessionContext();
	}

	@Override
	protected SSLSessionContext engineGetServerSessionContext() {
		return wrappedSSLContext.getServerSessionContext();
	}

	@Override
	protected SSLServerSocketFactory engineGetServerSocketFactory() {
		return new WrappedSSLServerSocketFactory(wrappedSSLContext. getServerSocketFactory(), parent);
	}

	@Override
	protected SSLSocketFactory engineGetSocketFactory() {
		return new WrappedSockectFactory(wrappedSSLContext.getSocketFactory(), parent);
	}

	@Override
	protected void engineInit(KeyManager[] km, TrustManager[] tm, SecureRandom sr) throws KeyManagementException {
		wrappedSSLContext.init(km, tm, sr);
	}

}
