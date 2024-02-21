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
package com.arcadsoftware.crypt.internal;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

import com.arcadsoftware.crypt.ConfiguredSSLContext;

public class WrappedSockectFactory extends SSLSocketFactory {

	private final SSLSocketFactory socketFactory;
	private final ConfiguredSSLContext parent;
	private String[] hostnames;
	
	public WrappedSockectFactory(SSLSocketFactory socketFactory, ConfiguredSSLContext parent) {
		super();
		this.socketFactory = socketFactory;
		this.parent = parent;
	}

	@Override
	public String[] getDefaultCipherSuites() {
		return socketFactory.getDefaultCipherSuites();
	}

	@Override
	public String[] getSupportedCipherSuites() {
		return socketFactory.getSupportedCipherSuites();
	}

	@Override
	public Socket createSocket(Socket s, InputStream consumed, boolean autoClose) throws IOException {
		return init((SSLSocket) socketFactory.createSocket(s, consumed, autoClose));
	}

	@Override
	public Socket createSocket() throws IOException {
		return init((SSLSocket) socketFactory.createSocket());
	}

	@Override
	public Socket createSocket(Socket arg0, String arg1, int arg2, boolean arg3) throws IOException {
		return init((SSLSocket) socketFactory.createSocket(arg0, arg1, arg2, arg3));
	}

	@Override
	public Socket createSocket(String arg0, int arg1) throws IOException, UnknownHostException {
		return init((SSLSocket) socketFactory.createSocket(arg0, arg1));
	}

	@Override
	public Socket createSocket(InetAddress arg0, int arg1) throws IOException {
		return init((SSLSocket) socketFactory.createSocket(arg0, arg1));
	}

	@Override
	public Socket createSocket(String arg0, int arg1, InetAddress arg2, int arg3)
			throws IOException, UnknownHostException {
		return init((SSLSocket) socketFactory.createSocket(arg0, arg1, arg2, arg3));
	}

	@Override
	public Socket createSocket(InetAddress arg0, int arg1, InetAddress arg2, int arg3) throws IOException {
		return init((SSLSocket) socketFactory.createSocket(arg0, arg1, arg2, arg3));
	}

	private SSLSocket init(SSLSocket socket) {
		socket.setNeedClientAuth(parent.isNeedClientAuth());
		socket.setWantClientAuth(parent.isWantClientAuth());
		if (parent.isForceClientMode()) {
			socket.setUseClientMode(parent.isUseClientMode());
		}
		socket.setEnabledCipherSuites(parent.getEnabledCipherSuites(socket.getSupportedCipherSuites()));
		socket.setEnabledProtocols(parent.getEnabledProtocols(socket.getSupportedProtocols()));
		if (parent.isVerifyHostname() && (hostnames != null) && (hostnames.length > 0)) {
			socket.addHandshakeCompletedListener(parent.getHostnameVerifierListener(hostnames));
		}
		return socket;
	}

	public void setPeerHostnames(String[] hostname) {
		hostnames = hostname;
	}

}
