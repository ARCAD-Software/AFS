package com.arcadsoftware.crypt.internal;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;

import com.arcadsoftware.crypt.ConfiguredSSLContext;

public class WrappedSSLServerSocketFactory extends SSLServerSocketFactory {

	private final SSLServerSocketFactory serverSocketFactory;
	private final ConfiguredSSLContext parent;
	
	public WrappedSSLServerSocketFactory(SSLServerSocketFactory serverSocketFactory, ConfiguredSSLContext parent) {
		super();
		this.parent = parent;
		this.serverSocketFactory = serverSocketFactory;
	}

	@Override
	public String[] getDefaultCipherSuites() {
		return serverSocketFactory.getDefaultCipherSuites();
	}

	@Override
	public String[] getSupportedCipherSuites() {
		return serverSocketFactory.getSupportedCipherSuites();
	}

	@Override
	public ServerSocket createServerSocket(int arg0) throws IOException {
		return init((SSLServerSocket) serverSocketFactory.createServerSocket(arg0));
	}

	@Override
	public ServerSocket createServerSocket(int arg0, int arg1) throws IOException {
		return init((SSLServerSocket) serverSocketFactory.createServerSocket(arg0, arg1));
	}

	@Override
	public ServerSocket createServerSocket(int arg0, int arg1, InetAddress arg2) throws IOException {
		return init((SSLServerSocket) serverSocketFactory.createServerSocket(arg0, arg1, arg2));
	}

	@Override
	public ServerSocket createServerSocket() throws IOException {
		return init((SSLServerSocket) serverSocketFactory.createServerSocket());
	}

	private SSLServerSocket init(SSLServerSocket socket) {
		socket.setNeedClientAuth(parent.isNeedClientAuth());
		socket.setWantClientAuth(parent.isWantClientAuth());
		if (parent.isForceClientMode()) {
			socket.setUseClientMode(parent.isUseClientMode());
		}
		socket.setEnabledCipherSuites(parent.getEnabledCipherSuites(socket.getSupportedCipherSuites()));
		socket.setEnabledProtocols(parent.getEnabledProtocols(socket.getSupportedProtocols()));
		return socket;
	}

}
