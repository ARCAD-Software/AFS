package com.arcadsoftware.crypt.internal;

import javax.net.ssl.SSLContext;

import com.arcadsoftware.crypt.ConfiguredSSLContext;

public class WrapperSSLContext extends SSLContext {

	public WrapperSSLContext(SSLContext wrappedSSLContext, ConfiguredSSLContext parent) {
		super(new WrappedSSLContextSpi(wrappedSSLContext, parent), wrappedSSLContext.getProvider(), wrappedSSLContext.getProtocol());
	}

}
