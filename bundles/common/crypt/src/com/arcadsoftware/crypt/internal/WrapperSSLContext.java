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

import javax.net.ssl.SSLContext;

import com.arcadsoftware.crypt.ConfiguredSSLContext;

public class WrapperSSLContext extends SSLContext {

	public WrapperSSLContext(SSLContext wrappedSSLContext, ConfiguredSSLContext parent) {
		super(new WrappedSSLContextSpi(wrappedSSLContext, parent), wrappedSSLContext.getProvider(), wrappedSSLContext.getProtocol());
	}

}
