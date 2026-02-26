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
package com.arcadsoftware.server.ssh.internal;

import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.rest.BeanMapItemResource;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.ssh.model.ISSHService;
import com.arcadsoftware.ssh.model.SSHException;
import com.arcadsoftware.ssh.model.SSHKey;

public class SSHGenerateKeyResource extends BeanMapItemResource {

	@Override
	public String getType() {
		return SSHKey.ENTITY;
	}

	@Override
	public boolean hasRight(final Method method) {
		return method.equals(Method.PUT) && hasRight(10);
	}

	@Override
	protected void put(final BeanMap bean) {
		final SSHKey tempSSHKey = new SSHKey(bean);
		// Passphrase may have been fogged
		if (tempSSHKey.isEncrypted()) {
			tempSSHKey.setPassphrase(new String(Crypto.unFog(tempSSHKey.getPassphrase())));
		}
		try {
			final SSHKey sshKey = getOSGiService(ISSHService.class).create(tempSSHKey.getBeanMap());
			bean.clear();
			bean.addAll(sshKey.getBeanMap());
			bean.forceId(sshKey.getId());
		} catch (final SSHException e) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, e.getMessage(), e);
		}
	}
}
