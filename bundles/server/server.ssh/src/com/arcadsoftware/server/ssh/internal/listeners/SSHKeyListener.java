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
package com.arcadsoftware.server.ssh.internal.listeners;

import java.io.IOException;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.server.ssh.services.SSHService;
import com.arcadsoftware.ssh.model.SSHKey;

@Component(service = { IMetaDataDeleteListener.class }, property = IMetaDataDeleteListener.PROP_TYPE + "=" + SSHKey.ENTITY)
public class SSHKeyListener implements IMetaDataDeleteListener {

	private Logger log = LoggerFactory.getLogger(SSHKeyListener.class);
	private SSHService sshService;

	@Reference
	private void bindSSHService(final SSHService sshService) {
		this.sshService = sshService;
	}

	@Override
	public void postDeletion(final MetaDataEntity entity, final BeanMap originalItem, final IConnectionUserBean user,
			final Language language) {
		try {
			sshService.deleteKeyFiles(new SSHKey(originalItem));
		} catch (final IOException e) {
			log.error("SSHKey postDeletion failed", e);
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, e);
		}
	}

	@Override
	public boolean testDeletion(final MetaDataEntity entity, final BeanMap originalItem, final IConnectionUserBean user,
			final Language language) {
		return true;
	}
}
