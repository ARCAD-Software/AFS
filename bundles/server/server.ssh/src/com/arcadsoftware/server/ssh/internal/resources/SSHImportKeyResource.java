/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.server.ssh.internal.resources;

import org.restlet.data.Method;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.Path;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.server.ssh.services.SSHService;
import com.arcadsoftware.ssh.model.SSHException;
import com.arcadsoftware.ssh.model.SSHKey;
import com.arcadsoftware.ssh.model.SSHKeyUpload;
import com.arcadsoftware.ssh.model.SSHRoutes;

@Path(SSHRoutes.IMPORT_KEY)
public class SSHImportKeyResource extends UserLinkedResource {

	@Override
	protected void doInit() {
		super.doInit();
		getAllowedMethods().add(Method.POST);
		getAllowedMethods().add(Method.PUT);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) {
		return super.post(representation, variant);
	}

	@Override
	protected Representation post(final Representation entity, final Variant variant) {
		final SSHKeyUpload sshKeyUpload = new SSHKeyUpload(new BeanMap(SSHKey.ENTITY, getRequestForm()));
		// Passphrase may have been fogged
		String pf = sshKeyUpload.getPassphrase();
		if ((pf != null) && !pf.isEmpty()) {
			sshKeyUpload.setPassphrase(Crypto.unFog(pf, StandardCharsets.UTF_8));
		}
		final SSHKeyUpload upload = new SSHKeyUpload(SSHKey.ENTITY);
		try {
			SSHService ssh = getOSGiService(SSHService.class);
			if (ssh != null) {
				ssh.importKey(sshKeyUpload);
				upload.setSuccessful(true);
			}
		} catch (SSHException e) {
			getLoggedPlugin().error(e.getLocalizedMessage(), e);
			upload.setSuccessful(false);
			final StringBuilder causes = new StringBuilder();
			Throwable t = e;
			while (t != null) {
				causes.append(t.getMessage());
				causes.append('\n');
				t = t.getCause();
			}
			upload.setMessage(causes.toString().trim());
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(new JSonBeanMapStream().toXML(upload.getBeanmap()), getClientPreferedLanguage());
		}
		return new XMLRepresentation(new XmlBeanMapStream().toXML(upload.getBeanmap()), getClientPreferedLanguage());
	}
}
