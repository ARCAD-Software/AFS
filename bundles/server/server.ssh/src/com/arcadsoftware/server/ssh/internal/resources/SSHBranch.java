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

import org.osgi.service.component.annotations.Component;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.rest.IBranch;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.RouteListBuilder;
import com.arcadsoftware.rest.SimpleBranch;

@Component(service = IBranch.class, property = { IBranch.URI + "=" + IBranch.SECUREDBRANCH })
public class SSHBranch extends SimpleBranch {
	@Override
	protected RouteList createAttachedResources(final Context context, final Router router) {
		return new RouteListBuilder(router).attach(SSHGenerateKeyResource.class).attach(SSHGetPublicKeyResource.class)
				.attach(SSHImportKeyResource.class).toRouteList();
	}
}
