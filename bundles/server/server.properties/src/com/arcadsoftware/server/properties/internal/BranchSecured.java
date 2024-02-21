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
package com.arcadsoftware.server.properties.internal;

import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;

public class BranchSecured extends SimpleBranch {

	@Override
	protected RouteList createAttachedResources(Context context, Router router) {
		return new RouteList(router.attach("/admin/properties/{filename}", AdminPropertiesResource.class)); //$NON-NLS-1$
	}

}
