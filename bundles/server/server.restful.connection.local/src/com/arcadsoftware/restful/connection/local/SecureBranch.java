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
package com.arcadsoftware.restful.connection.local;

import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;

public class SecureBranch extends SimpleBranch {

	private final Activator activator;
	
	public SecureBranch(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	protected RouteList createAttachedResources(Context context, Router router) {
		RouteList list = new RouteList(3);
		list.add(router.attach("/localauth/test/password", TestPasswordResource.class)); //$NON-NLS-1$
		list.add(router.attach("/localauth/{login}", new LocalAuthLoginRestlet(activator, context))); //$NON-NLS-1$
		list.add(router.attach("/localauth/{a}/{b}/{c}", new TestPasswordRestlet(context))); //$NON-NLS-1$
		return list;
	}

}
