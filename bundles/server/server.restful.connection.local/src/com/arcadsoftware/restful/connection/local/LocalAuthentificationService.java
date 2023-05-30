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
package com.arcadsoftware.restful.connection.local;

import java.util.ArrayList;
import java.util.List;

import org.restlet.Request;

import com.arcadsoftware.rest.connection.IBasicAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.beanmap.BeanMap;

public class LocalAuthentificationService implements IBasicAuthentificationService {

	private final Activator activator;
	
	public LocalAuthentificationService(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public IConnectionCredential generateCredential(Request request, String identifier) {
		BeanMap auth = activator.getAuth(identifier);
		if (auth == null) {
			return null;
		}
		return new LocalConnectionCredential(activator, identifier, auth);
	}

	@Override
	public void purgeConnectionCache() {
		activator.cacheClear();
	}

	@Override
	public void purgeConnectionCache(int id) {
		// TODO Partial reload of local connections cache.
		activator.cacheClear();
	}

	@Override
	public List<String> getUserLogins(String userType, int userId) {
		ArrayList<String> result = new ArrayList<String>();
		if ("user".equals(userType)) { //$NON-NLS-1$
			BeanMap auth = activator.getAuth(userId);
			if (auth != null) {
				result.add(auth.getString(Activator.LOCALAUTH_LOGIN));
			}
		}
		return result;
	}

}
