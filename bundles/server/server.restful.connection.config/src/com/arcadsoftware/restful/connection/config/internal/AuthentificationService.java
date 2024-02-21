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
/**
 * 
 */
package com.arcadsoftware.restful.connection.config.internal;

import java.util.ArrayList;
import java.util.List;

import org.restlet.Request;

import com.arcadsoftware.rest.connection.IBasicAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCredential;

/**
 * Creation Date: 15 mars 2011
 */
public class AuthentificationService implements IBasicAuthentificationService {

	private final Activator activator;
	
	public AuthentificationService(Activator activator) {
		this.activator = activator;
	}

	@Override
	public void purgeConnectionCache() {}

	@Override
	public void purgeConnectionCache(int id) {}

	@Override
	public IConnectionCredential generateCredential(Request request, String identifier) {
		return activator.getConnectionCredential(identifier);
	}

	public List<String> getUserLogins(String userType, int userId) {
		return new ArrayList<String>();
	}

}
