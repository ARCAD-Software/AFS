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
package com.arcadsoftware.rest.connection;

import java.io.Serializable;

/**
 * This Profile automatically provide any right with a value lower or equal to the inverse of the User ID...
 * 
 * <p>
 * For instance, a user with an Id equal to -1000 gain all rights from 1 to 999.
 * 
 * <p>
 * Creation Date: 15 mars 2011
 */
public class AutoProfile extends Profile implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private int limit;
	
	/**
	 * Default creator for serialization.
	 */
	public AutoProfile() {
		super();
	}
	
	/**
	 * Create the Profile information.
	 * 
	 * @param id The user ID is also used to determine the rights accessible to this user.
	 * @param owner The user connection information associated to this Profile.
	 */
	public AutoProfile(int id, IConnectionUserBean owner) {
		this();
		setOwner(owner);
		limit = -id;
	}

	@Override
	public boolean hasRight(int key) {
		return key < limit;
	}

	@Override
	public boolean hasRight(int key, int param) {
		return key < limit;
	}

	@Override
	public boolean isParametrized() {
		return false;
	}

	@Override
	public boolean isParametrized(int key) {
		return key < limit;
	}

}
