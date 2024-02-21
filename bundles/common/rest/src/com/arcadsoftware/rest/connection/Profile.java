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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

/**
 * This class is a right collection. 
 * 
 * <p>
 * It is build from the union of the profiles possessed by the user.
 */
public class Profile {

	private final HashSet<Right> rights = new HashSet<Right>();
	private transient ArrayList<Right> params;
	private transient IConnectionUserBean owner;
	
	/**
	 * Create an empty Profile.
	 */
	public Profile() {
		super();
	}
	
	/**
	 * Create a Profile.
	 * 
	 * @param list A list of access rights.
	 */
	public Profile(List<Right> list) {
		this();
		rights.addAll(list);
		buildKeysTable();
	}

	/**
	 * Create a Profile.
	 * 
	 * @param user A connected User.
	 * @param list A list of access rights.
	 */
	public Profile(IConnectionUserBean user, List<Right> list) {
		this(list);
		owner = user;
	}
	
	private void buildKeysTable() {
		if (params == null) {
			params = new ArrayList<Right>();
		} else {
			params.clear();
		}
		if (rights != null) {
			for(Right right : rights) {
				if (right.getParam() > 0) {
					params.add(right);
				}
			}
		}
	}

	/**
	 * Test a general right (with no parameters, value <1025).
	 * 
	 * @param key
	 * @return
	 */
	public boolean hasRight(int key) {
		return (rights != null) && rights.contains(new Right(key));
	}

	/**
	 * Test a right with parameters.
	 * 
	 * @param key the right ID. if the key is null or negative then any parameterized Right is searched.
	 * @param params any supplied object that can fulfill the request parameter type. If null or 
	 *   negative only the key is searched.
	 * @return True if the right is satisfied.
	 */
	public boolean hasRight(int key, int param) {
		if (param <= 0) {
			return hasRight(key);
		}
		if (params == null) {
			buildKeysTable();
		}
		if (key <= 0) {
			for (Right right: params) {
				if (param == right.getParam()) {
					return true;
				}
			}
		} else {
			for (Right right: params) {
				if ((param == right.getParam()) && (key == right.getId())) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Add or update a right.
	 * 
	 * @param right
	 */
	public void addRight(Right right) {
		rights.remove(right);
		rights.add(right);
		if (right.getParam() > 0) {
			params.add(right);
		}
	}
	
	/**
	 * @param owner the owner to set
	 */
	public void setOwner(IConnectionUserBean owner) {
		this.owner = owner;
	}

	/**
	 * @return the owner
	 */
	public IConnectionUserBean getOwner() {
		return owner;
	}
	
	/**
	 * @return true if this profile contain any parameterized right
	 */
	public boolean isParametrized() {
		return params.size() > 0;
	}
	
	/**
	 * 
	 * @param key
	 * @return true if this profile contain the specified parameterized right
	 */
	public boolean isParametrized(int key) {
		for (Right right: params) {
			if (right.getId() == key) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * @return all parameterized rights 
	 */
	public Collection<Right> getParams() {
		if (params == null) {
			buildKeysTable();
		}
		return params;
	}
	
	/**
	 * 
	 * @param key
	 * @return the specified parameterized rights
	 */
	public Collection<Right> getParams(int key) {
		if (params == null) {
			buildKeysTable();
		}
		ArrayList<Right> result = new ArrayList<Right>();
		for (Right right: params) {
			if (right.getId() == key) {
				result.add(right);
			}
		}
		return result;
	}
}
