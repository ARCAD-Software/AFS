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
package com.arcadsoftware.rest.connection;

/**
 * The connection cache is used to temporary store validated user connections.
 * 
 * <p>This OSGi service allow to purge elements from this cache if they are no longer valid.
 */
public interface IConnectionCache {

	/**
	 * OSGi service SID.
	 */
	public static String clazz = IConnectionCache.class.getName();

	/**
	 * Remove the corresponding user from the cache. 
	 * 
	 * @param userID
	 */
	public void purge(String userType, int userID);

	/**
	 * Remove all user from the cache.
	 */
	public void purgeAll(String userType);

	/**
	 * Remove all user from the cache.
	 */
	public void purgeAll();
}
