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
package com.arcadsoftware.afs.client.core.connection;

public interface ITrustStoreProvider {

	public String getTrustStorePath();
	/**
	 * 
	 * @return
	 * @deprecated Using String to store password is not secure.
	 */
	@Deprecated 
	public String getTrustStorePassword();
	
	public String getKeyStorePath();

	/**
	 * 
	 * @return
	 * @deprecated Using String to store password is not secure.
	 */
	@Deprecated
	public String getKeyStorePassword();
	
	public void setTrustStorePath(String path);
	
	/**
	 * 
	 * @param path
	 * @deprecated Using String to store password is not secure.
	 */
	@Deprecated
	public void setTrustStorePassword(String path);
	
	public void setKeyStorePath(String path);
	
	/**
	 * 
	 * @param path
	 * @deprecated Using String to store password is not secure.
	 */
	@Deprecated
	public void setKeyStorePassword(String path);
	
	public void resetToDefault();
	
	public boolean save();
	
}
