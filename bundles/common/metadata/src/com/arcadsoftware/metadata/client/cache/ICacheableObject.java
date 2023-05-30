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
package com.arcadsoftware.metadata.client.cache;

import java.io.Serializable;
import java.util.Date;

/**
 * Internal Interface.
 * 
 * Define cacheable object.
 * 
 * 1. Contain the last modification date.
 * 2. Contain the last test date.
 * 3. Some object are lazy (we do not test it if they are in a cache).
 * 4. Contain the data object (type T).
 * 5. Listable Object will cache their contain under a sub key.
 */
public interface ICacheableObject extends Serializable {

	public Object getContent();
	
	public void setContent(Object object);
	
	public Date getLastModification();
	
	public void setLastModification(Date date);
	
	public void setLastTest(Date date);
	
	public Date getLastTest();
	
	public boolean isCachableList();
	
	public void setCachableList(boolean list);
	
	public boolean isLazzy();
	
	public void setLazzy(boolean lazzy); 
}
