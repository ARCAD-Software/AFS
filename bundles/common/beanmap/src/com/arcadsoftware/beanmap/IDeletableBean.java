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
package com.arcadsoftware.beanmap;

/**
 * Implement a soft-deletion mecanism.
 * 
 * <p>Deleted object should not be proceeded into standart processes, until they are undeleted.
 * 
 * @author ARCAD Software
 *
 */
public interface IDeletableBean {

	/**
	 * DELETED property code (reserved attribute code).
	 */
	public static final String KEY_DELETED = "deleted"; //$NON-NLS-1$
	
	/**
	 * 
	 * @return true if this object is deleted.
	 */
	public boolean isDeleted();
}
