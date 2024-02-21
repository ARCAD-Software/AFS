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
package com.arcadsoftware.metadata.template;

import java.util.List;

/**
 * Templates are document that can contain references to entities values (using tags).
 *
 */
public interface ITemplate {

	public static final int MODIFICATIONTYPE_CREATION = 1;
	public static final int MODIFICATIONTYPE_MODIFICATION = 2;
	public static final int MODIFICATIONTYPE_DELETION = 4;
	public static final int MODIFICATIONTYPE_UNDELETION = 8;
	public static final int MODIFICATIONTYPE_LINK = 16;
	public static final int MODIFICATIONTYPE_UNLINK = 32;
	
	/**
	 * @return the reference entity type.
	 */
	public String getPrimaryType();
	
	/**
	 * @return optional auxiliary entity type.
	 */
	public String getSecondaryType();
	
	/**
	 * The body text.
	 * 
	 * @return
	 */
	public String getText();
	
	/**
	 * Document title.
	 * 
	 * @return
	 */
	public String getSubject();

	/**
	 * Attributes tested (if one condition is verified then the template is used).
	 * @return
	 */
	public List<ChangedAttribute> getChanged();
	
}
