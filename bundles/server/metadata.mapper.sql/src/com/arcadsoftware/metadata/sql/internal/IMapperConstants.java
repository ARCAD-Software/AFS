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
package com.arcadsoftware.metadata.sql.internal;

/**
 *
 */
public interface IMapperConstants {

	public static final String TYPE_STRING = "string"; //$NON-NLS-1$

	public static final String DEFAULT_TABLEALIAS = "x"; //$NON-NLS-1$

	public static final String ATTRIBUTE_MESSAGE = "message"; //$NON-NLS-1$
	public static final String ATTRIBUTE_CODE = "code"; //$NON-NLS-1$
	public static final String ATTRIBUTE_DATE = "date"; //$NON-NLS-1$

	public static final String SQL_JAVA_PREFIX = "j_"; //$NON-NLS-1$
	public static final String SQL_IDCOL = SQL_JAVA_PREFIX + "id"; //$NON-NLS-1$
	public static final String SQL_DATECOL = SQL_JAVA_PREFIX + "date"; //$NON-NLS-1$
	public static final String SQL_REFCOL = SQL_JAVA_PREFIX + "ref"; //$NON-NLS-1$
	public static final String SQL_DELETECOL = SQL_JAVA_PREFIX + "deleted"; //$NON-NLS-1$
	
}
