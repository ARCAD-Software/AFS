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
package com.arcadsoftware.database;

/**
 * Event thrown when a Data Source is added or removed from the platform.
 */
public interface IDataSourceEvent {

	public static final String TOPIC_PREFIX = "com/arcadsoftware/database/"; //$NON-NLS-1$
	public static final String TOPIC_ALL = TOPIC_PREFIX + "*"; //$NON-NLS-1$
	public static final String TOPIC_ADD = TOPIC_PREFIX + "add"; //$NON-NLS-1$
	public static final String TOPIC_REMOVE = TOPIC_PREFIX + "remove"; //$NON-NLS-1$
	
	public static final String PROP_DS = "datasource"; //$NON-NLS-1$
	public static final String PROP_DSID = "datasource.id"; //$NON-NLS-1$
	public static final String PROP_DSTYPE = "datasource.type"; //$NON-NLS-1$
	public static final String PROP_DSDIALECT = "datasource.dialect"; //$NON-NLS-1$
	public static final String PROP_DSURL = "datasource.url"; //$NON-NLS-1$
	
}
