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
package com.arcadsoftware.server.messages;

/**
 * 
 */
public interface IMSGConstants {
	
	/**
	 * 
	 */
	public final static String ENTITY_MESSAGE = "message"; //$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String ENTITY_STATUSMESSAGE = "statusmessage"; //$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String MESSAGE_DETAILS = "details"; //$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String MESSAGE_CODE = "code";	//$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String MESSAGE_TXTLVL1 = "textLevel1"; //$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String MESSAGE_TXTLVL2 = "textLevel2"; //$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String MESSAGE_LEVEL = "level"; //$NON-NLS-1$
	
	/**
	 * 
	 */
	public final static String MESSAGE_STATUS = "status"; //$NON-NLS-1$

	
	/**
	 * 
	 */
	public final static int MESSAGE_STATUS_OK = 0;
	
	/**
	 * 
	 */
	public final static int MESSAGE_STATUS_FAILED = 1;
	
	/**
	 * 
	 */
	public final static int MESSAGE_STATUS_NA = 2;
	
	/**
	 * 
	 */
	public final static int MESSAGE_LEVEL_ERROR = 0; //error;
	
	/**
	 * 
	 */
	public final static int MESSAGE_LEVEL_WARNING = 1; //warning;
	
	/**
	 * 
	 */
	public final static int MESSAGE_LEVEL_INFO = 2; //info;
	
	/**
	 * 
	 */
	public final static int MESSAGE_LEVEL_VERBOSE = 3; //verbose;
	
	/**
	 * 
	 */
	public final static int MESSAGE_LEVEL_NA = 4; //n/a;
	
}