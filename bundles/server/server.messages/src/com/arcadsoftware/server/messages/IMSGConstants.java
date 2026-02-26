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
 * Defines constants used for message representation and handling.
 * <p>
 * These constants include entity names, message property keys, and values for message levels and status.
 * They are typically used in structured logging, messaging frameworks, or for serializing messages
 * (e.g., to JSON or XML).
 * 
 * @author ARCAD Software
 */
public interface IMSGConstants {
	
	/**
	 * Entity name for a generic message.
	 */
	public final static String ENTITY_MESSAGE = "message"; //$NON-NLS-1$
	
	/**
	 * Entity name for a status message.
	 */
	public final static String ENTITY_STATUSMESSAGE = "statusmessage"; //$NON-NLS-1$
	
	/**
	 * Key for additional message details.
	 */
	public final static String MESSAGE_DETAILS = "details"; //$NON-NLS-1$
	
	/**
	 * Key for the message code (usually a unique identifier for the message template).
	 */
	public final static String MESSAGE_CODE = "code";	//$NON-NLS-1$
	
	/**
	 * Key for the primary text level of the message (main description).
	 */
	public final static String MESSAGE_TXTLVL1 = "textLevel1"; //$NON-NLS-1$
	
	/**
	 * Key for the secondary text level of the message (additional explanation).
	 */
	public final static String MESSAGE_TXTLVL2 = "textLevel2"; //$NON-NLS-1$
	
	/**
	 * Key indicating the severity level of the message (error, warning, etc.).
	 */
	public final static String MESSAGE_LEVEL = "level"; //$NON-NLS-1$
	
	/**
	 * Key indicating the execution or processing status (OK, FAILED, N/A).
	 */
	public final static String MESSAGE_STATUS = "status"; //$NON-NLS-1$

	/**
	 * Status value indicating successful processing.
	 */
	public final static int MESSAGE_STATUS_OK = 0;
	
	/**
	 * Status value indicating processing failure.
	 */
	public final static int MESSAGE_STATUS_FAILED = 1;
	
	/**
	 * Status value indicating that the status is not applicable.
	 */
	public final static int MESSAGE_STATUS_NA = 2;
	
	/**
	 * Severity level representing an error message.
	 */
	public final static int MESSAGE_LEVEL_ERROR = 0; // error
	
	/**
	 * Severity level representing a warning message.
	 */
	public final static int MESSAGE_LEVEL_WARNING = 1; // warning
	
	/**
	 * Severity level representing an informational message.
	 */
	public final static int MESSAGE_LEVEL_INFO = 2; // info
	
	/**
	 * Severity level representing a verbose/debug message.
	 */
	public final static int MESSAGE_LEVEL_VERBOSE = 3; // verbose
	
	/**
	 * Severity level indicating that the level is not applicable.
	 */
	public final static int MESSAGE_LEVEL_NA = 4; // n/a
}