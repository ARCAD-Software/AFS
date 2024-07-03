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
package com.arcadsoftware.afs.client.core;

public interface IACCMessages {
	/**
	 * Search Error
	 */
	public static final String ERR_SRH_CONNECTIONMISSING = "COR-SRH-E000001"; //$NON-NLS-1$
	public static final String ERR_SRH_UNKNOWNERROR = "COR-SRH-E000002"; //$NON-NLS-1$

	/**
	 * Container Creation Error
	 */

	public static final String ERR_CONTAINER_UNKNOWN_ERROR = "COR-CTC-E000000"; //$NON-NLS-1$
	public static final String ERR_CONTAINER_INVALID_RIGHT = "COR-CTC-E000001"; //$NON-NLS-1$

	/**
	 * Action Rights not allowed
	 */
	public static final String ERR_ACTION_NO_RIGHT = "COR-ACT-E000000"; //$NON-NLS-1$

	public static final String ERR_CONNECTION_INVALID_URL = "COR-CTC-E000002"; //$NON-NLS-1$
	public static final String ERR_CONNECTION_INVALID_TRUSTSTORE = "COR-CTC-E000003"; //$NON-NLS-1$

}
