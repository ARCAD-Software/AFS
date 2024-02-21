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
package com.arcadsoftware.mail;

/**
 * 
 * @author ARCAD Software
 */
public interface IMailBoxTester {

	/**
	 * OGSi Service reference
	 */
	public static final String clazz = IMailBoxTester.class.getName();
	
	/**
	 * Test the parameters of a Mail Box.
	 * 
	 * <p>
	 * 
	 * @param server
	 * @param port
	 * @param type
	 * @param folderName
	 * @param login
	 * @param pwd
	 * @return Return a non null string if test is successful.
	 */
	public String testMailBox(String server, int port, String type, String folderName, String login, String pwd);
	
}
