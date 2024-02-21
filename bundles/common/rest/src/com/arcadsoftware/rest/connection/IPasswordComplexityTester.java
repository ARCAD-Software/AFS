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
package com.arcadsoftware.rest.connection;

import org.restlet.data.Language;

/**
 * OSGi service interface used to test passwords complexity.
 * 
 */
public interface IPasswordComplexityTester {

	public static final String clazz = IPasswordComplexityTester.class.getName();

	public static final int REASON_OK = 0;	
	public static final int REASON_TOOSHORT = 1;	
	public static final int REASON_TOOLONG = 2;	
	public static final int REASON_NOTENOUGHDIGIT = 4;	
	public static final int REASON_NOTENOUGHALPHA = 8;
	public static final int REASON_TOOMANYLOGINCHAR = 16;	
	public static final int REASON_SAMEASLOGIN = 32;	
	public static final int REASON_SAMEASOLD = 64;	
	public static final int REASON_NOTENOUGHDIFFCHAR = 128;	
	public static final int REASON_NOTENOUGHNONALPHA = 256;
	public static final int REASON_TOOMANYOLDCHAR = 512;
	public static final int REASON_UNKNOWN = 1024;
	public static final int REASON_NOTENOUGHLOWERCASE = 2048;
	public static final int REASON_NOTENOUGHUPPERCASE = 4096;
	public static final int REASON_TOOMANYREPEATING = 8192;
	public static final int REASON_BLACKLISTED = 16384;
	/**
	 * Test the given password.
	 * 
	 * @param login
	 * @param oldPassword
	 * @param newPassword
	 * @return zero if the newPassword is acceptable.
	 */
	public int isPasswordAcceptable(String login, char[] oldPassword, char[] newPassword);

	/**
	 * Translate a numeric password error into the given user understandable message.
	 * 
	 * @param reason
	 * @return
	 */
	public String getTextualReason(int reason, Language language);
	
	/**
	 * Generate an acceptable password.
	 * 
	 * @param login Required for login-to-password comparison.
	 * @param oldPassword Required for password-to-password comparison.
	 * @return null if the generator can not provide an acceptable password.
	 */
	public char[] generateAcceptablePassword(String login, char[] oldPassword);
}
