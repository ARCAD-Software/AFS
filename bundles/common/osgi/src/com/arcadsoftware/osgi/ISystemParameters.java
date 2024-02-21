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
package com.arcadsoftware.osgi;

/**
 * System parameters key test service.
 * @since 1.0
 */
public interface ISystemParameters {

	/**
	 * OSGi service interface
	 */
	public static final String clazz = ISystemParameters.class.getName();

	/**
	 * Operating System Name parameter test.
	 */
	public static final int PARAMETER_OSNAME = 1;

	/**
	 * Operating System Architecture test.
	 */
	public static final int PARAMETER_OSARCH = 2;

	/**
	 * Operating System Version test.
	 */
	public static final int PARAMETER_OSVERSION = 4;

	/**
	 * Ehternet MAC address test.
	 */
	public static final int PARAMETER_MACADDRESS = 8;

	/**
	 * return the "system parameters" string. this is an hashed string given from the concat of the 4 testable
	 * parameters.
	 * 
	 * <p>
	 * This string is 25 characters long, formed by the following code :
	 * 
	 * <p>
	 * <b>XXXXXX-XXXXX-XXXXXX-XXXXX</b>
	 * 
	 * <p>
	 * where X characters are UUEncoded like, CRC32 from the 4 parameters.
	 * 
	 * @return a non null string.
	 */
	public String getSystemParameters();

	/**
	 * Test the given "Systam parameters" string with the current parameters according to the parameters to effectively
	 * test.
	 * 
	 * @param systemKey
	 *            The system parameters string to test.
	 * @param parameters
	 *            The parameters to test into the given string. Use a bit mask construct with PARAMETER_* constants.
	 * @return true if the string validate the test.
	 */
	public boolean testParameters(String systemKey, int parameters);

	/**
	 * Record the given test into the configuration and test it periodically. If the test fail the the OSGi framework is
	 * stoped.
	 * 
	 * @param systemKey
	 *            The system parameters string to test.
	 * @param parameters
	 *            The parameters to test into the given string. Use a bit mask construct with PARAMETER_* constants.
	 */
	public void storeParametersTest(String systemKey, int parameters);

}
