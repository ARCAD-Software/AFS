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
package com.arcadsoftware.cli;

import java.io.File;
import java.util.Calendar;
import java.util.Properties;

/**
 * This class given an advanced access to the command line arguments.
 * 
 * @author ARCAD Software
 */
public interface ICommandLineArguments {

	/**
	 * Get all the applicative arguments.
	 * 
	 * <p>
	 * This exclude the JVM arguments, the OSGi framework arguments, and the command name.
	 * 
	 * @return a non null array.
	 */
	public String[] getArguments();
	
	/**
	 * Return true if one of the given arguments name is used in the actual argument list.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @return
	 */
	public boolean isArgument(String... arg);
	
	/**
	 * Get the first String Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public String getArgumentValue(String arg, String defaultValue);

	/**
	 * Get all the String Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public String[] getArgumentValues(String arg, String defaultValue);
	
	/**
	 * Get the first String Argument values corresponding to one of the Argument names. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument names are case insensitive.
	 * 
	 * @param args
	 * @param defaultValue
	 * @return
	 */
	public String getArgumentValue(String[] args, String defaultValue);

	/**
	 * Get all the String Argument values corresponding to one of the Argument names. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument names are case insensitive.
	 * 
	 * @param args
	 * @param defaultValue
	 * @return
	 */
	public String[] getArgumentValues(String[] args);
	
	/**
	 * Get the first int Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public int getArgumentValue(String arg, int defaultValue);
	
	/**
	 * Get the first String Argument value corresponding to one of the Argument names. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument names are case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public int getArgumentValue(String[] args, int defaultValue);
	
	/**
	 * Get the first boolean Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public boolean getArgumentValue(String arg, boolean defaultValue);
	
	/**
	 * Get the first File Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public File getArgumentValue(String arg, File defaultValue);
	
	/**
	 * Get the first File Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public File getArgumentValue(String[] arg, File defaultValue);
	
	/**
	 * Get the first Properties set Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The Argument value is here the path of the properties file.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public Properties getArgumentValue(String arg, Properties defaultValue);
	
	/**
	 * Get the first Properties set Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The Argument value is here the path of the properties file.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public Properties getArgumentValue(String[] arg, Properties defaultValue);

	/**
	 * Get the first calendar date Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public Calendar getArgumentValue(String arg, Calendar defaultValue);

	/**
	 * Get the first calendar date Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public Calendar getArgumentValue(String[] arg, Calendar defaultValue);
	
	/**
	 * Get the first password Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public char[] getArgumentValue(String arg, char[] defaultValue);

	/**
	 * Get the first password Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The value is assumed to be encoded with the FOG algorithm.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public char[] getArgumentFog(String arg, char[] defaultValue);

	/**
	 * Get the first password Argument value corresponding to the Argument name. The argument value is the first following
	 * argument or the second part of the argument if a arg=value or arg:value notation is used.
	 * 
	 * <p>
	 * The value is assumed to be encoded with one of the supported encryption algorithm.
	 * 
	 * <p>
	 * The argument name is case insensitive.
	 * 
	 * @param arg
	 * @param defaultValue
	 * @return
	 */
	public char[] getArgumentEncrypt(String arg, char[] defaultValue);
}
