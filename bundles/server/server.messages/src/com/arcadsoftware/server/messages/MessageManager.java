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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Properties;

import com.arcadsoftware.server.messages.internal.Activator;

/**
 * Manages retrieval and formatting of user messages based on message codes.
 * <p>
 * This class provides a centralized way to manage and localize user-facing messages using
 * a message catalog stored in {@link Properties} files. Messages can be parameterized and
 * support two levels of text (e.g., summary and details).
 * <p>
 * It is implemented as a singleton and supports loading multiple property files.
 */
public class MessageManager {
	
	private static final String SUFFIX_LVL1 = "$1"; //$NON-NLS-1$
	private static final String SUFFIX_LVL2 = "$2"; //$NON-NLS-1$

	private static final MessageManager instance = new MessageManager();
	private static final Properties properties = new Properties();

	/**
	 * Private constructor to enforce singleton pattern.
	 */
	private MessageManager() {}

	/**
	 * Returns the singleton instance of the {@code MessageManager}.
	 *
	 * @return the singleton instance
	 */
	public static MessageManager getInstance() {
		return instance;
	}

	/**
	 * Loads a message property file and adds its contents to the internal message catalog.
	 * <p>
	 * Properties from this file are merged into the current set.
	 *
	 * @param f the property file containing message definitions
	 * @throws FileNotFoundException if the file does not exist
	 * @throws IOException if an error occurs while reading the file
	 */
	public static void addPropertyFile(File f) throws FileNotFoundException, IOException {
		Properties p = new Properties();
		p.load(new FileInputStream(f));
		properties.putAll(p);			
	}

	/**
	 * Retrieves a formatted {@link UserMessage} based on a code and optional parameters.
	 * <p>
	 * The method looks up two properties: one for the main message text (suffix {@code $1}),
	 * and one for the detailed text (suffix {@code $2}). If the second is not defined,
	 * it defaults to an empty message. Parameters are substituted using {@link String#format}.
	 *
	 * @param code the message code
	 * @param vars optional parameters to format into the message
	 * @return a {@link UserMessage} containing the formatted message
	 */
	public static UserMessage getMessage(String code, Object... vars) {
		String prop1 = properties.getProperty(code + SUFFIX_LVL1);
		String prop2 = properties.getProperty(code + SUFFIX_LVL2);
		String msg1 = ""; //$NON-NLS-1$
		String msg2 = ""; //$NON-NLS-1$
		try {
			if (prop1 != null) {
				msg1 = String.format(prop1, vars);
			}
			if (prop2 != null) {	
				msg2 = String.format(prop2, vars);
			}
			if (msg1.isEmpty()) {
				Activator.logInfo("Missing Message Code = " + code, null);
			}
		} catch (Exception e) {
			Activator.logInfo("Message formating error [code: " + code + "]: " + e.getLocalizedMessage(), e);
		}
		return new UserMessage(code, msg1, msg2);
	}

	/**
	 * Retrieves a {@link UserMessage} based on a code and a {@link Throwable}.
	 * <p>
	 * The first level of text is set to the exception message, and the second level contains the stack trace.
	 *
	 * @param code the message code
	 * @param e the exception to include in the message
	 * @return a {@link UserMessage} with error details
	 */
	public static UserMessage getMessage(String code, Throwable e) {
		return new UserMessage(code, e.getLocalizedMessage(), stackTraceToString(e));
	}	

	/**
	 * Retrieves a {@link UserMessage} based on a code, a {@link Throwable}, and optional formatting parameters.
	 * <p>
	 * The first and second level texts are formatted using the message catalog and parameters, and the
	 * exception stack trace is appended to the second level.
	 *
	 * @param code the message code
	 * @param e the exception to include
	 * @param vars optional parameters to format into the message
	 * @return a {@link UserMessage} with both formatted text and exception details
	 */
	public static UserMessage getMessage(String code, Throwable e, Object... vars) {
		UserMessage m = getMessage(code, vars);
		m.setTextLevel2(m.getTextLevel2() + "\n" + stackTraceToString(e)); //$NON-NLS-1$
		return m;
	}

	/**
	 * Converts the stack trace of a {@link Throwable} into a string.
	 *
	 * @param e the throwable to convert
	 * @return the stack trace as a string
	 */
	private static String stackTraceToString(Throwable e) {
		StringWriter w = new StringWriter();
		e.printStackTrace(new PrintWriter(w));
		return w.toString();
	}
}
