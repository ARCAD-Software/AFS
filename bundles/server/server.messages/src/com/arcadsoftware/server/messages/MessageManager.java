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
import java.util.IllegalFormatConversionException;
import java.util.MissingFormatArgumentException;
import java.util.Properties;
import java.util.UnknownFormatConversionException;

/**
 * 
 */
public class MessageManager {
	
	private static String SUFFIX_LVL1 = "$1"; //$NON-NLS-1$
	private static String SUFFIX_LVL2 = "$2"; //$NON-NLS-1$

	private static final MessageManager instance = new MessageManager();
	private static final Properties properties = new Properties();

	private MessageManager() {}
	
	/**
	 * 
	 */
	public static MessageManager getInstance() {
		return instance;
	}
	
	/**
	 * 
	 */
	public static void addPropertyFile(File f) throws FileNotFoundException, IOException {
		Properties p = new Properties();
		p.load(new FileInputStream(f));
		properties.putAll(p);			
	}
	
	/**
	 * 
	 */
	public static UserMessage getMessage(String code,Object... vars){
		String prop1 = properties.getProperty(code + SUFFIX_LVL1);
		String prop2 = properties.getProperty(code + SUFFIX_LVL2);
		String msg1 = ""; //$NON-NLS-1$
		String msg2 = ""; //$NON-NLS-1$
		if (prop2 == null) {
			prop2 = prop1;	
		}
		try{
			if (prop1 != null) {
				msg1 = String.format(prop1,vars);
			}
			if (prop2 != null) {	
				msg2 = String.format(prop2,vars);
			}
			if (msg1.isEmpty()) {
				System.out.println("MESSAGE:Missing Code - Code = " + code); //$NON-NLS-1$
			}
		} catch (MissingFormatArgumentException e) {
			System.out.println("MESSAGE:MissingFormatArgumentException - Code = " + code); //$NON-NLS-1$
		}  catch (IllegalFormatConversionException e) {
			System.out.println("MESSAGE:IllegalFormatConversionException - Code = " + code); //$NON-NLS-1$
		} catch (UnknownFormatConversionException e) {
			System.out.println("MESSAGE:IllegalFormatConversionException - Code = " + code); //$NON-NLS-1$
		}
		return new UserMessage(code,msg1,msg2);
	}
	
	/**
	 * 
	 */
	public static UserMessage getMessage(String code, Throwable e) {
		return new UserMessage(code, e.getLocalizedMessage(), stackTraceToString(e));
	}	
	
	/**
	 * 
	 */
	public static UserMessage getMessage(String code, Throwable e, Object... vars) {
		UserMessage m = getMessage(code, vars);
		m.setTextLevel2(m.getTextLevel2() + "\n" + stackTraceToString(e)); //$NON-NLS-1$
		return m;
	}	
	
	/**
	 * 
	 */
	private static String stackTraceToString(Throwable e) {
		StringWriter w = new StringWriter();
		e.printStackTrace(new PrintWriter(w));
		return w.toString();
	}
}
