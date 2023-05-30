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
package com.arcadsoftware.afs.framework.messages;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Properties;

public abstract class AbstractUserMessageManager {

	private static String SUFFIX_LVL1 = "$1";//$NON-NLS-1$
	private static String SUFFIX_LVL2 = "$2";//$NON-NLS-1$	
	private static Properties properties;
	
	protected AbstractUserMessageManager() {
		this(true);
	}
	
	protected AbstractUserMessageManager(boolean init) {
		super();
		properties = new Properties();
		if (init) {
			load();
		}
	}
	
	protected void load() {
		// Nothing to do.
	}
	
	public void addPropertyFile(File f) throws FileNotFoundException, IOException {
		Properties p = new Properties();
		p.load(new FileInputStream(f));
		properties.putAll(p);			
	}
	
	public void addProperties(Properties props) {
		properties.putAll(props);			
	}
	
	public UserMessage getMessage(String code, Object... vars) {
		String prop1 = properties.getProperty(code + SUFFIX_LVL1);
		String prop2 = properties.getProperty(code + SUFFIX_LVL2);
		String msg1 = ""; //$NON-NLS-1$
		String msg2 = ""; //$NON-NLS-1$
		if ((vars != null) && (vars.length >0)) {
			if (prop1!=null) {
				msg1 = String.format(prop1,vars);
			}
			if (prop2!=null) {
				msg2 = String.format(prop2,vars);
			}
		} else {
			if (prop1 != null) {
				msg1 = prop1;
			}
			if (prop2 != null) {
				msg2 = prop2;
			}
		}
		return new UserMessage(code,msg1,msg2);
	}
	
	public UserMessage getMessage(String code, Throwable e){
		return new UserMessage(code,e.getLocalizedMessage(), stackTrace(e));
	}	
	
	public UserMessage getMessage(String code, Throwable e,Object... vars){
		UserMessage m = getMessage(code,vars);
		String level2 = m.getTextLevel2();
		level2 += '\n' + stackTrace(e);
		m.setTextLevel2(level2);
		return m;
	}

	private String stackTrace(Throwable e) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		e.printStackTrace(pw);
		return sw.toString();
	}
	
}
