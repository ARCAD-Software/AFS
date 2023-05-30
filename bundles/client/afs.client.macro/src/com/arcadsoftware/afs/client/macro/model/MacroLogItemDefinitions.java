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
package com.arcadsoftware.afs.client.macro.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;

import com.arcadsoftware.afs.client.macro.parsers.StandardMacrologParser;



public class MacroLogItemDefinitions extends ArrayList<MacroLogItemDefinition> {
	
	
	public static final String ATTR_MSGID ="id";//$NON-NLS-1$
	public static final String ATTR_MSGTYPE ="type";//$NON-NLS-1$
	public static final String ATTR_MSGSTS ="status";//$NON-NLS-1$
	public static final String ATTR_MSGSEVERITY ="severity";//$NON-NLS-1$
	public static final String ATTR_MSGSEQ ="sequence";//$NON-NLS-1$
	public static final String ATTR_MSGLEVEL1 ="level1";//$NON-NLS-1$
	public static final String ATTR_MSGLEVEL2 ="level2";//$NON-NLS-1$
	
	public static final String TAG_ITEM = "item";//$NON-NLS-1$
	
	
	private String macrolibrary;
	private String macroname;
		
	public MacroLogItemDefinitions(String macrolibrary, String macroname) {
		super();
		this.macrolibrary = macrolibrary;
		this.macroname = macroname;
	}
	
	public String getMacrolibrary() {
		return macrolibrary;
	}
	
	public String getMacroname() {
		return macroname;
	}
	
	public String getLabel() {
		return macrolibrary+"/"+macroname;
	}
	
	public void setMacrolibrary(String macrolibrary) {
		this.macrolibrary = macrolibrary;
	}
	
	public void setMacroname(String macroname) {
		this.macroname = macroname;
	}
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -2663624782961846458L;

	public static MacroLogItemDefinitions fromXml(String macrolibrary, String macroname,String xml){
		Reader xmlReader = new StringReader(xml);
		return fromXml(macrolibrary, macroname, xmlReader);
	}
	
	public static MacroLogItemDefinitions fromXml(String macrolibrary, String macroname,File xmlFile, IMacrologParser... parsers){
		try (Reader xmlReader = new InputStreamReader(new FileInputStream(xmlFile), "UTF-8")){						
			return fromXml(macrolibrary, macroname, xmlReader,parsers);
		}
		catch (Exception e) {
			return null;
		}		
	}	
	
	
	private static MacroLogItemDefinitions fromXml(String macrolibrary, String macroname,Reader reader, IMacrologParser... parsers){		
		MacroLogItemDefinitions macros = 
				new MacroLogItemDefinitions(macrolibrary, macroname);		
		IMacrologParser parser = null;
		if (parsers.length==0) {
			parser = new StandardMacrologParser();
		} else {
			parser = parsers[0];
		}		
		parser.parse(reader, macros);
		return macros;		
	}		
	 
	

}
