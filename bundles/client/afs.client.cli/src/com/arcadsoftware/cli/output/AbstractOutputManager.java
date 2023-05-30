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
package com.arcadsoftware.cli.output;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import com.arcadsoftware.cli.core.services.AbstractService;
import com.arcadsoftware.cli.logger.LogEntries;
import com.arcadsoftware.cli.logger.LogEntry;
import com.arcadsoftware.cli.logger.ServiceLogger;

public abstract class AbstractOutputManager {

	public static final String NODE_SERVICE = "service";
	public static final String ATTR_NAME ="name";
	public static final String ATTR_RESULT ="result";
	public static final String ATTR_VALUE="value";
	public static final String NODE_PARAMETERS = "parameters";
	public static final String NODE_PARAMETER = "parameter";
	public static final String NODE_LOGS = "logs";
	public static final String NODE_LOG = "log";
	public static final String ATTR_COUNT ="count";		
		
	
	public static final String ATTR_LOGDATE ="logDate";
	public static final String ATTR_LEVEL ="level";
	
	
	public static final SimpleDateFormat SP = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");

	
	protected File outputFile = null;
	protected AbstractService service = null;
	protected OutputNode root = null;

	public AbstractOutputManager () {
		root = new OutputNode(getRootName());
	}
	
	public AbstractOutputManager (AbstractService service , File outputFile) {
		this.outputFile = outputFile;
		this.service = service;
		root = new OutputNode(getRootName());
	}

	public File getOutputFile() {
		return outputFile;
	}


	public void setOutputFile(File outputFile) {
		this.outputFile = outputFile;
	}


	public AbstractService getService() {
		return service;
	}


	public void setService(AbstractService service) {
		this.service = service;
	}

	public String getRootName() {
		return NODE_SERVICE;
	}
	
	protected boolean isEmpty(String value){
		return (value==null) || (value.length()==0);
	}
	
	
	public void generateParameterSection(OutputNode parent) {
		OutputNode parameters = parent.addNode(NODE_PARAMETERS);
		HashMap<String, ArrayList<String>> params = service.getOptionTable();
		Iterator<String> it = params.keySet().iterator();
		while (it.hasNext()) {
			String id = it.next();
			String values[] = service.getOptionValues(id);
			for (int x = 0; x<values.length ;x++) {
				OutputNode parameter = new OutputNode(NODE_PARAMETER);
				parameters.addNode(parameter);
				parameter.addAttribute(ATTR_NAME, id);
				parameter.addAttribute(ATTR_VALUE, values[x]);
			}			
		}			
	}
	public void generateLogSection(OutputNode parent, LogEntries entries){
		OutputNode logs = parent.addNode(NODE_LOGS);
		logs.addAttribute(ATTR_COUNT, String.valueOf(entries.size()));
		for (LogEntry le : entries) {
			OutputNode log = logs.addNode(NODE_LOG);
			log.addAttribute(ATTR_LOGDATE, SP.format(le.getLogDate()));
			log.addAttribute(ATTR_LEVEL, String.valueOf(le.getLevel()));
			log.setText(isEmpty(le.getMessage()) ? "" : le.getMessage());		
		}			
	}
	
	
	
	public void generateOutput() {	
		root.addAttribute(ATTR_RESULT, String.valueOf(service.getResult()));
		generateParameterSection(root);
		LogEntries logs = ((ServiceLogger)service.getLogger()).getLogEntries();
		generateLogSection(root,logs);
		service.outputAdditionalInfo(root);
	}
	
	public OutputNode getRoot() {
		return root;
	}
	
	

}
