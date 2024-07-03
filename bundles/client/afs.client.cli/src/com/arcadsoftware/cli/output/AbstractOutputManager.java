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
package com.arcadsoftware.cli.output;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;

import com.arcadsoftware.cli.core.services.AbstractService;
import com.arcadsoftware.cli.logger.LogEntries;
import com.arcadsoftware.cli.logger.LogEntry;
import com.arcadsoftware.cli.logger.ServiceLogger;

public abstract class AbstractOutputManager {

	public static final String NODE_SERVICE = "service"; //$NON-NLS-1$
	public static final String ATTR_NAME = "name"; //$NON-NLS-1$
	public static final String ATTR_RESULT = "result"; //$NON-NLS-1$
	public static final String ATTR_VALUE = "value"; //$NON-NLS-1$
	public static final String NODE_PARAMETERS = "parameters"; //$NON-NLS-1$
	public static final String NODE_PARAMETER = "parameter"; //$NON-NLS-1$
	public static final String NODE_LOGS = "logs"; //$NON-NLS-1$
	public static final String NODE_LOG = "log"; //$NON-NLS-1$
	public static final String ATTR_COUNT = "count"; //$NON-NLS-1$
	public static final String ATTR_LOGDATE = "logDate"; //$NON-NLS-1$
	public static final String ATTR_LEVEL = "level"; //$NON-NLS-1$
	public static final SimpleDateFormat SP = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS"); //$NON-NLS-1$

	protected File outputFile;
	protected AbstractService service;
	protected OutputNode root;

	public AbstractOutputManager() {
		root = new OutputNode(getRootName());
	}

	public AbstractOutputManager(AbstractService service, File outputFile) {
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

	protected boolean isEmpty(String value) {
		return (value == null) || (value.length() == 0);
	}

	public void generateParameterSection(OutputNode parent) {
		final OutputNode parameters = parent.addNode(NODE_PARAMETERS);
		final HashMap<String, ArrayList<String>> params = service.getOptionTable();
		for (final String id : params.keySet()) {
			final String values[] = service.getOptionValues(id);
			for (final String value : values) {
				final OutputNode parameter = new OutputNode(NODE_PARAMETER);
				parameters.addNode(parameter);
				parameter.addAttribute(ATTR_NAME, id);
				parameter.addAttribute(ATTR_VALUE, value);
			}
		}
	}

	public void generateLogSection(OutputNode parent, LogEntries entries) {
		final OutputNode logs = parent.addNode(NODE_LOGS);
		logs.addAttribute(ATTR_COUNT, String.valueOf(entries.size()));
		for (final LogEntry le : entries) {
			final OutputNode log = logs.addNode(NODE_LOG);
			log.addAttribute(ATTR_LOGDATE, SP.format(le.getLogDate()));
			log.addAttribute(ATTR_LEVEL, String.valueOf(le.getLevel()));
			log.setText(isEmpty(le.getMessage()) ? "" : le.getMessage());
		}
	}

	public void generateOutput() {
		root.addAttribute(ATTR_RESULT, String.valueOf(service.getResult()));
		generateParameterSection(root);
		final LogEntries logs = ((ServiceLogger) service.getLogger()).getLogEntries();
		generateLogSection(root, logs);
		service.outputAdditionalInfo(root);
	}

	public OutputNode getRoot() {
		return root;
	}

}
