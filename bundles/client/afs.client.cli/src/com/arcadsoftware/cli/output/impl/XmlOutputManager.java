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
package com.arcadsoftware.cli.output.impl;

import java.io.File;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.arcadsoftware.ae.core.utils.IXMLContentProvider;
import com.arcadsoftware.ae.core.utils.XMLUtils;
import com.arcadsoftware.cli.core.services.AbstractService;
import com.arcadsoftware.cli.output.AbstractOutputManager;
import com.arcadsoftware.cli.output.OutputAttribute;
import com.arcadsoftware.cli.output.OutputAttributes;
import com.arcadsoftware.cli.output.OutputNode;
import com.arcadsoftware.cli.output.OutputNodes;


public class XmlOutputManager extends AbstractOutputManager implements IXMLContentProvider {

	public XmlOutputManager() {
		super();		
	}
	
	public XmlOutputManager(AbstractService service, File outputFile) {
		super(service, outputFile);		
	}



	@Override
	public String getEncoding() {
		return "UTF-8";
	}

	
	private void transfer(Document document, Element parent, OutputNode outputNode) {
		Element xmlnode = document.createElement(outputNode.getName()); 
		parent.appendChild(xmlnode);
		OutputAttributes attributes = outputNode.getAttributes();
		for (OutputAttribute attribute : attributes) {
			xmlnode.setAttribute(attribute.getName(), attribute.getValue());
		}
		String text =outputNode.getText(); 
		if (text!=null) {
			xmlnode.setTextContent(text);
		}
		OutputNodes nodes = outputNode.getChildNodes();
		for (OutputNode node : nodes) {
			transfer(document, xmlnode, node);
		}
		
	}
	
	@Override
	public void provide(Document document, Element xmlroot) {
		//Manage root Node
		xmlroot.setAttribute(ATTR_NAME, root.getAttributeValue(ATTR_NAME));		
		xmlroot.setAttribute(ATTR_RESULT, root.getAttributeValue(ATTR_RESULT));
		
		//Manage Parameter Nodes
		OutputNode parameterNode = root.getNode(NODE_PARAMETERS);
		OutputNodes parameters = parameterNode.getNodes(NODE_PARAMETER);
		
		Element xmlParameters = document.createElement(NODE_PARAMETERS); 
		xmlroot.appendChild(xmlParameters);
		for (OutputNode parameter : parameters) {
			Element xmlParameter = document.createElement(NODE_PARAMETER); 
			xmlParameters.appendChild(xmlParameter);
			xmlParameter.setAttribute(ATTR_NAME, parameter.getAttributeValue(ATTR_NAME));
			xmlParameter.setAttribute(ATTR_VALUE,parameter.getAttributeValue(ATTR_VALUE));
			
		}
		//Manage Log Nodes
		OutputNode logNode = root.getNode(NODE_LOGS);
		OutputNodes logs = logNode.getNodes(NODE_LOG);
		Element xmlLogs = document.createElement(NODE_LOGS); 
		xmlroot.appendChild(xmlLogs);
		xmlLogs.setAttribute(ATTR_COUNT, logNode.getAttributeValue(ATTR_COUNT));		
		for (OutputNode log : logs) {
			Element xmlLog = document.createElement(NODE_LOG); 
			xmlLogs.appendChild(xmlLog);
			xmlLog.setAttribute(ATTR_LOGDATE, log.getAttributeValue(ATTR_LOGDATE));
			xmlLog.setAttribute(ATTR_LEVEL,log.getAttributeValue(ATTR_LEVEL));
			String text = log.getText();
			xmlLog.setTextContent(isEmpty(text) ? "" : text);			
		}
		//Manage Additional Information
		OutputNodes  childs = root.getChildNodes();
		for (OutputNode child : childs) {
			if ((!child.getName().equalsIgnoreCase(NODE_PARAMETERS)) && (!child.getName().equalsIgnoreCase(NODE_LOGS))) {
				transfer(document, xmlroot, child);
			}
		}
	}

	@Override
	public void generateOutput() {
		super.generateOutput();
		String outputFilename = outputFile.getAbsolutePath();
		if (!outputFile.exists()) {
			outputFile.getParentFile().mkdirs();
		}
		try {
			XMLUtils.createXmlDocument(outputFilename, this);
		} catch (Exception e) {}
	}
	
	
}
