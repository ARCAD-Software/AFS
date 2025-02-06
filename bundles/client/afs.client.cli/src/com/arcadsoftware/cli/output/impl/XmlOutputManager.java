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
		final Element xmlnode = document.createElement(outputNode.getName());
		parent.appendChild(xmlnode);
		final OutputAttributes attributes = outputNode.getAttributes();
		for (final OutputAttribute attribute : attributes) {
			xmlnode.setAttribute(attribute.getName(), attribute.getValue());
		}
		final String text = outputNode.getText();
		if (text != null) {
			xmlnode.setTextContent(text);
		}
		final OutputNodes nodes = outputNode.getChildNodes();
		for (final OutputNode node : nodes) {
			transfer(document, xmlnode, node);
		}
	}

	@Override
	public void provide(Document document, Element xmlroot) {
		// Manage root Node
		xmlroot.setAttribute(ATTR_NAME, root.getAttributeValue(ATTR_NAME));
		xmlroot.setAttribute(ATTR_RESULT, root.getAttributeValue(ATTR_RESULT));
		// Manage Parameter Nodes
		final OutputNode parameterNode = root.getNode(NODE_PARAMETERS);
		final OutputNodes parameters = parameterNode.getNodes(NODE_PARAMETER);
		final Element xmlParameters = document.createElement(NODE_PARAMETERS);
		xmlroot.appendChild(xmlParameters);
		for (final OutputNode parameter : parameters) {
			final Element xmlParameter = document.createElement(NODE_PARAMETER);
			xmlParameters.appendChild(xmlParameter);
			xmlParameter.setAttribute(ATTR_NAME, parameter.getAttributeValue(ATTR_NAME));
			xmlParameter.setAttribute(ATTR_VALUE, parameter.getAttributeValue(ATTR_VALUE));
		}
		// Manage Log Nodes
		final OutputNode logNode = root.getNode(NODE_LOGS);
		final OutputNodes logs = logNode.getNodes(NODE_LOG);
		final Element xmlLogs = document.createElement(NODE_LOGS);
		xmlroot.appendChild(xmlLogs);
		xmlLogs.setAttribute(ATTR_COUNT, logNode.getAttributeValue(ATTR_COUNT));
		for (final OutputNode log : logs) {
			final Element xmlLog = document.createElement(NODE_LOG);
			xmlLogs.appendChild(xmlLog);
			xmlLog.setAttribute(ATTR_LOGDATE, log.getAttributeValue(ATTR_LOGDATE));
			xmlLog.setAttribute(ATTR_LEVEL, log.getAttributeValue(ATTR_LEVEL));
			final String text = log.getText();
			xmlLog.setTextContent(isEmpty(text) ? "" : text);
		}
		// Manage Additional Information
		final OutputNodes childs = root.getChildNodes();
		for (final OutputNode child : childs) {
			if ((!child.getName().equalsIgnoreCase(NODE_PARAMETERS))
					&& (!child.getName().equalsIgnoreCase(NODE_LOGS))) {
				transfer(document, xmlroot, child);
			}
		}
	}

	@Override
	public void generateOutput() {
		super.generateOutput();
		final String outputFilename = outputFile.getAbsolutePath();
		if (!outputFile.exists()) {
			outputFile.getParentFile().mkdirs();
		}
		try {
			XMLUtils.createXmlDocument(outputFilename, this);
		} catch (final Exception e) {}
	}
}
