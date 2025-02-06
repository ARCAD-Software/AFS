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
package com.arcadsoftware.cli.core.services;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import org.w3c.dom.Element;

import com.arcadsoftware.ae.core.utils.IXMLContentParser;
import com.arcadsoftware.ae.core.utils.XMLUtils;
import com.arcadsoftware.cli.logger.ServiceLogger;
import com.arcadsoftware.cli.model.LoggedObject;

public class ServiceFactory extends LoggedObject implements IXMLContentParser {

	private final HashMap<String, String> registry;

	private static ServiceFactory instance = new ServiceFactory();

	private ServiceFactory() {
		registry = new HashMap<>();
	}

	public void initialize(String filename)
			throws Throwable {
		XMLUtils.loadXmlDocument(filename, this);
	}

	public AbstractService createService(String[] args)
			throws Throwable {
		if (args.length == 0) {
			return null;
		}
		final String serviceIdentifier = args[0];
		final String[] options = new String[args.length - 1];
		System.arraycopy(args, 1, options, 0, options.length);
		final String className = registry.get(serviceIdentifier);
		if (className != null) {
			final AbstractService service = (AbstractService) Class.forName(className).newInstance();
			if (service != null) {
				service.setServiceName(serviceIdentifier);
				service.setOptions(options);
				logger = new ServiceLogger();
				service.setLogger(logger);
				return service;
			}
			throw new Exception("An Error occured during " + className + " instanciation");
		}
		throw new Exception("Invalid Service Identifier");
	}

	@Override
	public String getRootName() {
		return "service"; //$NON-NLS-1$
	}

	@Override
	public boolean parse(Element element) {
		final String identifier = element.getAttribute("id"); //$NON-NLS-1$
		final String classname = element.getAttribute("class"); //$NON-NLS-1$
		registry.put(identifier, classname);
		return true;
	}

	public String getServiceList() {
		final StringBuilder list = new StringBuilder();
		final Set<String> keys = registry.keySet();
		final Iterator<String> it = keys.iterator();
		while (it.hasNext()) {
			if (list.length() == 0) {
				list.append(it.next());
			} else {
				list.append('\n');
				list.append(it.next());
			}
		}
		return list.toString();
	}

	public static ServiceFactory getInstance() {
		return instance;
	}
}
