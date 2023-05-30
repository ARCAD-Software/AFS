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
package com.arcadsoftware.afs.client.core.internal.server.model;

import java.io.File;
import java.nio.charset.StandardCharsets;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.arcadsoftware.aev.core.tools.XMLTools;
import com.arcadsoftware.afs.client.core.internal.BaseActivator;
import com.arcadsoftware.afs.client.core.servers.model.IServerLoader;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.servers.model.Servers;
import com.arcadsoftware.crypt.Crypto;

public class FileServerLoader implements IServerLoader {
	
	private static final String FILE = "servers.xml"; //$NON-NLS-1$
	public static final String XMLNODE_ROOT = "servers"; //$NON-NLS-1$
	public static final String XMLNODE_SERVER = "server"; //$NON-NLS-1$
	public static final String XMLATTR_NAME = "name"; //$NON-NLS-1$	
	public static final String XMLATTR_URL = "url"; //$NON-NLS-1$
	public static final String XMLATTR_LOGIN = "login"; //$NON-NLS-1$
	public static final String XMLATTR_PWD = "pwd"; //$NON-NLS-1$
	public static final String XMLATTR_RETAIN = "retain"; //$NON-NLS-1$
	public static final String XMLATTR_PROXY_HOST = "proxyHost"; //$NON-NLS-1$
	public static final String XMLATTR_PROXY_PORT = "proxyPort"; //$NON-NLS-1$
	public static final String XMLATTR_PROXY_USER = "proxyUser"; //$NON-NLS-1$
	public static final String XMLATTR_PROXY_PASSWORD = "proxyPassword"; //$NON-NLS-1$
	
	private Document document;
	private Element root;
	private String fileName;
	
	public FileServerLoader() {
		String rootDirectory = BaseActivator.getDefault().getStateLocation().toOSString();
		fileName = rootDirectory + File.separator+FILE;
	}
	
	private boolean createDocument() {
		try {
			document = XMLTools.createNewXMLDocument();
			root = document.createElement(XMLNODE_ROOT);
			document.appendChild(root);
			XMLTools.writeXMLDocumentToFile(document, new File(fileName), StandardCharsets.UTF_8.name());
		    return true;
		} catch (Exception ex) {
			BaseActivator.getLogger().error(ex.getLocalizedMessage(), ex);
			return false;			
		}
	}
	
	private String getAttributeValue(Element e, String name) {
		final String value = e.getAttribute(name);
		if (value == null) {
			return "";
		}
    	return value;
	}		

	public boolean saveDocument(Servers servers) {
		try {
			document = XMLTools.createNewXMLDocument();
			root = document.createElement(XMLNODE_ROOT);
			document.appendChild(root);
			for (final Server server : servers) {
				final Element serverElement = document.createElement(XMLNODE_SERVER);
				root.appendChild(serverElement);
				serverElement.setAttribute(XMLATTR_NAME, server.getName());
				serverElement.setAttribute(XMLATTR_URL, server.getUrl());
				serverElement.setAttribute(XMLATTR_LOGIN, server.getLastLogin());
				if (server.isRememberPassword()) {
					if (server.getLastPassword() != null) {
						// FIXME This method us the Master key (on RCP side this key is not defined.
						serverElement.setAttribute(XMLATTR_PWD, Crypto.encrypt(server.getLastPassword().toCharArray()));
					}
					serverElement.setAttribute(XMLATTR_RETAIN, "true"); //$NON-NLS-1$
				} else {
					serverElement.setAttribute(XMLATTR_RETAIN, "false"); //$NON-NLS-1$
				}
			}
			XMLTools.writeXMLDocumentToFile(document, new File(fileName), StandardCharsets.UTF_8.name());					        
	        return true;
		} catch (Exception ex) {
			BaseActivator.getLogger().error(ex.getLocalizedMessage(), ex);
		}
		return false;
	}
	
	public Servers load() {
		final Servers servers = new Servers();
		final File f = new File(fileName);
		if (!f.exists()) {
			createDocument();
		}
        try {
			document = XMLTools.loadXMLFromFile(f);
			final NodeList serverNodes = document.getElementsByTagName(XMLNODE_SERVER);
	        for (int i = 0; i < serverNodes.getLength(); i++) {
	        	Element serverElement = (Element) serverNodes.item(i);
	        	Server server = new Server();
	        	server.setName(getAttributeValue(serverElement,XMLATTR_NAME));	        	
	        	server.setUrl(getAttributeValue(serverElement,XMLATTR_URL));	        	
	        	server.setLastLogin(getAttributeValue(serverElement,XMLATTR_LOGIN));
	        	if (serverElement.getAttribute(XMLATTR_PWD) != null) {
					// FIXME This method us the Master key (on RCP side this key is not defined.
	        		server.setLastPassword(new String(Crypto.decrypt(serverElement.getAttribute(XMLATTR_PWD))));
	        	}
	        	server.setRememberPassword("true".equalsIgnoreCase(getAttributeValue(serverElement, XMLATTR_RETAIN))); //$NON-NLS-1$
	        	servers.add(server);
	        }
		} catch (Exception e) {
			BaseActivator.getLogger().error(e.getLocalizedMessage(), e);
		}	
		return servers;
	}
	
	public boolean add(Server server) {
		Servers servers = load();
		servers.add(server);
		return saveDocument(servers);
	}	
	
	public boolean delete(Server server) {
		Servers servers = load();
		int index = -1;
		for (int i = 0; i < servers.size(); i++) {
			Server s = servers.get(i);
			if (s.getName().equalsIgnoreCase(server.getName())){
				index = i;
				break;
			}				
		}		
		if (index!=-1) {
			servers.remove(index);
		}
		return saveDocument(servers);
	}	
	
	public boolean update(Server server) {
		Servers servers = load();
		for (int i = 0; i < servers.size(); i++) {
			Server s = servers.get(i);
			if (s.getName().equalsIgnoreCase(server.getName())){
				s.assign(server);				
				break;
			}				
		}		
		return saveDocument(servers);
	}		
}
