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
package com.arcadsoftware.beanmap.rest;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import org.restlet.data.MediaType;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.StreamRepresentation;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.rest.FileMediaType;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.rest.WebServiceAccess;

/**
 * 
 * Facility Class used to get BeanMaps from web services.
 * 
 * <p>
 * If Metadata entities are used into the current OSGi platforms you should consider to use
 * the class <code>com.arcadsoftware.metadata.client.DataAccess</code>. Most of the method
 * declared into this class are not compatible with MetaData services (where the type
 * of the beanMap is not equal to the root relative address of the service). 
 * 
 * @author ARCAD Software
 */
public class BeanMapWebAccess {

	private WebServiceAccess wsa;
	private final XmlBeanMapStream xs;

	/**
	 * Default constructor.
	 * 
	 * <p>
	 * The WebServiceAccess object should be set before this object can be used.
	 */
	public BeanMapWebAccess() {
		super();
		xs = new XmlBeanMapStream(BeanMapWebAccess.class.getClassLoader());
	}

	/**
	 * Create a BeanMapWebAccess in front of an existing Web-service connection.
	 * Add a class loader to be able to manage embedded Java object classes into the BeanMap.
	 *  
	 * @param wsa The Web services Access API
	 * @param classLoader The class loader used for XML serialization. 
	 */
	public BeanMapWebAccess(WebServiceAccess wsa, ClassLoader classLoader) {
		super();
		xs = new XmlBeanMapStream(classLoader);
		setWebServiceAccess(wsa);
	}

	/**
	 * Create a BeanMapWebAccess in front of an existing Web-service connection.
	 * 
	 * @param wsa the Web services Access API
	 */
	public BeanMapWebAccess(WebServiceAccess wsa) {
		this();
		setWebServiceAccess(wsa);
	}

	/**
	 * Create a new BeanMapWebAccess from the connection parameters.
	 * 
	 * @param serveraddress The server URL
	 * @param login User login to access to secured services.
	 * @param password User password to access to secured services.
	 */
	public BeanMapWebAccess(String serverAddress, String login, char[] password) {
		this(new WebServiceAccess(null, serverAddress, login, password));
	}

	/**
	 * Access the BeanMap XML Serializer. This object is used to get the XML responses from the
	 * web-services.
	 * @return
	 */
	public XmlBeanMapStream getXStream() {
		return xs;
	}
	
	/**
	 * @param wsa the new Web services Access API
	 */
	public void setWebServiceAccess(WebServiceAccess wsa) {
		this.wsa = wsa;
	}

	/**
	 * @return the current Web Services Access API
	 */
	public WebServiceAccess getWebServiceAccess() {
		return wsa;
	}
	
	/**
	 * Get a BeanMap from a web-service.
	 * 
	 * @param address The web-service URI.
	 * @param type The BeanMap type to get.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap get(String address, String type) throws ServerErrorException {
		return (BeanMap)xs.fromXML(type, wsa.get(address));
	}

	/**
	 * Get a BeanMap from a web-service, where the web-service URI is equal to "type/id".
	 * 
	 * @param type The BeanMap type to get.
	 * @param id The identifier of the BeanMap.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap get(String type, int id) throws ServerErrorException {
		return (BeanMap)xs.fromXML(type, wsa.get(type + '/' + id));
	}
	
	/**
	 * Load a BeanMap from a web-service, where the web-service URI is equal to "type/id".
	 * 
	 * <p>
	 * REplace any of the existing properties into the current BeanMap with the 
	 * new ones.
	 * 
	 * @param bean A BeanMap to load, must have a type and and identifier.
	 * @return The <code>bean</code> itself.
	 * @throws ServerErrorException
	 */
	public BeanMap get(BeanMap bean) throws ServerErrorException {
		bean.addAll((BeanMap)xs.fromXML(bean.getType(), wsa.get(bean.getType() + '/' + bean.getId())));
		return bean;
	}
	
	/**
	 * Get a BeanMap list from the given URI.
	 * 
	 * @param address The web-service URI.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMapList getList(String address) throws ServerErrorException {
		return (BeanMapList)xs.fromXML(wsa.get(address));
	}

	/**
	 * Get a BeanMap list from the given URI.
	 * 
	 * @param address The web-service URI.
	 * @param params The parameters to pass to the web-service.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMapList getList(String address, Map<String, Object> params) throws ServerErrorException {
		return (BeanMapList)xs.fromXML(wsa.get(address, params));
	}

	/**
	 * Get an "untyped" BeanMap from a web-service. the BeanMap xml format must use the generic format,
	 * with a tag "item" to start the BeanMap definition.
	 *  
	 * @param address The web-service URI.
	 * @param params The parameters to pass to the web-service.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap get(String address, Map<String, Object> params) throws ServerErrorException {
		return (BeanMap)xs.fromXML(wsa.get(address, params));
	}
	
	/**
	 * Get a BeanMap from a Web-Service.
	 * 
	 * @param address The web-service URI.
	 * @param type the BeanMap type to retrieve.
	 * @param params The parameters to pass to the web-service.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap get(String address, String type, Map<String, Object> params) throws ServerErrorException {
		return (BeanMap)xs.fromXML(type, wsa.get(address, params));
	}
	
	/**
	 * Update a BeanMap.
	 * 
	 * @param address The web-service URI.
	 * @param bean the new values to pass through the web-service.
	 * @throws ServerErrorException
	 * @see {@link #getList(String)}
	 */
	public void put(String address, BeanMap bean) throws ServerErrorException {
		wsa.put(address, bean);
	}

	/**
	 * Update a BeanMap through a web-service, where the web-service URI is equal to "type/id".
	 * 
	 * @param bean The BeanMap to update
	 * @throws ServerErrorException
	 * @see {@link #get(BeanMap)}
	 */
	public void put(BeanMap bean) throws ServerErrorException {
		put(bean.getType() + '/' + bean.getId(), bean);
	}
	
	/**
	 * Create a BeanMap represented Resource through a web-service.
	 * 
	 * @param address The web-service URI.
	 * @param bean The BeanMap to create.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap post(String address, BeanMap bean) throws ServerErrorException {
		String xml = wsa.post(address, bean);
		if ((xml != null) && (xml.length() > 0)) {
			return (BeanMap)xs.fromXML(bean.getType(), xml);
		}
		return null;
	}
	
	/**
	 * Create a BeanMap represented Resource through a web-service. The Result of this
	 * creation is another BeanMap with a different type.
	 * 
	 * @param address The web-service URI.
	 * @param requestBean The BeanMap to post.
	 * @param resultType the type of the BeanMap returned.
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap post(String address, BeanMap requestBean, String resultType) throws ServerErrorException {
		return (BeanMap)xs.fromXML(resultType, wsa.post(address, requestBean));
	}

	/**
	 * Create a BeanMap represented Resource through a web-service, where the web-service URI is equal to "type/id".
	 * 
	 * @param bean
	 * @return
	 * @throws ServerErrorException
	 */
	public BeanMap post(BeanMap bean) throws ServerErrorException {
		return post(bean.getType() + '/', bean);
	}
	
	/**
	 * Send a File to a web service.
	 * 
	 * @param address
	 * @param params
	 * @param file
	 * @throws ServerErrorException 
	 */
	public void post(String address, BeanMap params, File file) throws ServerErrorException {
		wsa.post(address, params, new FileRepresentation(file, FileMediaType.guess(file.getName())));
	}
	
	/**
	 * Send a Binary Stream to a web service.
	 * 
	 * @param address
	 * @param params
	 * @param stream
	 * @throws ServerErrorException 
	 */
	public void post(String address, BeanMap params, final InputStream stream) throws ServerErrorException {
		wsa.post(address, params, new StreamRepresentation(MediaType.APPLICATION_OCTET_STREAM) {
			@Override
			public void write(OutputStream outputStream) throws IOException {
				//Rien à faire !
			}
			@Override
			public InputStream getStream() throws IOException {
				// FIXME A new Stream must be sent to each all of this method.
				return stream;
			}
		});
	}
	
	/**
	 * Delete a resource.
	 * 
	 * @param address The web-service URI.
	 * @throws ServerErrorException
	 */
	public void delete(String address) throws ServerErrorException {
		wsa.delete(address,null);
	}
	
	/**
	 * Delete a resource with the following URI: type/id
	 * 
	 * @param type
	 * @param id
	 * @throws ServerErrorException
	 */
	public void delete(String type, int id) throws ServerErrorException {
		delete(type + '/' + id);
	}
	
	/**
	 * Delete a resource with the following URI: type/id
	 * 
	 * @param bean
	 * @throws ServerErrorException
	 */
	public void delete(BeanMap bean) throws ServerErrorException {
		delete(bean.getType() + '/' + bean.getId());
	}
	
}
