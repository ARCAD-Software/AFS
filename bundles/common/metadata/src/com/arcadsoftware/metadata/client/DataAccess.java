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
package com.arcadsoftware.metadata.client;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Reference;
import org.restlet.data.Status;
import org.restlet.representation.EmptyRepresentation;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StreamRepresentation;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.DataAccessServices;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.metadata.client.cache.CachedDatedBean;
import com.arcadsoftware.metadata.client.cache.CachedProperties;
import com.arcadsoftware.metadata.client.cache.CachedString;
import com.arcadsoftware.metadata.client.cache.ICacheableObject;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;
import com.arcadsoftware.metadata.xml.XmlMetaDataStream;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.ErrorMessageBean;
import com.arcadsoftware.rest.FileMediaType;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.rest.WebServiceAccess;

/**
 * High level access class to web services that provide BeanMap and other kind of objects.
 * 
 * <p>
 * This class implement a simple local Cache that allow to repeatedly call get method limited Server response time cost.
 * 
 * <p>
 * BeanMap can be directly obtained from Metadata Web services just from their Type and ID.
 * 
 * <p>
 * Creation Date: 28 février 2011
 * 
 * @see LocalCache
 * @see BeanMap
 */
public class DataAccess {

	private static final String PARAM_PAGECOUNT = "pagecount"; //$NON-NLS-1$
	private static final String PARAM_ATTRIBUTES = "attributes"; //$NON-NLS-1$
	private static final String PARAM_CRITERIA = "criteria"; //$NON-NLS-1$
	private static final String PARAM_DELETED = "deleted"; //$NON-NLS-1$
	private static final String PARAM_HARDDELETE = "harddelete"; //$NON-NLS-1$
	private static final String PARAM_DISTINCTS = "distincts"; //$NON-NLS-1$
	private static final String PARAM_OFFSET = "pagestart"; //$NON-NLS-1$
	private static final String PARAM_ORDERS = "orders"; //$NON-NLS-1$
	private static final String PARAM_DELETETARGET = "deletetargets"; //$NON-NLS-1$
	private static final String PARAM_IGNORESUBDIVISION = "ignoresubdivision"; //$NON-NLS-1$
	private static final String PATH_LISTTYPE = "list/"; //$NON-NLS-1$
	private static final String PATH_DATA = "data/"; //$NON-NLS-1$
	private static final String PATH_ENTITY = "metadata/"; //$NON-NLS-1$
	private static final String PATH_PROPERTIES = "properties/"; //$NON-NLS-1$
	private static final String PATH_BINEXT = "/bin"; //$NON-NLS-1$
	private static final String PATH_BINREDIRECT = "binredirect/"; //$NON-NLS-1$
	private static final String PATH_BIN = "bin/"; //$NON-NLS-1$
	private static final String PATH_LAYOUT = "layout/"; //$NON-NLS-1$
	private static final HashMap<String, Object> REMOVEMAP = new HashMap<String, Object>();
	static {
		REMOVEMAP.put("harddelete", "true"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static String stringArrayToString(String[] strings) {
		StringBuilder asb = new StringBuilder();
		for (String a : strings) {
			if (asb.length() > 0) {
				asb.append(' ');
			}
			asb.append(a);
		}
		return asb.toString();
	}

	public static String stringListToString(Collection<String> strings) {
		StringBuilder asb = new StringBuilder();
		for (String a : strings) {
			if (asb.length() > 0) {
				asb.append(' ');
			}
			asb.append(a);
		}
		return asb.toString();
	}

	private WebServiceAccess wsa;
	private LocalCache cache;
	private XmlMetaDataStream xs;
	private ILoggedPlugin activator;
	private DataAccessServices facade;
	private boolean lazzyEntities;
	private boolean testBeforeGet;

	/**
	 * Create a new Data access interface.
	 * 
	 * @param activator
	 *            optional interface used to log informations.
	 * @param webServiceAccess
	 *            the Web-Services access interface.
	 */
	public DataAccess(ILoggedPlugin activator, WebServiceAccess webServiceAccess) {
		super();
		this.activator = activator;
		setWebServicesAccess(webServiceAccess);
		setCache(new LocalCache());
		xs = new XmlMetaDataStream();
		facade = new DataAccessServices(activator, this);
		lazzyEntities = true;
	}

	/**
	 * Create a new Data access interface.
	 * 
	 * <p>
	 * Use the Metadata bundle activator to log informations.
	 * 
	 * @param activator
	 *            optional interface used to log informations.
	 * @param webServiceAccess
	 *            the Web-Services access interface.
	 */
	public DataAccess(WebServiceAccess webServiceAccess) {
		this(Activator.getInstance(), webServiceAccess);
	}

	/**
	 * Create a new Data access interface.
	 * 
	 * <p>
	 * Use the Metadata bundle activator to log informations.
	 * 
	 * @param serverAddress
	 *            server URL address
	 * @param login
	 *            optional user login
	 * @param password
	 *            optional user password.
	 */
	public DataAccess(String serverAddress, String login, char[] password) {
		this(Activator.getInstance(), new WebServiceAccess(Activator.getInstance(), serverAddress, login, password));
	}

	/**
	 * Create a new Data access interface.
	 * 
	 * @param activator
	 *            optional interface used to log informations.
	 * @param serverAddress
	 *            server URL address
	 * @param login
	 *            optional user login
	 * @param password
	 *            optional user password.
	 */
	public DataAccess(ILoggedPlugin activator, String serverAddress, String login, char[] password) {
		this(activator, new WebServiceAccess(activator, serverAddress, login, password));
	}

	private String lastFragment(String value) {
		if (value == null) {
			return ""; //$NON-NLS-1$
		}
		int slash = value.lastIndexOf('/');
		if (slash == -1) {
			return value;
		}
		while (slash == value.length() - 1) {
			value = value.substring(0, value.length() - 1);
			slash = value.lastIndexOf('/');
			if (slash == -1) {
				return value;
			}
		}
		return value.substring(slash + 1, value.length());
	}

	protected void logError(Throwable e) {
		if (activator != null) {
			activator.error(e.getLocalizedMessage(), e);
		} else {
			//Do Something
		}
	}

	/**
	 * Get the current "lazy" entities loading state.
	 * @return
	 * @see #setLazzyEntities(boolean)
	 */
	public boolean isLazzyEntities() {
		return lazzyEntities;
	}

	/**
	 * Define the "lazy" attribute of the cached entities.
	 * 
	 * <p>
	 * As long as this flag is true any loaded entity will be considered as a "lazy" one.
	 * Lazy entities will never be reloaded (until the cache is purged, or the application restarted).
	 * Lazy entities are faster to get because once they are loaded into the cache they will never get
	 * reloaded from server. 
	 *
	 * @param lazzyEntities New lazy state of the loaded entities. default value is false.
	 */
	public void setLazzyEntities(boolean lazzyEntities) {
		this.lazzyEntities = lazzyEntities;
	}

	/**
	 * @param wsa
	 *            the Web-Service access low level interface.
	 */
	public void setWebServicesAccess(WebServiceAccess wsa) {
		this.wsa = wsa;
	}

	/**
	 * @return the Web-Service access low level interface.
	 */
	public WebServiceAccess getWebServicesAccess() {
		return wsa;
	}

	/**
	 * @param cache
	 *            the local cache to set
	 */
	public void setCache(LocalCache cache) {
		this.cache = cache;
	}

	/**
	 * @return the local cache
	 */
	public LocalCache getCache() {
		return cache;
	}

	/**
	 * You can add aliases and converter to this XStreamer to be able to download new classes of complex objects.
	 * 
	 * @return the XStreamer objet associated to this Access object.
	 */
	public XmlMetaDataStream getXStream() {
		return xs;
	}

	/**
	 * Serialize the criteria to an XML stream.
	 * 
	 * @param criteria
	 * @return
	 */
	public String criteriaToXML(ISearchCriteria criteria) {
		if (criteria == null) {
			return "<all/>"; //$NON-NLS-1$
		}
		return xs.toXML(criteria);
	}

	/**
	 * Generic loading of IDatedBean. This method can be used to cache any object that is loaded from an unique URI (
	 * <code>key</code>) and may be optionally have a <code>type</code>.
	 * 
	 * <h2>Technical notes:</h2>
	 * <p>
	 * use the standard following algorithm :
	 * <ul>
	 * <li>Get the last cached value.
	 * <li>Test the local cache (lazy and latency properties).
	 * <li>Test on the server serveur (HEAD HTTP Call).
	 * <li>Load the value from the server.
	 * <li>If this is a new value add it to the cache.
	 * </ul>
	 * 
	 * @param path
	 *            URL path from server. (used to store the cached data).
	 * @param type
	 *            The BeanMap Type, null if data is not a BeanMap.
	 * @return A Object reference like cached into the LocalCache.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	protected ICacheableObject getDatedBean(String path, String type) throws ServerErrorException {
		// Blindage.
		if ((path == null) || (path.length() == 0)) {
			return null;
		}
		if (path.charAt(0) == '/') {
			path = path.substring(1, path.length());
		}
		// Test du cache local.
		ICacheableObject result = cache.get(path);
		if (cache.testLocal(path, result)) {
			return result;
		}
		boolean isnew = false;
		if (result == null) {
			result = new CachedDatedBean();
			isnew = true;
			// Appel du serveur pour tester si l'objet a été modifié.
		} else if (testBeforeGet && wsa.head(path, result.getLastModification())) {
			return result;
		}
		// Chargement à partir du serveur.
		Calendar cal = new GregorianCalendar();
		cal.setTime(new Date(0));
		if (type == null) {
			result.setContent(xs.fromXML(wsa.get(path, cal)));
		} else {
			result.setContent(xs.fromXML(type, wsa.get(path, cal)));
		}
		Date date = cal.getTime();
		if (date.getTime() > 0) {
			result.setLastModification(date);
		}
		// insertion dans le cache.
		if (isnew) {
			cache.put(path, result);
		}
		return result;
	}

	/**
	 * Get Raw Data.
	 * 
	 * <p>
	 * This Data may be locally cached if not modified.
	 * 
	 * @param path
	 *            URL path from server. (used to store the cached data).
	 * @return A Object reference like cached into the LocalCache.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	protected ICacheableObject getRaw(String path) throws ServerErrorException {
		// Blindage.
		if ((path == null) || (path.length() == 0)) {
			return null;
		}
		if (path.charAt(0) == '/') {
			path = path.substring(1, path.length());
		}
		// Test du cache local.
		ICacheableObject result = cache.get(path);
		Representation rep = null;
		if (cache.testLocal(path, result)) {
			return result;
		}
		// Appel du serveur
		try {
			if (result == null) {
				rep = wsa.getRaw(path, null);
			} else {
				rep = wsa.getRaw(path, result.getLastModification());
			}
		} catch (ServerErrorException e) {
			// Test si téléchargement non nécessaire.
			if (e.getHTTPCode() == Status.REDIRECTION_NOT_MODIFIED.getCode()) {
				return result;
			}
			throw e;
		}
		boolean isnew = false;
		if (result == null) {
			result = new CachedString();
		}
		if ((rep != null) && rep.isAvailable()) {
			try {
				try {
					result.setContent(rep.getText());
				} catch (IOException e) {
					if (activator != null) {
						activator.log(e);
					}
				}
				result.setLastModification(rep.getModificationDate());
			} finally {
				try {
					rep.exhaust();
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				}
				rep.release();
			}
		}
		if (isnew) {
			cache.put(path, result);
		}
		return result;
	}

	/**
	 * Get Raw Data from secured web service.
	 * 
	 * <p>
	 * This Data may be locally cached if not modified.
	 * 
	 * @param key
	 *            URL path from server. (used to store the cached data).
	 * @return A Object reference like cached into the LocalCache.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	protected ICacheableObject getSecuredRaw(String key) throws ServerErrorException {
		// Blindage.
		if ((key == null) || (key.length() == 0)) {
			return null;
		}
		if (key.charAt(0) == '/') {
			key = key.substring(1, key.length());
		}
		// Test du cache local.
		ICacheableObject result = cache.get(key);
		Representation rep = null;
		if (cache.testLocal(key, result)) {
			return result;
		}
		// Appel du serveur
		try {
			if (result == null) {
				rep = wsa.getSecuredRaw(key, (Date) null);
			} else {
				rep = wsa.getSecuredRaw(key, result.getLastModification());
			}
		} catch (ServerErrorException e) {
			// Test si téléchargement non nécessaire.
			if (e.getHTTPCode() == Status.REDIRECTION_NOT_MODIFIED.getCode()) {
				return result;
			}
			throw e;
		}
		boolean isnew = false;
		if (result == null) {
			result = new CachedString();
			isnew = true;
		}
		if ((rep != null) && rep.isAvailable()) {
			try {
				try {
					result.setContent(rep.getText());
				} catch (IOException e) {
					if (activator != null) {
						activator.log(e);
					}
				}
				result.setLastModification(rep.getModificationDate());
			} finally {
				try {
					rep.exhaust();
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				}
				rep.release();
			}
		}
		if (isnew) {
			cache.put(key, result);
		}
		return result;
	}

	/**
	 * Get Properties files content.
	 * 
	 * @param key
	 * @param locale
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	protected ICacheableObject getProps(String key, Locale locale) throws ServerErrorException {
		// Blindage.
		if ((key == null) || (key.length() == 0)) {
			return null;
		}
		if (key.charAt(0) == '/') {
			key = key.substring(1, key.length());
		}
		key = key + '_' + locale.toString();
		// Test du cache local.
		ICacheableObject result = cache.get(key);
		Representation rep = null;
		if (cache.testLocal(key, result)) {
			return result;
		}
		// Appel du serveur
		try {
			if (result == null) {
				rep = wsa.getRaw(key, null);
			} else {
				rep = wsa.getRaw(key, result.getLastModification());
			}
		} catch (ServerErrorException e) {
			// Test si téléchargement non nécessaire.
			if (e.getHTTPCode() == Status.REDIRECTION_NOT_MODIFIED.getCode()) {
				return result;
			}
			throw e;
		}
		boolean isnew = false;
		if (result == null) {
			result = new CachedProperties();
			isnew = true;
		}
		if ((rep != null) && rep.isAvailable()) {
			result.setLastModification(rep.getModificationDate());
			try {
				Properties p = new Properties();
				p.load(rep.getStream());
				result.setContent(p);
			} catch (IOException e) {
				if (activator != null) {
					activator.error(e.getLocalizedMessage(), e);
				}
				throw new ServerErrorException(10000, new ErrorMessageBean(Messages.DataAccess_Error_LoadingProperties
						+ e.getLocalizedMessage()));
			} finally {
				try {
					rep.exhaust();
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				}
				rep.release();
			}
		}
		if (isnew) {
			cache.put(key, result);
		}
		return result;
	}

	/**
	 * Get Properties files content.
	 * 
	 * @param key
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client invalid request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	protected ICacheableObject getProps(String key) throws ServerErrorException {
		// Blindage.
		if ((key == null) || (key.length() == 0)) {
			return null;
		}
		if (key.charAt(0) == '/') {
			key = key.substring(1, key.length());
		}
		// Test du cache local.
		ICacheableObject result = cache.get(key);
		Representation rep = null;
		if (cache.testLocal(key, result)) {
			return result;
		}
		// Appel du serveur
		try {
			if (result == null) {
				rep = wsa.getRaw(key, null);
			} else {
				rep = wsa.getRaw(key, result.getLastModification());
			}
		} catch (ServerErrorException e) {
			// Test si téléchargement non nécessaire.
			if (e.getHTTPCode() == Status.REDIRECTION_NOT_MODIFIED.getCode()) {
				return result;
			}
			throw e;
		}
		boolean isnew = false;
		if (result == null) {
			result = new CachedProperties();
			isnew = true;
		}
		if ((rep != null) && rep.isAvailable()) {
			result.setLastModification(rep.getModificationDate());
			try {
				Properties p = new Properties();
				p.load(rep.getStream());
				result.setContent(p);
			} catch (IOException e) {
				if (activator != null) {
					activator.error(e.getLocalizedMessage(), e);
				}
				throw new ServerErrorException(10000, new ErrorMessageBean(Messages.DataAccess_Error_LoadingProperties
						+ e.getLocalizedMessage()));
			} finally {
				try {
					rep.exhaust();
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				}
				rep.release();
			}
		}
		if (isnew) {
			cache.put(key, result);
		}
		return result;
	}

	/**
	 * Get a beanMap via his type and his id.
	 * 
	 * @param type
	 * @param id
	 * @return null if this item does not exist on the server.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client invalid request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap get(String type, int id) throws ServerErrorException {
		BeanMap result = null;
		if ((id > 0) && (type != null) && (type.length() > 0)) {
			try {
				if (type.startsWith(PATH_LISTTYPE)) {
					result = cache.getListElement(type, id);
					if (result == null) {
						result = (BeanMap) getDatedBean(PATH_DATA + type + '/' + id, type).getContent();
					}
				} else {
					result = (BeanMap) getDatedBean(PATH_DATA + type + '/' + id, type).getContent();
				}
			} catch (ServerErrorException e) {
				if (e.getHTTPCode() == 404) {
					return null;
				}
				throw e;
			}
		}
		return result;
	}

	/**
	 * Get a beanMap via his type and his unique code.
	 * 
	 * @param type
	 * @param code
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client invalid request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap get(String type, String code, boolean deleted) throws ServerErrorException {
		if ((code == null) || (code.trim().length() == 0) || (type == null) || (type.length() == 0)) {
			return null;
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		if (deleted) {
			params.put(PARAM_DELETED, "true"); //$NON-NLS-1$
		}
		Object result = xs.fromXML(type, wsa.get(PATH_DATA + type + '/' + code, params));
		if (result instanceof BeanMapList) {
			if (((BeanMapList) result).isEmpty()) {
				return null;
			}
			return ((BeanMapList) result).get(0);
		}
		return (BeanMap) result;
	}

	/**
	 * Get a BeanMap from its <code>address</code> and <code>type</code>.
	 * 
	 * <p>
	 * This can be used to get a BeanMap object form outside of the MetaData repository. The address identify the
	 * BeanMap "id" that will be retreived. But the type can not be guessed.
	 * 
	 * @param address
	 *            the full web-service path
	 * @param type
	 *            the BeanMap type
	 * @return a BeanMap object, may be null.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client bad request, connection problem or a
	 *             server internal error. It should be logged into User accessible journal and show to him if it stop
	 *             the current process.
	 */
	public BeanMap get(String address, String type) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return null;
		}
		if ((address == null) || (address.length() == 0)) {
			return new BeanMap(type);
		}
		try {
			return (BeanMap) getDatedBean(address, type).getContent();
		} catch (ServerErrorException e) {
			if (e.getHTTPCode() == 404) {
				return null;
			}
			throw e;
		}
	}

	/**
	 * Get a beanMap with specific attributes (support references, with dots).
	 * 
	 * @param type
	 *            the type to get.
	 * @param id
	 *            The item to return.
	 * @param attributes
	 *            the list of attributes to return, if null all attributes will be returned.
	 * @param deleted
	 *            true if a deleted item must be returned.
	 * @return a BeanMap object, may be null.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client bad request, connection problem or a
	 *             server internal error. It should be logged into User accessible journal and show to him if it stop
	 *             the current process.
	 */
	public BeanMap get(String type, int id, String attributes, boolean deleted) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return null;
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		if ((attributes != null) && (attributes.length() > 0)) {
			params.put(PARAM_ATTRIBUTES, attributes);
		}
		if (deleted) {
			params.put(PARAM_DELETED, "true"); //$NON-NLS-1$
		}
		try {
			return (BeanMap) xs.fromXML(type, wsa.get(PATH_DATA + type + '/' + id, params));
		} catch (ServerErrorException e) {
			if (e.getHTTPCode() == 404) {
				return null;
			}
			throw e;
		}
	}

	/**
	 * Get a beanMap with specific attributes (support references, with dots).
	 * 
	 * @param type
	 *            the type to get.
	 * @param id
	 *            The item to return.
	 * @param attributes
	 *            the list of attributes to return, if null all attributes will be returned.
	 * @return a BeanMap object, may be null.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client bad request, connection problem or a
	 *             server internal error. It should be logged into User accessible journal and show to him if it stop
	 *             the current process.
	 */
	public BeanMap get(String type, int id, String[] attributes) throws ServerErrorException {
		if ((attributes == null) || (attributes.length == 0)) {
			return get(type, id);
		}
		StringBuilder sb = new StringBuilder(attributes[0]);
		for (int i = 1; i < attributes.length; i++) {
			sb.append(' ');
			sb.append(attributes[i]);
		}
		return get(type, id, sb.toString(), false);
	}

	/**
	 * Method to load a beanMap from itself.
	 * 
	 * <p>
	 * This method is useful to complete a partially loaded BeanMap.
	 * 
	 * @param item
	 *            a non null BeanMap object.
	 * @return a BeanMap object, may be null.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap get(IBeanMap item) throws ServerErrorException {
		return get(item.getType(), item.getId());
	}

	/**
	 * Method to get a beanMapList knowing its type.
	 * <p>
	 * This method can be used with enumerations lists, aka "list/*" types, or some other lists.
	 * 
	 * <p>
	 * This method return the full list of entities
	 * 
	 * @param type
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String type) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return new BeanMapList();
		}
		String key;
		if (type.charAt(0) == '/') {
			key = type.substring(1, type.length());
		} else {
			key = type;
		}
		if (key.startsWith(PATH_LISTTYPE)) {
			BeanMapList list = cache.getList(key);
			if (list != null) {
				return list;
			}
			list = (BeanMapList) getDatedBean(PATH_DATA + type, type).getContent();
			if ((list != null) && (list.size() > 0)) {
				cache.putList(key, list);
			}
			return list;
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		params.put(PARAM_PAGECOUNT, -1);
		return (BeanMapList) xs.fromXML(type, wsa.get(PATH_DATA + type, params));
	}

	/**
	 * Method to get a beanMapList from a specific web-service.
	 * 
	 * @param path
	 *            URL of the service on the server.
	 * @param type
	 *            the type of beanmaps returned into the list.
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #getList(String)
	 */
	public BeanMapList getList(String path, String type) throws ServerErrorException {
		if ((type == null) || (type.length() == 0) || (path == null) || (path.length() == 0)) {
			return new BeanMapList();
		}
		return (BeanMapList) xs.fromXML(type, wsa.get(path));
	}
	/**
	 * Method to get a beanMapList from a specific web-service.
	 * 
	 * @param path
	 *            URL of the service on the server.
	 * @param type
	 *            the type of beanmaps returned into the list.
	 * @param attributes
	 *            the list of attributes to return, if null "listables" attributes will be returned.
	 * @param criteria
	 *            the condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            return all items.
	 * @param orders
	 *            the list of attributes (must be members of the attributes parameter) that will be used to order the
	 *            result. Each attribute name can prefixed this exclamation (!) to indicate reserve order.
	 * @param page
	 *            the first row number to return, start with zero for first row.
	 * @param count
	 *            the maximal number of items to return, -1 for unlimited results.
	 * @param deleted
	 *            true if the deleted items must be returned with non deleted ones.
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #getList(String)
	 */
	public BeanMapList getList(String path, String type, String attributes, String criteria, String orders, int page, int count,
			boolean deleted, boolean distinct) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return new BeanMapList();
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		if ((attributes != null) && (attributes.length() > 0)) {
			params.put(PARAM_ATTRIBUTES, attributes);
		}
		if ((criteria != null) && (criteria.length() > 0)) {
			params.put(PARAM_CRITERIA, criteria);
		}
		if (deleted) {
			params.put(PARAM_DELETED, "true"); //$NON-NLS-1$
		}
		if (distinct) {
			params.put(PARAM_DISTINCTS, "true"); //$NON-NLS-1$
		}
		if (count != 0) {
			params.put(PARAM_PAGECOUNT, count);
		}
		if (page > 0) {
			params.put(PARAM_OFFSET, page);
		}
		if ((orders != null) && (orders.length() > 0)) {
			params.put(PARAM_ORDERS, orders);
		}
		return (BeanMapList) xs.fromXML(type, wsa.get(path, params));
	}

	/**
	 * Get Linked items from a source BeanMap.
	 * 
	 * @param item
	 *            The Source BeanMap
	 * @param linkCode
	 *            The association link code.
	 * @param linkType
	 *            The type of the associated BeanMaps
	 * @return A list of linked BeanMap.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getLinks(BeanMap item, String linkCode, String linkType) throws ServerErrorException {
		if ((item.getId() <= 0) || (item.getType() == null) || (item.getType().length() == 0)) {
			return new BeanMapList();
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		params.put(PARAM_PAGECOUNT, -1);
		return (BeanMapList) xs.fromXML(linkType,
				wsa.get(PATH_DATA + item.getType() + '/' + item.getId() + '/' + linkCode, params));
	}

	/**
	 * Get Linked items from a source BeanMap.
	 * 
	 * @param item
	 *            The Source BeanMap
	 * @param linkCode
	 *            The association link code.
	 * @param linkType
	 *            The type of the associated BeanMaps
	 * @param attributes
	 *            the list of attributes to return, if null "listables" attributes will be returned.
	 * @param criteria
	 *            the condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            return all items.
	 * @param orders
	 *            the list of attributes (must be members of the attributes parameter) that will be used to order the
	 *            result. Each attribute name can prefixed this exclamation (!) to indicate reserve order.
	 * @param page
	 *            the first row number to return, start with zero for first row.
	 * @param count
	 *            the maximal number of items to return, -1 for unlimited results.
	 * @param deleted
	 *            true if the deleted items must be returned with non deleted ones.
	 * @param ignoreSubdivision
	 *            If true and if the link parent entity possess a recursive link then all the elements linked to 
	 *            the sourceId plus all elements linked to any other source item linked, recursivelly to the
	 *            sourceID one, will be returned.
	 * @return A list of linked BeanMap.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getLinks(BeanMap item, String linkCode, String linkType, String attributes, String criteria,
			String orders, int page, int count, boolean deleted, boolean ignoreSubdivision) throws ServerErrorException {
		if ((item.getId() <= 0) || (item.getType() == null) || (item.getType().length() == 0)) {
			return new BeanMapList();
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		if ((attributes != null) && (attributes.length() > 0)) {
			params.put(PARAM_ATTRIBUTES, attributes);
		}
		if ((criteria != null) && (criteria.length() > 0)) {
			params.put(PARAM_CRITERIA, criteria);
		}
		if (deleted) {
			params.put(PARAM_DELETED, "true"); //$NON-NLS-1$
		}
		if (ignoreSubdivision) {
			params.put(PARAM_IGNORESUBDIVISION, "true"); //$NON-NLS-1$
		}
		if (count != 0) {
			params.put(PARAM_PAGECOUNT, count);
		}
		if (page > 0) {
			params.put(PARAM_OFFSET, page);
		}
		if ((orders != null) && (orders.length() > 0)) {
			params.put(PARAM_ORDERS, orders);
		}
		return (BeanMapList) xs.fromXML(linkType,
				wsa.get(PATH_DATA + item.getType() + '/' + item.getId() + '/' + linkCode, params));
	}

	/**
	 * Return a list of BeanMap.
	 * 
	 * @param type
	 *            The type of BeanMap to get.
	 * @param parameters
	 *            Any request parameters, optional, can be null.
	 * @return A BeanMapList (or a PartialBeanMapList) of results, the results are not cached.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String type, BeanMap parameters) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return new BeanMapList();
		}
		return (BeanMapList) xs.fromXML(type, wsa.get(PATH_DATA + type, parameters));
	}

	/**
	 * Return a list of BeanMap.
	 * 
	 * @param address
	 *            The web-service address.
	 * @param type
	 *            The type of BeanMap to get.
	 * @param parameters
	 *            Any request parameters, optional, can be null.
	 * @return A BeanMapList (or a PartialBeanMapList) of results, the results are not cached.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String address, String type, BeanMap parameters) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return new BeanMapList();
		}
		return (BeanMapList) xs.fromXML(type, wsa.get(address, parameters));
	}

	/**
	 * Call to the search service with all the accepted parameters.
	 * 
	 * @param type
	 *            the type to search with.
	 * @param attributes
	 *            the list of attributes to return, if null "listables" attributes will be returned.
	 * @param criteria
	 *            the condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            return all items.
	 * @param orders
	 *            the list of attributes (must be members of the attributes parameter) that will be used to order the
	 *            result. Each attribute name can prefixed this exclamation (!) to indicate reserve order.
	 * @param page
	 *            the first row number to return, start with zero for first row.
	 * @param count
	 *            the maximal number of items to return, -1 for unlimited results.
	 * @param deleted
	 *            true if the deleted items must be returned with non deleted ones.
	 * @return A BeanMapList (or a PartialBeanMapList) of results, the results are not cached.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String type, String attributes, ISearchCriteria criteria, String orders, int page,
			int count, boolean deleted) throws ServerErrorException {
		if (criteria == null) {
			return getList(type, attributes, "", orders, page, count, deleted); //$NON-NLS-1$
		}
		return getList(type, attributes, new XmlCriteriaStream().toXML(criteria), orders, page, count, deleted);
	}

	/**
	 * Call to the search service with all the accepted parameters.
	 * 
	 * @param type
	 *            the type to search with.
	 * @param attributes
	 *            the list of attributes to return, if null "listables" attributes will be returned.
	 * @param criteria
	 *            the condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            return all items.
	 * @param orders
	 *            the list of attributes (must be members of the attributes parameter) that will be used to order the
	 *            result. Each attribute name can prefixed this exclamation (!) to indicate reserve order.
	 * @param page
	 *            the first row number to return, start with zero for first row.
	 * @param count
	 *            the maximal number of items to return, -1 for unlimited results.
	 * @param deleted
	 *            true if the deleted items must be returned with non deleted ones.
	 * @param distinct
	 *            Return only distinct results (may be not supported by all servers).
	 * @return A BeanMapList (or a PartialBeanMapList) of results, the results are not cached.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String type, String attributes, ISearchCriteria criteria, String orders, int page,
			int count, boolean deleted, boolean distinct) throws ServerErrorException {
		if (criteria == null) {
			return getList(type, attributes, "", orders, page, count, deleted, distinct); //$NON-NLS-1$
		}
		return getList(type, attributes, new XmlCriteriaStream().toXML(criteria), orders, page, count, deleted, distinct);
	}

	/**
	 * Call to the search service with all the accepted parameters.
	 * 
	 * @param type
	 *            the type to search with.
	 * @param attributes
	 *            the list of attributes to return, if null "listables" attributes will be returned.
	 * @param criteria
	 *            the condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            return all items.
	 * @param orders
	 *            the list of attributes (must be members of the attributes parameter) that will be used to order the
	 *            result. Each attribute name can prefixed this exclamation (!) to indicate reserve order.
	 * @param page
	 *            the first row number to return, start with zero for first row.
	 * @param count
	 *            the maximal number of items to return, -1 for unlimited results.
	 * @param deleted
	 *            true if the deleted items must be returned with non deleted ones.
	 * @return A BeanMapList (or a PartialBeanMapList) of results, the results are not cached.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String type, String attributes, String criteria, String orders, int page, int count,
			boolean deleted) throws ServerErrorException {
		return getList(type, attributes, criteria, orders, page, count, deleted, false);
	}
	/**
	 * Call to the search service with all the accepted parameters.
	 * 
	 * @param type
	 *            the type to search with.
	 * @param attributes
	 *            the list of attributes to return, if null "listables" attributes will be returned.
	 * @param criteria
	 *            the condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            return all items.
	 * @param orders
	 *            the list of attributes (must be members of the attributes parameter) that will be used to order the
	 *            result. Each attribute name can prefixed this exclamation (!) to indicate reserve order.
	 * @param page
	 *            the first row number to return, start with zero for first row.
	 * @param count
	 *            the maximal number of items to return, -1 for unlimited results.
	 * @param deleted
	 *            true if the deleted items must be returned with non deleted ones.
	 * @param distinct
	 *            Return only distinct results (may be not supported by all servers).
	 * @return A BeanMapList (or a PartialBeanMapList) of results, the results are not cached.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getList(String type, String attributes, String criteria, String orders, int page, int count,
			boolean deleted, boolean distinct) throws ServerErrorException {
		if ((type == null) || (type.length() == 0)) {
			return new BeanMapList();
		}
		HashMap<String, Object> params = new HashMap<String, Object>();
		if ((attributes != null) && (attributes.length() > 0)) {
			params.put(PARAM_ATTRIBUTES, attributes);
		}
		if ((criteria != null) && (criteria.length() > 0)) {
			params.put(PARAM_CRITERIA, criteria);
		}
		if (deleted) {
			params.put(PARAM_DELETED, "true"); //$NON-NLS-1$
		}
		if (distinct) {
			params.put(PARAM_DISTINCTS, "true"); //$NON-NLS-1$
		}
		if (count != 0) {
			params.put(PARAM_PAGECOUNT, count);
		}
		if (page > 0) {
			params.put(PARAM_OFFSET, page);
		}
		if ((orders != null) && (orders.length() > 0)) {
			params.put(PARAM_ORDERS, orders);
		}
		return (BeanMapList) xs.fromXML(type, wsa.get(PATH_DATA + type, params));
	}

	/**
	 * Create a beanMap with all his attributes.
	 * 
	 * @param address
	 *            The web service server relative URL.
	 * @param item
	 *            The values to store.
	 * @return The server responses, should be the effectively stored values.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap post(String address, BeanMap item) throws ServerErrorException {
		return post(address, item, item.getType());
	}

	/**
	 * Create a beanMap with all his attributes.
	 * 
	 * @param address
	 *            The web service server relative URL.
	 * @param item
	 *            The values to store.
	 * @param resultType
	 *            The BeanMap type used into the response message.
	 * @return The server responses, should be the effectively stored values.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap post(String address, BeanMap item, String resultType) throws ServerErrorException {
		if (address == null) {
			return null;
		}
		if (!(address.charAt(address.length() - 1) == '/')) {
			address = address + '/';
		}
		String xml = wsa.post(address, item);
		if ((xml == null) || (xml.length() == 0)) {
			return item;
		}
		cache.purge(item.getType());
		return (BeanMap) xs.fromXML(resultType, xml);
	}

	/**
	 * Create a beanMap with all his attributes.
	 * 
	 * @return The Created BeanMap (with a valid Id and auto-generated identifier).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap post(BeanMap item) throws ServerErrorException {
		return post(PATH_DATA + item.getType(), item);
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
	 * @param stream can be null
	 * @throws ServerErrorException 
	 */
	public void post(String address, BeanMap params, final InputStream stream) throws ServerErrorException {
		post(address, params, stream, null);
	}
	
	public BeanMap post(String address, BeanMap params, final InputStream stream, String type) throws ServerErrorException {
		Representation representation = null;
		if (stream == null) {
			representation = new EmptyRepresentation();
		} else {
			representation = new StreamRepresentation(MediaType.APPLICATION_OCTET_STREAM) {
				@Override
				public void write(OutputStream outputStream) throws IOException {
					if (stream != null) {
					    int readBytes = stream.read();
					    while (readBytes != -1) {
					    	outputStream.write(readBytes);
					    	readBytes = stream.read();
					    }
					    outputStream.flush();
					}
					// FIXME Who close the stream ?
				}
				@Override
				public InputStream getStream() throws IOException {
					// FIXME a new Stream must be sent at each call.
					return stream;
				}
			};
		}
		String xml = wsa.post(address, params, representation);
		if ((type != null) && (type.length() > 0) && (xml != null) && (xml.length() > 0)) {
			return (BeanMap) xs.fromXML(type, xml);
		}
		return null;
	}

	/**
	 * Update a specific object. Purge the cache from the corresponding data (address is the cache key).
	 * 
	 * @param address
	 *            The web-service address.
	 * @param params
	 *            The values to update (key-values pairs).
	 * @return the Resource result if any, as an XML fragment.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public String put(String address, Map<String, Object> params) throws ServerErrorException {
		if (address == null) {
			return null;
		}
		if (address.charAt(address.length() - 1) != '/') {
			address = address + '/';
		}
		String result = wsa.put(address, params);
		cache.purge(address);
		return result;
	}

	/**
	 * Update a specific object. Purge the cache from the corresponding data (address is the cache key).
	 * 
	 * @param address
	 *            The web-service address.
	 * @param type
	 *            The type of the returned BeanMap.
	 * @param params
	 *            The values to update (key-values pairs).
	 * @return null or the updated BeanMap if the Resource return one.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap put(String address, String type, Map<String, Object> params) throws ServerErrorException {
		String result = put(address, params);
		if ((result == null) || result.isEmpty()) {
			return null;
		}
		return (BeanMap) xs.fromXML(type, result);
	}

	/**
	 * Update a beanMap.
	 * 
	 * <p>
	 * The passed BeanMap must only contain id and effectively modified attributes do not send back to the server non
	 * modified attribute this is useless and consume bandwidth.
	 * </p>
	 * 
	 * @param item
	 *            The part of the BeanMap to be updated.
	 * @return null or the updated BeanMap if the Resource return one.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap put(BeanMap item) throws ServerErrorException {
		return put(PATH_DATA + item.getType(), item);
	}

	/**
	 * Update a beanMap with an other service. A service that is <b>not</b> the MetaData data resource.
	 * 
	 * <p>
	 * The passed BeanMap should only contain id and effectively modified attributes do not send back to the server non
	 * modified attribute this is useless and consume bandwidth.
	 * 
	 * <p>
	 * After this call the passed BeanMap is updated with the server result, if and only if any result is sent.
	 * 
	 * @param address
	 *            The web service server relative URL.
	 * @param item
	 *            The BeanMap to be updated.
	 * @return null or the updated BeanMap if the Resource return one.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMap put(String address, BeanMap item) throws ServerErrorException {
		if (item == null) {
			return null;
		}
		if (item.getId() > 0) {
			if (address.charAt(address.length() - 1) != '/') {
				address = address + '/';
			}
			return put(address + item.getId(), item.getType() , item);
		}
		return put(address, item.getType(), item);
	}

	/**
	 * Update one attribute of a BeanMap.
	 * 
	 * <p>
	 * Support null values.
	 * 
	 * @param type
	 *            The BeanMap type.
	 * @param id
	 *            The BeanMap Id.
	 * @param attribute
	 *            The attribute code to update.
	 * @param value
	 *            The new value.
	 * @return The Resource result if any, as an XML fragment.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public String put(String type, int id, String attribute, Object value) throws ServerErrorException {
		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put(attribute, value);
		return put(PATH_DATA + type + '/' + id, map);
	}

	/**
	 * Put a raw representation into the server.
	 * 
	 * @param address
	 *            The web service server relative URL.
	 * @param raw
	 *            Raw data to store.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void putRaw(String address, String raw) throws ServerErrorException {
		wsa.put(address, raw);
		cache.purge(address);
	}

	/**
	 * Delete a beanMap. This deletion may be recover this undelete if the corresponding entity use a "logical deletion" metadata.
	 * 
	 * <p>
	 * Only the type and the id of the BeanMap are needed to try delete it. You can pass through this method only a
	 * partial BeanMap (attribute values will not be sent to the server).
	 * 
	 * @param item
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #undelete(BeanMap)
	 * @see #remove(BeanMap)
	 */
	public void delete(BeanMap item) throws ServerErrorException {
		delete(item.getType(), item.getId());
	}

	/**
	 * Delete a beanMap. This deletion may be recover this undelete if the corresponding entity use a "logical deletion" metadata.
	 * 
	 * @param type
	 *            The BeanMap type.
	 * @param id
	 *            The BeanMap Id.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #undelete(String, int)
	 * @see #remove(String, int)
	 */
	public void delete(String type, int id) throws ServerErrorException {
		delete(PATH_DATA + type + '/' + id);
	}

	/**
	 * Delete a beanMap. This deletion may be recover this undelete if the corresponding entity use a "logical deletion" metadata.
	 * 
	 * @param type
	 *            The BeanMap type.
	 * @param id
	 *            The BeanMap Id.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #undelete(String, int)
	 * @see #remove(String, int)
	 */
	public void deleteHard(String type, int id) throws ServerErrorException {
		delete(PATH_DATA + type + '/' + id, REMOVEMAP);
	}
	
	/**
	 * Delete a list of beanMaps.
	 * 
	 * <p>
	 * Only the type and the id of the BeanMap are needed to delete it. You can pass through this method only a
	 * partial BeanMap (attribute values will not be sent to the server).
	 * 
	 * @param list The BeanMap list to delete.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void delete(BeanMapList list) throws ServerErrorException {
		// TODO (Optionisation) utiliser un appel multiple.
		if (list != null) {
			for (BeanMap b: list) {
				delete(b);
			}
		}
	}

	/**
	 * Delete a list of data through one unique call to the server.
	 *  
	 * @param type
	 *            The BeanMap type.
	 * @param criteria
	 *            The condition associated to the request. Null criteria will delete all items.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void delete(String type, ISearchCriteria criteria) throws ServerErrorException {
		HashMap<String, Object> params = new HashMap<String, Object>();
		if (criteria != null) {
			params.put(PARAM_CRITERIA, criteriaToXML(criteria));
		}
		wsa.delete(PATH_DATA + type, params);
	}

	/**
	 * Delete a list of data through one unique call to the server.
	 *  
	 * @param type
	 *            The BeanMap type.
	 * @param criteria
	 *            The condition associated to the request, this parameter must an XML representation of the criteria.
	 *            Use the <code>criteriaToXML(ISearchCriteia)</code> method to get this parameter. Empty criteria will
	 *            delete all items.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void delete(String type, String criteria) throws ServerErrorException {
		HashMap<String, Object> params = new HashMap<String, Object>();
		if ((criteria != null) && (criteria.length() > 0)) {
			params.put(PARAM_CRITERIA, criteria);
		}
		wsa.delete(PATH_DATA + type, params);
	}

	/**
	 * Process to an hard deletion (unrecoverable suppression) of a set of data. This operation is made with an unique 
	 * call to the server.
	 * 
	 * @param type
	 *            The BeanMap type.
	 * @param criteria
	 *            The condition associated to the request. Null criteria will delete all items.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void deleteHard(String type, ISearchCriteria criteria) throws ServerErrorException {
		HashMap<String, Object> params = new HashMap<String, Object>();
		if (criteria != null) {
			params.put(PARAM_CRITERIA, criteriaToXML(criteria));
		}
		params.put(PARAM_HARDDELETE, true);
		wsa.delete(PATH_DATA + type, params);
	}

	/**
	 * Definitively delete a beanMap.
	 * 
	 * <p>
	 * Only the type and the id of the BeanMap are needed to delete it. You can pass through this method only a
	 * partial BeanMap (attribute values will not be sent to the server).
	 * 
	 * @param item
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #delete(BeanMap)
	 */
	public void remove(BeanMap item) throws ServerErrorException {
		remove(item.getType(), item.getId());
	}

	/**
	 * Definitively delete a list of beanMaps.
	 * 
	 * <p>
	 * Only the type and the id of the BeanMap are needed to delete it. You can pass through this method only a
	 * partial BeanMap (attribute values will not be sent to the server).
	 * 
	 * @param list
	 * @throws ServerErrorException
	 */
	public void remove(BeanMapList list) throws ServerErrorException {
		// TODO (Optionisation) utiliser un appel multiple.
		if (list != null) {
			for(BeanMap b: list) {
				remove(b);
			}
		}
	}

	/**
	 * Definitively Delete a beanMap.
	 * 
	 * @param type
	 * @param id
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #delete(String, int)
	 */
	public void remove(String type, int id) throws ServerErrorException {
		String address = PATH_DATA + type + '/' + id;
		wsa.delete(address, REMOVEMAP);
		cache.purge(address);
	}

	/**
	 * Undelete a BeanMap.
	 * 
	 * <p>
	 * Only the type and the id of the BeanMap are needed to try to undelete it. You can pass through this method only a
	 * partial BeanMap (attribute values will not be sent to the server).
	 * 
	 * <p>
	 * This methods change the last modification date even if the BeanMap is not deleted into the database.
	 * 
	 * @param item
	 *            The beanMap to undelete.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void undelete(BeanMap item) throws ServerErrorException {
		undelete(item.getType(), item.getId());
	}

	/**
	 * Undelete a BeanMap.
	 * 
	 * @param type
	 * @param id
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void undelete(String type, int id) throws ServerErrorException {
		HashMap<String, Object> map = new HashMap<String, Object>();
		map.put("_undelete", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		String address = PATH_DATA + type + '/' + id;
		wsa.put(address, map);
		cache.purge(address);
	}

	/**
	 * Delete an object onto the server.
	 * 
	 * <p>
	 * Delete the corresponding object form the local cache (if any).
	 * 
	 * @param address
	 *            the server Path address.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void delete(String address) throws ServerErrorException {
		wsa.delete(address, null);
		cache.purge(address);
	}
	
	/**
	 * Delete an object onto the server.
	 * 
	 * <p>
	 * Delete the corresponding object form the local cache (if any).
	 * 
	 * @param address
	 *            the server Path address.
	 * @param parameters
	 *            the parameters.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void delete(String address, Map<String, Object> parameters) throws ServerErrorException {
		wsa.delete(address, parameters);
		cache.purge(address);
	}


	/**
	 * Return an entity Structure information object.
	 * 
	 * <p>
	 * This object can be locally cached.
	 * 
	 * <p>
	 * <b>If it is cached then it will be not reloaded even if there is a modification on the server side.</b>
	 * 
	 * @param type
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public MetaDataEntity getEntity(String type) throws ServerErrorException {
		ICacheableObject db = getDatedBean(PATH_ENTITY + type, type);
		if (db == null) {
			return null;
		}
		db.setLazzy(lazzyEntities);
		MetaDataEntity entity = (MetaDataEntity) db.getContent();
		entity.setMapper(facade);
		entity.setRegistry(facade);
		return entity;
	}

	/**
	 * @return The list of all the entities defined on the server.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public List<MetaDataEntity> getEntities() throws ServerErrorException {
		if (cache.getEntities() != null) {
			return cache.getEntities();
		}
		@SuppressWarnings("unchecked")
		List<MetaDataEntity> result = (List<MetaDataEntity>) xs.fromXML(wsa.get(PATH_ENTITY, (Calendar) null));
		if (result == null) {
			return new ArrayList<MetaDataEntity>();
		}
		// Fill cache...
		for (MetaDataEntity entity : result) {
			entity.setMapper(facade);
			entity.setRegistry(facade);
			ICacheableObject db = new CachedDatedBean(entity);
			db.setLazzy(lazzyEntities);
			cache.put(PATH_ENTITY + entity.getType(), db);
		}
		cache.setEntities(result);
		return result;
	}

	/**
	 * Download an layout document (XML file) without any preprocessing.
	 * 
	 * <p>
	 * This document can be locally cached.
	 * 
	 * <p>
	 * <b>If it is cached then it will be not reloaded even if there is a modification on the server side.</b>
	 * 
	 * <p>
	 * This service is not secured.
	 * 
	 * @param name
	 *            the Layout file name (can be null).
	 * @param type
	 *            the associated entity type (can not be null);
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public String getLayoutFile(String name, String type) throws ServerErrorException {
		String key = PATH_LAYOUT + type;
		if (name != null) {
			key += '/' + name;
		}
		ICacheableObject result = cache.get(key);
		result = null;
		// ATTENTION Pas de tests de modification sur le serveur !!!
		if (result instanceof CachedString) {
			return result.getContent().toString();
		}
		Representation rep;
		rep = wsa.getRaw(key, null);
		if (rep.isAvailable()) {
			try {
				String s = rep.getText();
				if (s != null) {
					cache.put(key, new CachedString(null, s));
				}
				return s;
			} catch (IOException e) {
				if (activator != null) {
					activator.log(e);
				}
			} finally {
				try {
					rep.exhaust();
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				}
				rep.release();
			}
		}
		return null;
	}

	/**
	 * Download an raw XML representation and cache it.
	 * 
	 * @param address
	 *            the web-service address.
	 * @return The xml string returned by the service. Null if no result are returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public String getXml(String address) throws ServerErrorException {
		if ((address == null) || (address.length() == 0)) {
			return null;
		}
		if (address.charAt(0) == '/') {
			address = address.substring(1, address.length());
		}
		// Gestion du Cache
		ICacheableObject result = cache.get(address);
		// Test local.
		if (cache.testLocal(address, result)) {
			return (String) result.getContent();
		}
		// Test en appelant le serveur.
		boolean isnew = false;
		if (result == null) {
			result = new CachedString();
			isnew = true;
		} else if (wsa.head(address, result.getLastModification())) {
			return (String) result.getContent();
		}
		// Chargement.
		Calendar cal = new GregorianCalendar();
		cal.setTime(new Date(0));
		result.setContent(wsa.get(address, cal));
		Date date = cal.getTime();
		if (date.getTime() > 0) {
			result.setLastModification(date);
		}
		if (isnew) {
			cache.put(address, result);
		}
		return (String) result.getContent();
	}


	/**
	 * Return a url that is return by a proxy service.
	 * 
	 * @param path
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @deprecated use {@link #getRedirection(Method, String)}
	 */
	public String getRedirection(String path) throws ServerErrorException {
		return getRedirection(Method.GET, path);
	}
	
	/**
	 * Return a url that is return by a proxy service.
	 * 
	 * @param path
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public String getRedirection(Method method, String path) throws ServerErrorException {
		Reference ref = wsa.getRedirection(method, path);
		if (ref != null) {
			return ref.toString();
		}
		return null;
	}

	/**
	 * Traite un service qui renvois du XML. Ce service peut être un fichier statique et/ou un service distant
	 * (redirection depuis les services d'Arcad-Customer).
	 * 
	 * @param path
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public String getXmlNoCache(String path) throws ServerErrorException {
		return wsa.get(path, true, true);
	}

	/**
	 * Get an association listing.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param associationKey
	 *            The association code. By convention this is a type+"s".
	 * @param linkedType
	 *            The type of the returned entity items.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public BeanMapList getLinks(String type, int id, String associationKey, String linkedType)
			throws ServerErrorException {
		return (BeanMapList) getDatedBean(PATH_DATA + type + '/' + id + '/' + associationKey, linkedType).getContent();
	}

	/**
	 * Create a new association between two entity items, according to a specified association code.
	 * 
	 * <p>
	 * This method is idempotent, sending many times the same operation will generate only one association.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code. By convention this is a type+"s".
	 * @param subid
	 *            The linked item.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void addLink(String type, int id, String linkCode, int subid) throws ServerErrorException {
		String ref = PATH_DATA + type + '/' + id + '/' + linkCode;
		cache.purge(ref);
		wsa.put(ref + '/' + subid, true);
	}

	/**
	 * Create new associations between the given entity a	nd target list of items, according to a specified association code.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code. By convention this is a type+"s".
	 * @param subids
	 *            The linked items.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void addLinks(String type, int id, String linkCode, int... subids) throws ServerErrorException {
		if ((subids != null) && (subids.length > 0)) {
			String ref = PATH_DATA + type + '/' + id + '/' + linkCode;
			cache.purge(ref);
			StringBuilder ids = new StringBuilder();
			for (int subid : subids) {
				if (subid > 0) {
					if (ids.length() > 0) {
						ids.append('/');
					}
					ids.append(subid);
				}
			}		
			wsa.put(ref + '/' + ids.toString(), true);
		}
	}

	/**
	 * Remove an association link between two items, according to a specified association code.
	 * 
	 * <p>
	 * This method is <i>idempotent</i>, sending many times the same operation will delete only one association.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code. By convention this is a type+"s".
	 * @param subid
	 *            The linked item.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void removeLink(String type, int id, String linkCode, int subid) throws ServerErrorException {
		String ref = PATH_DATA + type + '/' + id + '/' + linkCode;
		cache.purge(ref);
		wsa.delete(ref + '/' + subid, null);
	}
	
	/**
	 * Hard-Remove association links from an item to linked items, according to a specified association code.
	 * 
	 * <p>
	 * This method is <i>idempotent</i>, sending many times the same operation will delete only one association.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code. By convention this is a type+"s".
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void removeLinks(String type, int id, String linkCode) throws ServerErrorException {
		String ref = PATH_DATA + type + '/' + id + '/' + linkCode;
		cache.purge(ref);
		wsa.delete(ref, REMOVEMAP);
		cache.purge(ref);
	}
	
	/**
	 * Hard-Remove association links from an item to linked items, according to a specified association code.
	 * Also hard-remove linked items.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code. By convention this is a type+"s".
	 * @param deleteItems
	 *            True to delete linked items and association links; False to delete only association links.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void removeLinkedItems(String type, int id, String linkCode, boolean deleteTargets) throws ServerErrorException {
		String ref = PATH_DATA + type + '/' + id + '/' + linkCode;
		cache.purge(ref);
		@SuppressWarnings("unchecked")
		HashMap<String, Object> map = (HashMap<String, Object>)REMOVEMAP.clone();
		map.put(PARAM_DELETETARGET, deleteTargets);
		wsa.delete(ref, map);
		cache.purge(ref);
	}

	/**
	 * Test if the given association link exist.
	 * 
	 * <p>
	 * Return true if the item can be touched on the server. This operation should not require any privilege (right) and
	 * return no information about the contain.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code (By convention this is a type+"s").
	 * @param subid
	 *            The linked item.
	 * @return True if the link exists.
	 */
	public boolean testLink(String type, int id, String linkCode, int subid) {
		return testLink(type, id, linkCode, subid, false);
	}

	/**
	 * Test if the given association link exist.
	 * 
	 * <p>
	 * Return true if the item can be touched on the server. This operation should not require any privilege (right) and
	 * return no information about the contain.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param linkCode
	 *            The association code (By convention this is a type+"s").
	 * @param subid
	 *            The linked item.
	 * @param ignoseSubdivision
	 *            If true only a direct is test. 
	 * @return True if the link exists.
	 */
	public boolean testLink(String type, int id, String linkCode, int subid, boolean ignoseSubdivision) {
		try {
			if (ignoseSubdivision) {
				wsa.get(PATH_DATA + type + '/' + id + '/' + linkCode + '/' + subid + "?norec=true", (Calendar) null);
			} else {
				wsa.get(PATH_DATA + type + '/' + id + '/' + linkCode + '/' + subid, (Calendar) null);
			}
			return true;
		} catch (ServerErrorException e) {
			return false;
		}
	}

	/**
	 * Return a downloadable object that can be acceded from different methods.
	 * 
	 * <p>
	 * Be careful to read completely the file's content, or call the <code>release()</code> method to close any
	 * connecting socket.
	 * 
	 * @param key
	 *            The Binaries Web-service temporal key.
	 * @return An object that represent the downloaded file's content (streaming capable).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Representation getStream(String key) throws ServerErrorException {
		return wsa.getRaw(PATH_BIN + key, null);
	}

	/**
	 * Download a file associate to the given BeanMap.
	 * 
	 * <p>
	 * By convention the client get the file temporary key from a service : <code>/type/{id}/bin</code>.
	 * 
	 * <p>
	 * Be careful to read completely the file's content, or call the <code>release()</code> method to close any
	 * connecting socket.
	 * 
	 * @param bean
	 *            The source item.
	 * @return An object that represent the downloaded file's content (streaming capable).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Representation getBeanStream(BeanMap bean) throws ServerErrorException {
		return getBeanStream(bean.getType(), bean.getId());
	}

	/**
	 * Download a file associate to the given BeanMap.
	 * 
	 * <p>
	 * By convention the client get the file temporary key from a service : <code>/type/{id}/bin</code>.
	 * 
	 * <p>
	 * Be careful to read completely the file's content, or call the <code>release()</code> method to close any
	 * connecting socket.
	 * 
	 * @param bean
	 *            The source item.
	 * @param binaryKey
	 *            The bineay metadata key. If this key use a multi-level name (i.e. with slash "/") then only the last
	 *            level is determinent for this call.
	 * @return An object that represent the downloaded file's content (streaming capable).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Representation getBeanStream(BeanMap bean, String binaryKey) throws ServerErrorException {
		return getBeanStream(bean.getType(), bean.getId(), binaryKey);
	}

	/**
	 * Download a file associate to the given BeanMap.
	 * 
	 * <p>
	 * By convention the client get the file temporary key from a service : <code>/type/{id}/bin</code>.
	 * 
	 * <p>
	 * Be careful to read completely the file's content, or call the <code>release()</code> method to close any
	 * connecting socket.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @return An object that represent the downloaded file's content (streaming capable).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Representation getBeanStream(String type, int id) throws ServerErrorException {
		Reference ref = wsa.getRedirection(Method.GET, PATH_DATA + type + '/' + id + PATH_BINEXT);
		if (ref != null) {
			return wsa.getRaw(ref, null);
		}
		throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileDownload));
	}

	/**
	 * Download a file associate to the given BeanMap.
	 * 
	 * <p>
	 * By convention the client get the file temporary key from a service : <code>/type/{id}/bin</code>.
	 * 
	 * <p>
	 * Be careful to read completely the file's content, or call the <code>release()</code> method to close any
	 * connecting socket.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param binaryKey
	 *            The bineay metadata key. If this key use a multi-level name (i.e. with slash "/") then only the last
	 *            level is determinent for this call.
	 * @return An object that represent the downloaded file's content (streaming capable).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Representation getBeanStream(String type, int id, String binaryKey) throws ServerErrorException {
		if ((binaryKey == null) || (binaryKey.length() == 0)) {
			return getBeanStream(type, id);
		}
		Reference ref = wsa.getRedirection(Method.GET, PATH_DATA + type + '/' + id + PATH_BINEXT + '/' + lastFragment(binaryKey));
		if (ref != null) {
			return wsa.getRaw(ref, null);
		}
		throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileDownload));
	}

	/**
	 * Delete a file associated to a BeanMap.
	 * 
	 * <p>
	 * By convention the client get the file temporary key from a service : <code>/type/{id}/bin</code>.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void deleteBeanStream(String type, int id) throws ServerErrorException {
		Reference ref = wsa.getRedirection(Method.DELETE, PATH_DATA + type + '/' + id + PATH_BINEXT);
		if (ref == null) {
			throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileDownload));
		}
		wsa.deleteRaw(ref);
	}

	/**
	 * Delete a file associated to a BeanMap.
	 * 
	 * <p>
	 * By convention the client get the file temporary key from a service : <code>/type/{id}/bin</code>.
	 * 
	 * @param type
	 *            the source entity type.
	 * @param id
	 *            The source item from <code>type</code> type.
	 * @param binaryKey
	 *            The bineay metadata key. If this key use a multi-level name (i.e. with slash "/") then only the last
	 *            level is determinent for this call.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void deleteBeanStream(String type, int id, String binaryKey) throws ServerErrorException {
		if ((binaryKey == null) || (binaryKey.length() == 0)) {
			deleteBeanStream(type, id);
		}
		Reference ref = wsa.getRedirection(Method.DELETE, PATH_DATA + type + '/' + id + PATH_BINEXT + '/' + lastFragment(binaryKey));
		if (ref == null) {
			throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileDownload));
		}
		wsa.deleteRaw(ref);
	}

	/**
	 * Access to the binaries repository through an HTTP redirection.
	 * 
	 * <p>
	 * The redirection give a temporary key to upload the file. This temporary redirection can point to another server
	 * and is secured only if this server require it.
	 * 
	 * @param type
	 *            the entity type
	 * @param id
	 *            the entity id
	 * @param file
	 *            the file to upload onto the server repository.
	 * 
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void uploadBeanStream(String type, int id, File file) throws ServerErrorException {
		Reference ref = wsa.getRedirection(Method.POST, PATH_DATA + type + '/' + id + PATH_BINEXT);
		if (ref == null) {
			throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileUpload));
		}
		// [ML] Mars 2009, le service accepte un nom de fichier en dernière position.
		ref.addSegment(file.getName());
		// [ML] Mars 2011(!), Cette nouvelle version propage l'authentification sur le serveur destinataire du
		// téléchargement.
		wsa.post(ref, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM));
		// wsa.operation(Method.POST, ref, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM), null);
	}

	/**
	 * Access to the binaries repository through an HTTP redirection.
	 * 
	 * <p>
	 * The redirection give a temporary key to upload the data. This temporary redirection can point to another server
	 * and is secured only if this server require it.
	 * 
	 * @param type
	 *            the entity type
	 * @param id
	 *            the entity id
	 * @param stream
	 *            the data to upload onto the server repository.
	 * 
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void uploadBeanStream(String type, int id, final InputStream stream) throws ServerErrorException {
		Reference ref = wsa.getRedirection(Method.POST, PATH_DATA + type + '/' + id + PATH_BINEXT);
		if (ref == null) {
			throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileUpload));
		}
		wsa.post(ref, new StreamRepresentation(MediaType.APPLICATION_OCTET_STREAM) {
			@Override
			public void write(OutputStream outputStream) throws IOException {		
				if (stream != null) {
				    int readBytes = stream.read();
				    while (readBytes!=-1){
				    	outputStream.write(readBytes);
				    	readBytes = stream.read();
				    }			
				    outputStream.flush();
				}
				// FIXME Who close the stream ?
			}
			@Override
			public InputStream getStream() throws IOException {
				// FIXME a new Stream must be sent at each call.
				return stream;
			}
		});
	}

	/**
	 * Access to the binaries repository through an HTTP redirection.
	 * 
	 * <p>
	 * The redirection give a temporary key to upload the file. This temporary redirection can point to another server
	 * and is secured only if this server require it.
	 * 
	 * @param type
	 *            The entity type
	 * @param id
	 *            The entity id
	 * @param binaryKey
	 *            The binary metadata key. If this key use a multi-level name (i.e. with slash "/") then only the last
	 *            level is determinent for this call.
	 * @param file
	 *            The file to upload onto the server repository.
	 * 
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void uploadBeanStream(String type, int id, String binaryKey, File file) throws ServerErrorException {
		if ((binaryKey == null) || (binaryKey.length() == 0)) {
			uploadBeanStream(type, id, file);
			return;
		}
		Reference ref = wsa.getRedirection(Method.POST, PATH_DATA + type + '/' + id + PATH_BINEXT + '/' + lastFragment(binaryKey));
		if (ref == null) {
			throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileUpload));
		}
		// [ML] Mars 2009, le service accepte un nom de fichier en dernière position.
		ref.addSegment(file.getName());
		// [ML] Mars 2011(!), Cette nouvelle version propage l'authentification sur le serveur destinataire du
		// téléchargement.
		wsa.post(ref, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM));
		// wsa.operation(Method.POST, ref, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM), null);
	}

	/**
	 * Return the Binary service redirection address.
	 * <p>
	 * This URL is a temporary address hat should not be persisted.
	 * 
	 * @param type
	 *            An entity Type.
	 * @param id
	 *            An entity data id.
	 * @return An URL to the temporary (unsecured) binary file transfert service.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #uploadBeanStream
	 * @deprecated use {@link #getUploadRedirection(Method, String, int)}
	 */
	public String getUploadRedirection(String type, int id) throws ServerErrorException {
		return getUploadRedirection(Method.POST, type, id);
	}

	/**
	 * Return the Binary service redirection address.
	 * <p>
	 * This URL is a temporary address hat should not be persisted.
	 * 
	 * @param method the HTTP Method of the destination call.
	 * @param type
	 *            An entity Type.
	 * @param id
	 *            An entity data id.
	 * @return An URL to the temporary (unsecured) binary file transfert service.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #uploadBeanStream
	 */
	public String getUploadRedirection(Method method, String type, int id) throws ServerErrorException {
		Reference ref = wsa.getRedirection(method, PATH_DATA + type + '/' + id + PATH_BINEXT);
		if (ref != null) {
			return ref.toString();
		}
		return null;
	}

	/**
	 * Return the Binary service redirection address.
	 * <p>
	 * This URL is a temporary address hat should not be persisted.
	 * 
	 * @param type
	 *            An entity Type.
	 * @param id
	 *            An entity data id.
	 * @param binaryKey
	 *            The bineay metadata key. If this key use a multi-level name (i.e. with slash "/") then only the last
	 *            level is determinent for this call.
	 * @return An URL to the temporary (unsecured) binary file transfert service.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #uploadBeanStream
	 * @deprecated use {@link #getUploadRedirection(Method, String, int, String)}
	 */
	public String getUploadRedirection(String type, int id, String binaryKey) throws ServerErrorException {
		return getUploadRedirection(Method.POST, type, id, binaryKey);
	}
	/**
	 * Return the Binary service redirection address.
	 * <p>
	 * This URL is a temporary address hat should not be persisted.
	 * 
	 * @param type
	 *            An entity Type.
	 * @param id
	 *            An entity data id.
	 * @param binaryKey
	 *            The bineay metadata key. If this key use a multi-level name (i.e. with slash "/") then only the last
	 *            level is determinent for this call.
	 * @return An URL to the temporary (unsecured) binary file transfert service.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @see #uploadBeanStream
	 */
	public String getUploadRedirection(Method method, String type, int id, String binaryKey) throws ServerErrorException {
		if ((binaryKey == null) || (binaryKey.length() == 0)) {
			return getUploadRedirection(type, id);
		}
		Reference ref = wsa.getRedirection(method, PATH_DATA + type + '/' + id + PATH_BINEXT + '/' + lastFragment(binaryKey));
		if (ref != null) {
			return ref.toString();
		}
		return null;
	}

	/**
	 * Send an image file (or any dinary file) to the server shared image repository.
	 *  
	 * @param type
	 * @param id
	 * @param file
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void uploadImage(String type, int id, File file) throws ServerErrorException {
		Reference ref = wsa.getRedirection(Method.POST, PATH_BINREDIRECT + type.replace('/', '_') + '/' + id);
		if (ref == null) {
			throw new ServerErrorException(-1, new ErrorMessageBean(Messages.DataAccess_Error_FileUpload));
		}
		// [ML] Mars 2009, le service accepte un nom de fichier en dernière position.
		ref.addSegment(file.getName());
		// wsa.operation(Method.POST, ref, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM), null);
		wsa.post(ref, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM));
	}

	/**
	 * Upload a binary file on the server.
	 * 
	 * @param address
	 *            The service address.
	 * @param file
	 *            The file to upload.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void upload(String address, File file) throws ServerErrorException {
		if ((address != null) && (address.length() > 0)) {
			Map<String, Object> parameters = new HashMap<String, Object>();
			if (file != null) {
				parameters.put("filename", file.getName());
			}
			wsa.post(address, parameters, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM));
		}
	}
	

	/**
	 * Upload a binary file on the server, and get back a BeanMap
	 * 
	 * @param address
	 *            The service address.
	 * @param file
	 *            The file to upload.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 * @return response BeanMap if any
	 */
	public BeanMap uploadToBeanMap(String address, String resultType, File file) throws ServerErrorException {
		if ((address != null) && (address.length() > 0)) {
			Map<String, Object> parameters = new HashMap<String, Object>();
			if (file != null) {
				parameters.put("filename", file.getName());
			}
			String xml =  wsa.post(address, parameters, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM));
			if ((xml != null) && (xml.length() > 0)) {
				cache.purge(resultType);
				return (BeanMap) xs.fromXML(resultType, xml);
			}			
		}
		return null;
	}

	/**
	 * Upload a binary file on the server.
	 * 
	 * @param address
	 *            The service address.
	 * @param file
	 *            The file to upload.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public void upload(String address, Map<String, Object> parameters, File file) throws ServerErrorException {
		if ((address != null) && (address.length() > 0)) {
			wsa.post(address, parameters, new FileRepresentation(file, MediaType.APPLICATION_OCTET_STREAM));
		}
	}

	/**
	 * Download a properties file from de dedicated web service.
	 * 
	 * @param filekey
	 *            the file key name, corresponding to the first part of the file name (without language codes neither
	 *            the properties extension).
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Properties getProperties(String filekey) throws ServerErrorException {
		if ((filekey == null) || (filekey.length() == 0)) {
			return new Properties();
		}
		if (filekey.startsWith("/")) {
			filekey = filekey.substring(1);
		}
		return (Properties) getProps(PATH_PROPERTIES + filekey).getContent();
	}

	/**
	 * Download a properties file from de dedicated web service.
	 * 
	 * @param filekey
	 *            the file key name, corresponding to the first part of the file name (without language codes neither
	 *            the properties extension).
	 * @param locale
	 *            the locale variable corresponding to the available properties files (case of a resourceBundle)
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client mal formed request, connection
	 *             problem or a server internal error. It should be logged into User accessible journal and show to him
	 *             if it stop the current process.
	 */
	public Map<Locale, Properties> getProperties(String filekey, Collection<Locale> locales)
			throws ServerErrorException {
		HashMap<Locale, Properties> props = new HashMap<Locale, Properties>();
		if ((filekey == null) || (filekey.length() == 0)) {
			return props;
		}
		if (filekey.startsWith("/")) {
			filekey = filekey.substring(1);
		}
		for (Locale locale : locales) {
			props.put(locale, ((Properties) getProps(PATH_PROPERTIES + filekey, locale).getContent()));
		}
		return props;
	}

	/**
	 * Remove the corresponding BeanMap from the local cache (if it was sotred into it).
	 * 
	 * <p>
	 * This ensure that next call to a get method will call the server. this does not affect
	 * the BeanMap state on the server.
	 *  
	 * @param type
	 * @param id
	 */
	public void purgeCache(String type, int id) {
		cache.purge(PATH_DATA + type + '/' + id);
	}

	/**
	 * Remove the given BeanMap from the local cahe (if it was in stored into).
	 * 
	 * <p>
	 * This ensure that next call to a get method will call the server. this does not affect
	 * the BeanMap state on the server.

	 * @param bean
	 */
	public void purgeCache(BeanMap bean) {
		purgeCache(bean.getType(), bean.getId());
	}

	/**
	 * Define if before to get a BeanMap get test if the server have a newer version of it.
	 * @param test
	 */
	public void setTestBerofeGet(boolean test) {
		testBeforeGet = test;
	}
}
