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
package com.arcadsoftware.rest;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import org.osgi.framework.BundleContext;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.engine.util.DateUtils;
import org.restlet.representation.Representation;
import org.restlet.representation.RepresentationInfo;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.osgi.SysOutLogged;
import com.arcadsoftware.rest.internal.WebForwardServiceAccess;

/**
 * <p>If this resource is attached to an <code>OSGiApplication</code> then it can give
 * back the bundle context of this application to be able to track any OSGi services.
 * 
 * <p>Do not register any service within a Resource, the resource life cycle does not
 * allow to maintain an OSGi service.
 * 
 * <p>This resource implement an simple process to test the modification of resource data.
 * Its relay to the if-modified-since and if-unmodified-since HTTP conditional operations.
 * if the client use this condition into any other method than HEAD the final implementation
 * of this resource must check the dates.
 * 
 * <p>
 * Any descendant Resource should support application/xml and application/json MediaType in
 * response. If not use <b>setVariant</b> in <b>doInit</b> method to reset the default list.
 * Use <b>addVariant</b> to extend this list.
 * 
 * @see OSGiApplication
 */
public abstract class OSGiResource extends BaseResource {

	private Date lastModification;
	
	/**
	 * Return the bundle Context of the application.
	 * 
	 * <p>
	 * This method can return null if this resource is not attached to an OSGi Application.
	 * 
	 * @return a bundle context.  
	 */
	protected final BundleContext getBundleContext() {
		if ((getApplication() != null) && (getApplication() instanceof OSGiApplication)) {
			return ((OSGiApplication) getApplication()).getBundleContext();
		}
		return null;
	}
	
	/**
	 * Get the OSGiApplication associated to this resource.
	 * 
	 * @return null if this application is not an OSGi one.
	 */
	protected final OSGiApplication getOSGiApplication() {
		if ((getApplication() != null) && (getApplication() instanceof OSGiApplication)) {
			return (OSGiApplication) getApplication();
		}
		return null;
	}

	/**
	 * Get the OSGi configuration identified by the given PID.
	 * 
	 * @param pid The OSGi configuration PID.
	 * @return null if this configuration does not exist.
	 */
	protected final Dictionary<String, Object> getOSGiConfiguration(String pid) {
		ConfigurationAdmin ca = getOSGiService(ConfigurationAdmin.class);
		if (ca == null) {
			return null;
		}
		try {
			Configuration c = ca.getConfiguration(pid, null);
			if (c == null) {
				return new Hashtable<String, Object>();
			}
			return c.getProperties();
		} catch (IOException e) {
			return null;
		}
	}
	
	/**
	 * Get the OSGi logger facilities. Use for API requirement. Prefer the standard Restlet logger access when logging
	 * into a Restlet resource (or the local bundle logger interface when available).
	 *  
	 * <p>
	 * This method can return null if this resource is not attached to an OSGi Application.
	 * 
	 * @return The OSGi logger interface.
	 */
	protected final ILoggedPlugin getLoggedPlugin() {
		if ((getApplication() != null) && (getApplication() instanceof OSGiApplication)) {
			return ((OSGiApplication) getApplication()).getActivator();
		}
		return null;
	}

	/**
	 * Get a service currently registered for the given clazz.
	 * 
	 * <p>
	 * The result is valid at the time of the call to this method. However
	 * since the Framework is a very dynamic environment, the service can be
	 * modified or unregistered at any time.
	 * 
	 * @param <T> Type of Service
	 * @param clazz The class under whose name the service was registered. Must
	 *        not be {@code null}.
	 * @return null if there is no service registered for this clazz.
	 */
	protected final <T> T getOSGiService(Class<T> clazz) {
		List<T> services = getOSGiServices(clazz);
		if (services.size() > 0) {
			return services.get(0);
		}
		return null;
	}
	
	/**
	 * Get all OSGi Services currently registered.
	 * 
	 * <p>
	 * The collection is valid at the time of the call to this method. However
	 * since the Framework is a very dynamic environment, services can be
	 * modified or unregistered at any time.
	 * 
	 * @param <T> Type of Service
	 * @param clazz The class under whose name the service was registered. Must
	 *        not be {@code null}.
	 * @return never return null, return an empty list if there no service registered.
	 */
	protected final <T> List<T> getOSGiServices(Class<T> clazz) {
		final ArrayList<T> result = new ArrayList<T>();
		BundleContext context = getBundleContext();
		if (context != null) {
			try {
				Collection<ServiceReference<T>> srs = context.getServiceReferences(clazz, null);
				if ((srs != null) && !srs.isEmpty()) {
					for(ServiceReference<T> sr: srs) {
						T service = context.getService(sr);
						if (service != null) {
							result.add(service);
						}
					}
				}
			} catch (InvalidSyntaxException e) {}
		}
		return result;
	}

	/**
	 * Post an OSGi event from the Application owner Bundle.
	 * 
	 * @param topic The OSGi Event topic.
	 * @param properties The parameters sent with the event.
	 * @return true if the event has been sent, false if none event service are currently registered or if an error occurs.
	 */
	protected final boolean fireOSGiEvent(String topic, Map<String,Object> properties) {
		return fireOSGiEvent(topic, properties);
	}
	
	/**
	 * Post an OSGi event from the Application owner Bundle.
	 * 
	 * @param topic The OSGi Event topic.
	 * @param properties The parameters sent with the event.
	 * @param synchronous indicate if the message must be processed synchronously or not.
	 * @return true if the event has been sent, false if none event service are currently registered or if an error occurs.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected final boolean fireOSGiEvent(String topic, Map<String,Object> properties, boolean synchronous) {
		BundleContext context = getBundleContext();
		if (context == null) {
			return false;
		}
		ServiceReference[] srs = null;
		try {
			srs = context.getServiceReferences(EventAdmin.class.getName(), null);
		} catch (InvalidSyntaxException e1) {
			return false;
		}
		if ((srs == null) || (srs.length == 0)) {
			return false;
		}
		boolean sent = false;
		for (ServiceReference sr:srs) {
			EventAdmin ea = (EventAdmin) context.getService(sr);
			if (ea != null) {
				try {
					if (synchronous) {
						ea.sendEvent(new Event(topic,properties));
					} else {
						ea.postEvent(new Event(topic,properties));
					}
					sent = true;
				} catch (SecurityException e) {}
			}
		}
		return sent;
	}
	
	/**
	 * Set the last modification date used to perform some test when HEAD method is used by
	 * client. 
	 * 
	 * @param lastModification
	 */
	protected final void setLastModification(Date lastModification) {
		this.lastModification = lastModification;
	}

	/**
	 * @return the last modification of the resource (must be set into the final implementation).
	 */
	protected Date getLastModification() {
		return lastModification;
	}

    /**
     * Set-up method that can be overridden in order to initialize the state of
     * the resource. By default it does nothing.
     * 
     * @see org.restlet.resource.UniformResource#init(Context, Request, Response)
     */
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.OPTIONS);
		getAllowedMethods().add(Method.HEAD);
		setVariants(MEDIATYPES_BASE_XMLJSON);
	}

    /**
     * Returns a representation whose metadata will be returned to the client. A
     * variant parameter is passed to indicate which representation should be
     * returned if any.<br>
     * <br>
     * This method is only invoked if content negotiation has been enabled as
     * indicated by the {@link #isNegotiated()}, otherwise the {@link #head()}
     * method is invoked.<br>
     * <br>
     * The default implementation directly returns the variant if it is already
     * an instance of {@link Representation}. In other cases, you need to
     * override this method in order to provide your own implementation. *
     * 
     * @param variant
     *            The variant whose full representation must be returned.
     * @return The resource's representation.
     * @see #get(Variant)
     * @throws ResourceException
     */
	@Override
	protected Representation head(Variant variant) throws ResourceException {
		return head();
	}

    /**
     * Returns information about the resource's representation. Those metadata
     * are important for conditional method processing. The advantage over the
     * complete {@link Representation} class is that it is much lighter to
     * create. This method is only invoked if content negotiation has been
     * disabled as indicated by the {@link #isNegotiated()}, otherwise the
     * {@link #getInfo(Variant)} method is invoked.<br>
     * <br>
     * The default behavior is to invoke the {@link #get()} method.
     * 
     * @return Information about the resource's representation.
     * @throws ResourceException
     */
	@Override
	protected final RepresentationInfo getInfo() throws ResourceException {
		if (lastModification != null) {
			return new RepresentationInfo(MediaType.APPLICATION_XML, lastModification);
		}
		return new RepresentationInfo(MediaType.APPLICATION_XML);
	}

    /**
     * Returns information about the resource's representation. Those metadata
     * are important for conditional method processing. The advantage over the
     * complete {@link Representation} class is that it is much lighter to
     * create. A variant parameter is passed to indicate which representation
     * should be returned if any.<br>
     * <br>
     * This method is only invoked if content negotiation has been enabled as
     * indicated by the {@link #isNegotiated()}, otherwise the
     * {@link #getInfo(Variant)} method is invoked.<br>
     * <br>
     * The default behavior is to invoke the {@link #get(Variant)} method.
     * 
     * @param variant
     *            The variant whose representation information must be returned.
     * @return Information about the resource's representation.
     * @throws ResourceException
     */
	@Override
	protected RepresentationInfo getInfo(Variant variant) throws ResourceException {
		if (variant  == null) {
			return getInfo();
		}
		if (lastModification != null) {
			return new RepresentationInfo(variant, lastModification);
		}
		return new RepresentationInfo(variant, new Date());
	}

    /**
     * Returns a representation whose metadata will be returned to the client.
     * This method is only invoked if content negotiation has been disabled as
     * indicated by the {@link #isNegotiated()}, otherwise the
     * {@link #head(Variant)} method is invoked.
     * 
     * @return The resource's representation.
     * @throws ResourceException
     * @see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.3
     */
	@Override
	public final Representation head() {
		if (!isExisting()) { // just return a 404...
			throw new ResourceException(Status.CLIENT_ERROR_NOT_FOUND);
		} else if (lastModification != null) {
			// test the last modification date.
			Date date = getRequest().getConditions().getModifiedSince();
			if (date != null) {
				if (DateUtils.before(lastModification, date)) {
					getResponse().setStatus(Status.SUCCESS_OK);
				} else {
					getResponse().setStatus(Status.REDIRECTION_NOT_MODIFIED);
				}
			} else {
				date = getRequest().getConditions().getUnmodifiedSince();
				if ((date != null) && DateUtils.before(lastModification, date)) {
					getResponse().setStatus(Status.CLIENT_ERROR_PRECONDITION_FAILED);
				} else {
					getResponse().setStatus(Status.SUCCESS_OK);
				}
			}
		} else if ((getRequest().getConditions().getModifiedSince() != null) ||
				(getRequest().getConditions().getUnmodifiedSince() != null)) {
			// We certainly need to compute the body to answer to the question.
			return get();
		} else {
			getResponse().setStatus(Status.SUCCESS_OK);
		}
		return null;
	}

    /**
     * Deletes the resource and all its representations. A variant parameter is
     * passed to indicate which representation should be returned if any.<br>
     * <br>
     * This method is only invoked if content negotiation has been enabled as
     * indicated by the {@link #isNegotiated()}, otherwise the {@link #delete()}
     * method is invoked.<br>
     * <br>
     * The default behavior is to set the response status to
     * {@link Status#CLIENT_ERROR_METHOD_NOT_ALLOWED}.
     * 
     * @param variant
     *            The variant of the response entity.
     * @return The optional response entity.
     * @throws ResourceException
     * @see #get(Variant)
     * @see <a
     *      href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7"
     *      >HTTP DELETE method</a>
     */
	@Override
	protected Representation delete(Variant variant) throws ResourceException {
        setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		return null;
	}

    /**
     * Returns a full representation for a given variant. A variant parameter is
     * passed to indicate which representation should be returned if any.<br>
     * <br>
     * This method is only invoked if content negotiation has been enabled as
     * indicated by the {@link #isNegotiated()}, otherwise the {@link #get()}
     * method is invoked.<br>
     * <br>
     * The default behavior is to set the response status to
     * {@link Status#CLIENT_ERROR_METHOD_NOT_ALLOWED}.<br>
     * 
     * @param variant
     *            The variant whose full representation must be returned.
     * @return The resource's representation.
     * @see #get(Variant)
     * @throws ResourceException
     */
	@Override
	protected Representation get(Variant variant) throws ResourceException {
        setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		return null;
	}

    /**
     * Posts a representation to the resource at the target URI reference. A
     * variant parameter is passed to indicate which representation should be
     * returned if any.<br>
     * <br>
     * This method is only invoked if content negotiation has been enabled as
     * indicated by the {@link #isNegotiated()}, otherwise the
     * {@link #post(Representation)} method is invoked.<br>
     * <br>
     * The default behavior is to set the response status to
     * {@link Status#CLIENT_ERROR_METHOD_NOT_ALLOWED}.<br>
     * 
     * @param entity
     *            The posted entity.
     * @param variant
     *            The variant of the response entity.
     * @return The optional result entity.
     * @throws ResourceException
     * @see <a
     *      href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5"
     *      >HTTP POST method</a>
     */
	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
        setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		return null;
	}

    /**
     * Creates or updates a resource with the given representation as new state
     * to be stored. A variant parameter is passed to indicate which
     * representation should be returned if any.<br>
     * <br>
     * This method is only invoked if content negotiation has been enabled as
     * indicated by the {@link #isNegotiated()}, otherwise the
     * {@link #put(Representation)} method is invoked.<br>
     * <br>
     * The default behavior is to set the response status to
     * {@link Status#CLIENT_ERROR_METHOD_NOT_ALLOWED}.<br>
     * 
     * @param representation
     *            The representation to store.
     * @param variant
     *            The variant of the response entity.
     * @return The optional result entity.
     * @throws ResourceException
     * @see #get(Variant)
     * @see <a
     *      href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.6"
     *      >HTTP PUT method</a>
     */
	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
        setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		return null;
	}
	
	// The following overrides help developers to not override methods only used into resources with annotation...
	// Remove the method if needed...
	
	@Override
	protected final Representation delete() throws ResourceException {
		// Cette implémentation n'a pas d'utilité à être surchargé par les classes filles.
		// Si cela devient nécessaire supprimez cette surcharge finale.
		return super.delete();
	}

	@Override
	protected final Representation get() throws ResourceException {
		// Cette implémentation n'a pas d'utilité à être surchargé par les classes filles.
		// Si cela devient nécessaire supprimez cette surcharge finale.
		return super.get();
	}

	@Override
	protected final Representation post(Representation entity) throws ResourceException {
		// Cette implémentation n'a pas d'utilité à être surchargé par les classes filles.
		// Si cela devient nécessaire supprimez cette surcharge finale.
		return super.post(entity);
	}

	@Override
	protected final Representation put(Representation entity) throws ResourceException {
		// Cette implémentation n'a pas d'utilité à être surchargé par les classes filles.
		// Si cela devient nécessaire supprimez cette surcharge finale.
		return super.put(entity);
	}
	
	/**
	 * Get a HTTP Client Restlet facade that allow to forward the current user identity to
	 * another web-service.
	 * 
	 * <p>
	 * Please note that using this interface to call the same Server is quite ineffective !
	 * You should consider to call an OSGi Service instead.
	 *  
	 * @param targetServerURL The Server URL.
	 * @return a WebServiceAccess facade object. 
	 */
	protected WebServiceAccess getWebServiceAccess(String targetServerURL) {
		ILoggedPlugin activator = getLoggedPlugin();
		if (activator == null) {
			activator = new SysOutLogged();
		}
		WebServiceAccess wsa = new WebForwardServiceAccess(activator, getRequest());
		wsa.setDefaultServeraddress(targetServerURL);
		return wsa;
	}
}
