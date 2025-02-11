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
package com.arcadsoftware.osgi;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceFactory;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.log.LogEntry;

import com.arcadsoftware.osgi.internal.LogTracker;
import com.arcadsoftware.osgi.internal.Messages;

/**
 * This abstract Activator implement some helper methods and process to automatic services unregistration and easy
 * registration.
 * 
 * <p>
 * This class archive log message and transmit them to the OSGi log service when it will be available.
 * 
 * @see BundleActivator
 */
public abstract class AbstractActivator implements BundleActivator, ILoggedPlugin {

	/**
	 * An error message (Value 0).
	 * 
	 * <p>
	 * This message is always written in the log.
	 */
	public static final int LOG_AUDIT = 0;

	/**
	 * An error message (Value 1).
	 * 
	 * <p>
	 * This log entry indicates the bundle or service may not be functional.
	 */
	public static final int LOG_ERROR = 1;

	/**
	 * A warning message (Value 2).
	 * 
	 * <p>
	 * This log entry indicates a bundle or service is still functioning but may experience problems in the future
	 * because of the warning condition.
	 */
	public static final int LOG_WARNING = 2;
	
	/**
	 * An informational message (Value 3).
	 * 
	 * <p>
	 * This log entry may be the result of any change in the bundle or service and does not indicate a problem.
	 */
	public static final int LOG_INFO = 3;
	
	/**
	 * A debugging message (Value 4).
	 * 
	 * <p>
	 * This log entry is used for problem determination and may be irrelevant to anyone but the bundle developer.
	 */
	public static final int LOG_DEBUG = 4;
	
	/**
	 * A low level debugging message (Value 5).
	 * 
	 * <p>
	 * This log entry is used for problem determination and is irrelevant to anyone but the bundle developer.
	 */
	public static final int LOG_TRACE = 5;

	/**
	 * Return true if the given Bundle is a Fragment.
	 * 
	 * @param bundle
	 * @return
	 */
	public static boolean isFragment(Bundle bundle) {
		return bundle.getHeaders().get(Constants.FRAGMENT_HOST) != null;
	}

	/**
	 * Performs the specified PrivilegedAction with privileges enabled if and only if a Security Manager is active.
	 * 
	 * <p>
	 * The action is performed with all of the permissions possessed by the caller's protection domain.
	 * If the action's run method throws an (unchecked) exception, it will propagate through this method.
	 * Note that any DomainCombiner associated with the current AccessControlContext will be ignored while the action is performed.
	 * 
	 * @param <T> the type of the value returned by the PrivilegedAction's run method.
	 * @param action the action to be performed.
	 * @return the value returned by the action's run method.
	 * @throws NullPointerException if the action is null.
	 */
	public static <T> T doPrivileged(PrivilegedAction<T> action) {
		if (System.getSecurityManager() != null) {
			return AccessController.doPrivileged(action);
		}
		return action.run();
	}
	
	private BundleContext context;
	private final ArrayList<ServiceRegistration<?>> serviceRegistrationList = new ArrayList<ServiceRegistration<?>>();
	private LogTracker logTracker;
	private final ArrayList<BundleListener> bundleListenerList = new ArrayList<BundleListener>();

	/**
	 * Called when this bundle is started so the Framework can perform the
	 * bundle-specific activities necessary to start this bundle. This method
	 * can be used to register services or to allocate any resources that this
	 * bundle needs.
	 * 
	 * <p>
	 * This method must complete and return to its caller in a timely manner.
	 * 
	 * @param context The execution context of the bundle being started.
	 * @throws Exception If this method throws an exception, this
	 *         bundle is marked as stopped and the Framework will remove this
	 *         bundle's listeners, unregister all services registered by this
	 *         bundle, and release all services used by this bundle.
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		this.context = bundleContext;
		// Track for OSGi logger.
		String name = getLoggerName();
		if ((name == null) || name.isBlank()) {
			name = bundleContext.getBundle().getSymbolicName();
		}
		logTracker = new LogTracker(bundleContext, name);
		logTracker.open();
	}

	/**
	 * Get the logger name used by this AbstractActivator class. Default implementation use the Bundle symbolic name as Logger name.
	 * 
	 * <p>
	 * This method may be overridden to provide a different name.
	 * 
	 * @return a non null Logger name.
	 */
	protected String getLoggerName() {
		return null;
	}

	/**
	 * Called when this bundle is stopped so the Framework can perform the
	 * bundle-specific activities necessary to stop the bundle. In general, this
	 * method should undo the work that the <code>BundleActivator.start</code>
	 * method started. There should be no active threads that were started by
	 * this bundle when this bundle returns. A stopped bundle must not call any
	 * Framework objects.
	 * 
	 * <p>
	 * This method must complete and return to its caller in a timely manner.
	 * 
	 * @param context The execution context of the bundle being stopped.
	 * @throws Exception If this method throws an exception, the
	 *         bundle is still marked as stopped, and the Framework will remove
	 *         the bundle's listeners, unregister all services registered by the
	 *         bundle, and release all services used by the bundle.
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		try {
			// Automatically unregister any registered services...
			try {
				for (ServiceRegistration<?> element : serviceRegistrationList) {
					if (element != null) {
						Object service = bundleContext.getService(element.getReference());
						if (service instanceof Closeable) {
							try {
								((Closeable) service).close();
							} catch (Exception e) {
								info("Error will terminating service: " + service.getClass().getCanonicalName(), e);
							}
						}
						element.unregister();
					}
				}
			} finally {
				serviceRegistrationList.clear();
			}
		} catch (ConcurrentModificationException e) {
			// Il peut y avoir des accès concurent si le stop est
			// appelé pendant qu'un enregistrement n'est pas terminé (ou bloqué).
		}
		try {
			for (BundleListener bl: bundleListenerList) {
				try {
					bundleContext.removeBundleListener(bl);
				} catch(Throwable e) {}
			}
		} catch (ConcurrentModificationException e) {
			// Il peut y avoir des accès concurent si le stop est
			// appelé pendant qu'un enregistrement n'est pas terminé (ou bloqué).
		}
		if (logTracker != null) {
			logTracker.close();
			logTracker = null;
		}
		context = null;
	}

	/**
	 * Register the specified OSGi Service with the framework under the specified class name. A single property to this
	 * service (given by <code>key</code> and <code>value</code>).
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param service
	 *            The service object or a <code>ServiceFactory</code> object.
	 * @param key
	 *            The property key.
	 * @param value
	 *            The property value.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public ServiceRegistration<?> registerService(String clazz, Object service, String key, Object value) {
		Hashtable<String, Object> properties = new Hashtable<String, Object>();
		properties.put(key, value);
		return registerService(clazz, service, properties);
	}

	/**
	 * Register the specified OSGi Service with the framework under the specified class name. A single property to this
	 * service (given by <code>key</code> and <code>value</code>).
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param service
	 *            The service object.
	 * @param key
	 *            The property key.
	 * @param value
	 *            The property value.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public <T> ServiceRegistration<T> registerService(Class<T> clazz, T service, String key, Object value) {
		Hashtable<String, Object> properties = new Hashtable<String, Object>();
		properties.put(key, value);
		return registerService(clazz, service, properties);
	}

	/**
	 * Register the specified OSGi Service with the framework under the specified class name. A single property to this
	 * service (given by <code>key</code> and <code>value</code>).
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param serviceFactory
	 *            The <code>ServiceFactory</code> object.
	 * @param key
	 *            The property key.
	 * @param value
	 *            The property value.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public <T> ServiceRegistration<T> registerService(Class<T> clazz, ServiceFactory<T> serviceFactory, String key, Object value) {
		Hashtable<String, Object> properties = new Hashtable<String, Object>();
		properties.put(key, value);
		return registerService(clazz, serviceFactory, properties);
	}

	/**
	 * Register the specified OSGi Service with the framework under the specified class name.
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param service
	 *            The service object.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public ServiceRegistration<?> registerService(String clazz, Object service) {
		return registerService(clazz, service, new Hashtable<String, Object>());
	}

	/**
	 * Register the specified OSGi Service with the framework under the specified class name.
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param serviceFactory
	 *            The <code>ServiceFactory</code> object.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public <T> ServiceRegistration<T> registerService(Class<T> clazz, ServiceFactory<T> serviceFactory) {
		return registerService(clazz, serviceFactory, new Hashtable<String, Object>());
	}

	/**
	 * Register the specified OSGi Service with the framework under the specified class name.
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param service
	 *            The service object or a <code>ServiceFactory</code> object.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public <T> ServiceRegistration<T> registerService(Class<T> clazz, T service) {
		return registerService(clazz, service, new Hashtable<String, Object>());
	}

	/**
	 * Register the specified OSGi Service with the specified properties with the framework under the specified class
	 * name.
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param service
	 *            The service object or a <code>ServiceFactory</code> object.
	 * @param properties
	 *            The properties for this service.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public ServiceRegistration<?> registerService(String clazz, Object service, Dictionary<String, ?> properties) {
		ServiceRegistration<?> reg = context.registerService(clazz, service, properties);
		if (reg != null) {
			serviceRegistrationList.add(reg);
		}
		return reg;
	}

	/**
	 * Register the specified OSGi Service with the specified properties with the framework under the specified class
	 * name.
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param service
	 *            The service object.
	 * @param properties
	 *            The properties for this service.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public <T> ServiceRegistration<T> registerService(Class<T> clazz, T service, Dictionary<String, ?> properties) {
		ServiceRegistration<T> reg = context.registerService(clazz, service, properties);
		if (reg != null) {
			serviceRegistrationList.add(reg);
		}
		return reg;
	}

	/**
	 * Register the specified OSGi Service with the specified properties with the framework under the specified class
	 * name.
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class name under which the service can be located.
	 * @param serviceFactory
	 *            The service factory object.
	 * @param properties
	 *            The properties for this service.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see #registerService(java.lang.String[], java.lang.Object, java.util.Dictionary)
	 */
	public <T> ServiceRegistration<T> registerService(Class<T> clazz, ServiceFactory<T> serviceFactory, Dictionary<String, ?> properties) {
		ServiceRegistration<T> reg = context.registerService(clazz, serviceFactory, properties);
		if (reg != null) {
			serviceRegistrationList.add(reg);
		}
		return reg;
	}

	/**
	 * Registers the specified service object with the specified properties under the specified class names into the
	 * Framework. A <code>ServiceRegistration</code> object is returned. The <code>ServiceRegistration</code> object is
	 * for the private use of the bundle registering the service and should not be shared with other bundles. The
	 * registering bundle is defined to be the context bundle. Other bundles can locate the service by using either the
	 * {@link org.osgi.framework.BundleContext#getServiceReferences} or
	 * {@link org.osgi.framework.BundleContext#getServiceReference} method.
	 * 
	 * <p>
	 * A bundle can register a service object that implements the {@link org.osgi.framework.ServiceFactory} interface to
	 * have more flexibility in providing service objects to other bundles.
	 * 
	 * <p>
	 * The following steps are required to register a service:
	 * <ol>
	 * <li>If <code>service</code> is not a <code>ServiceFactory</code>, an <code>IllegalArgumentException</code> is
	 * thrown if <code>service</code> is not an <code>instanceof</code> all the classes named.
	 * <li>The Framework adds these service properties to the specified <code>Dictionary</code> (which may be
	 * <code>null</code>): a property named {@link org.osgi.framework.Constants#SERVICE_ID} identifying the registration
	 * number of the service and a property named {@link org.osgi.framework.Constants#OBJECTCLASS} containing all the
	 * specified classes. If any of these properties have already been specified by the registering bundle, their values
	 * will be overwritten by the Framework.
	 * <li>The service is added to the Framework service registry and may now be used by other bundles.
	 * <li>A service event of type {@link org.osgi.framework.ServiceEvent#REGISTERED} is fired.
	 * <li>A <code>ServiceRegistration</code> object for this registration is returned.
	 * </ol>
	 * 
	 * <p>
	 * The abstract Activator is in charge with the unregistration of this service. For service that do not follow the
	 * life-cycle of the bundle please use the BundleContext methods (Do not forget to unregister the service in this
	 * case). If the service implement the interface Closeable then the close() method will be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param clazz
	 *            The class names under which the service can be located. The class names in this array will be stored
	 *            in the service's properties under the key {@link org.osgi.framework.Constants#OBJECTCLASS}.
	 * @param service
	 *            The service object or a <code>ServiceFactory</code> object.
	 * @param properties
	 *            The properties for this service. The keys in the properties object must all be <code>String</code>
	 *            objects. See {@link org.osgi.framework.Constants} for a list of standard service property keys.
	 *            Changes should not be made to this object after calling this method. To update the service's
	 *            properties the {@link org.osgi.framework.ServiceRegistration#setProperties} method must be called. The
	 *            set of properties may be <code>null</code> if the service has no properties.
	 * 
	 * @return A <code>ServiceRegistration</code> object for use by the bundle registering the service to update the
	 *         service's properties.
	 * 
	 * @throws java.lang.IllegalArgumentException
	 *             If one of the following is true:
	 *             <ul>
	 *             <li><code>service</code> is <code>null</code>. <li><code>service</code> is not a <code>ServiceFactory
	 *             </code> object and is not an instance of all the named classes in <code>clazzes</code>. <li><code>
	 *             properties</code> contains case variants of the same key name.
	 *             </ul>
	 * 
	 * @throws java.lang.SecurityException
	 *             If the caller does not have the <code>ServicePermission</code> to register the service for all the
	 *             named classes and the Java Runtime Environment supports permissions.
	 * 
	 * @throws java.lang.IllegalStateException
	 *             If this BundleContext is no longer valid.
	 * 
	 * @see ServiceRegistration
	 * @see ServiceFactory
	 */
	public ServiceRegistration<?> registerService(String[] clazz, Object service, Dictionary<String, ?> properties) {
		ServiceRegistration<?> reg = context.registerService(clazz, service, properties);
		if (reg != null) {
			serviceRegistrationList.add(reg);
		}
		return reg;
	}

	/**
	 * Unregisters a service. Remove a <code>ServiceRegistration</code> object from the Framework service registry. All
	 * <code>ServiceReference</code> objects associated with this <code>ServiceRegistration</code> object can no longer
	 * be used to interact with the service.
	 * 
	 * <p>
	 * The following steps are required to unregister a service:
	 * <ol>
	 * <li>The service is removed from the Framework service registry so that it can no longer be used.
	 * <code>ServiceReference</code> objects for the service may no longer be used to get a service object for the
	 * service.
	 * <li>A service event of type {@link org.osgi.framework.ServiceEvent#UNREGISTERING} is fired so that bundles using
	 * this service can release their use of it.
	 * <li>For each bundle whose use count for this service is greater than zero: <br>
	 * The bundle's use count for this service is set to zero. <br>
	 * If the service was registered with a {@link org.osgi.framework.ServiceFactory} object, the
	 * <code>ServiceFactory.ungetService</code> method is called to release the service object for the bundle.
	 * </ol>
	 * 
	 * <p>
	 * If the service implement the interface Closeable then the close() method is be automatically called 
	 * when the service is unregistered.
	 * 
	 * @param serviceRegistration
	 *            the service to unregister.
	 * @throws java.lang.IllegalStateException
	 *             If this <code>ServiceRegistration</code> object has already been unregistered.
	 * @see BundleContext#ungetService
	 * @see ServiceFactory#ungetService
	 */
	public void unregister(ServiceRegistration<?> serviceRegistration) {
		if (serviceRegistration != null) {
			serviceRegistrationList.remove(serviceRegistration);
			BundleContext ctx = context;
			if (ctx != null) {
				final Object service = ctx.getService(serviceRegistration.getReference());
				if (service instanceof Closeable) {
					try {
						((Closeable) service).close();
					} catch (Exception e) {
						info("Error will terminating service: " + service.getClass().getCanonicalName(), e);
					}
				}
			}
			serviceRegistration.unregister();
		}
	}

	/**
	 * Utility method that register a IStarttask. This task will be executed when all
	 * the required constraints will be met.
	 *  
	 * @param task
	 * @return
	 * @see IStartTask
	 */
	public ServiceRegistration<?> registerStartTask(AbstractStartTask task) {
		return registerService(AbstractStartTask.clazz, task, task.getConstraints());
	}
	
	/**
	 * Get the Bundle context, this helper method can return null before the bundle start and after the bundle stop.
	 * 
	 * @return The bundle context.
	 */
	public BundleContext getContext() {
		return context;
	}

	/**
	 * Logs a message with an exception associated and a
	 * <code>ServiceReference</code> object.
	 * 
	 * @param sr The <code>ServiceReference</code> object of the service that this
	 *        message is associated with.
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param exception The exception that reflects the condition or
	 *        <code>null</code>.
	 * @see #LOG_AUDIT
	 * @see #LOG_ERROR
	 * @see #LOG_WARNING
	 * @see #LOG_INFO
	 * @see #LOG_DEBUG
	 * @see #LOG_TRACE
	 * @see org.osgi.service.log.LogService#log(org.osgi.framework.ServiceReference, int, java.lang.String,
	 * java.lang.Throwable)
	 */
	public void log(ServiceReference<?> sr, int level, String message, Throwable e) {
		if (logTracker != null) {
			if (e != null) {
				if (sr != null) {
					logTracker.log(level, message, sr, e);
				} else {
					logTracker.log(level, message, e);
				}
			} else if (sr != null) {
				logTracker.log(level, message, sr);
			} else {
				logTracker.log(level, message);
			}
		} else {
			if (level >= LOG_ERROR) {
				System.err.println(message);
			} else {
				System.out.println(message);
			}
			if (e != null) {
				if (level < LOG_WARNING) {
					e.printStackTrace();
				} else {
					System.err.println("EXCEPTION MESSAGE: " + e.getLocalizedMessage()); //$NON-NLS-1$
				}
			}
		}
	}
	
	/**
	 * Log a formatted message with the OSGi the LoggerFactory once the Logger determines the log
	 * level is enabled. Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 * @see #LOG_AUDIT
	 * @see #LOG_ERROR
	 * @see #LOG_WARNING
	 * @see #LOG_INFO
	 * @see #LOG_DEBUG
	 * @see #LOG_TRACE
	 * @see org.osgi.service.log.Logger
	 */
	public void log(int level, String formattedMessage, Object... arguments) {
		if (logTracker != null) {
			logTracker.log(level, formattedMessage, arguments);
		} else {
			if (level >= LOG_ERROR) {
				System.err.println(formattedMessage);
			} else {
				System.out.println(formattedMessage);
			}
			for (Object o: arguments) {
				if (o instanceof Throwable) {
					((Throwable) o).printStackTrace();
				}
			}
		}
	}

	/**
	 * Logs a message with an exception associated and a <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with.
	 * @param level
	 *            The severity of the message. This should be one of the defined log levels but may be any integer that
	 *            is interpreted in a user defined way.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param exception
	 *            The exception that reflects the condition or <code>null</code>.
	 * @see #LOG_AUDIT
	 * @see #LOG_ERROR
	 * @see #LOG_WARNING
	 * @see #LOG_INFO
	 * @see #LOG_DEBUG
	 * @see #LOG_TRACE
	 * @see org.osgi.service.log.LogService#log(int, java.lang.String, java.lang.Throwable)
	 */
	public void log(int level, String message, Throwable e) {
		log(null, level, message, e);
	}

	/**
	 * Logs a message.
	 * 
	 * <p>
	 * The <code>ServiceReference</code> field and the <code>Throwable</code> field
	 * of the <code>LogEntry</code> object will be set to <code>null</code>.
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @see #LOG_AUDIT
	 * @see #LOG_ERROR
	 * @see #LOG_WARNING
	 * @see #LOG_INFO
	 * @see #LOG_DEBUG
	 * @see #LOG_TRACE
	 * @see org.osgi.service.log.LogService#log(int, java.lang.String)
	 */
	public void log(int level, String message) {
		log(null, level, message, null);
	}

	/**
	 * Logs a message associated with a specific <code>ServiceReference</code>
	 * object.
	 * 
	 * <p>
	 * The <code>Throwable</code> field of the <code>LogEntry</code> will be set to
	 * <code>null</code>.
	 * 
	 * @param sr The <code>ServiceReference</code> object of the service that this
	 *        message is associated with or <code>null</code>.
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @see #LOG_AUDIT
	 * @see #LOG_ERROR
	 * @see #LOG_WARNING
	 * @see #LOG_INFO
	 * @see #LOG_DEBUG
	 * @see #LOG_TRACE
	 * @see org.osgi.service.log.LogService#log(org.osgi.framework.ServiceReference, int, java.lang.String)
	 */
	public void log(ServiceReference<?> sr, int level, String message) {
		log(sr, level, message, null);
	}

	@Override
	public void log(String message) {
		log(null, LOG_INFO, message, null);
	}

	@Override
	public void log(String message, Throwable e) {
		log(null, LOG_INFO, message, e);
	}

	@Override
	public void log(Throwable e) {
		if (e != null) {
			log(null, LOG_INFO, e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Logs a debug message associated with a specific <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with or
	 *            <code>null</code>.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void trace(ServiceReference<?> sr, String message) {
		log(sr, LOG_TRACE, message, null);
	}

	@Override
	public void trace(String message) {
		log(null, LOG_TRACE, message, null);
	}

	@Override
	public void trace(String message, Throwable e) {
		log(null, LOG_TRACE, message, e);
	}
	
	@Override
	public void trace(Throwable e) {
		if (e != null) {
			log(null, LOG_TRACE, e.getLocalizedMessage(), e);
		}
	}
	
	@Override
	public void trace(String message, Object... objects) {
		log(LOG_TRACE, message, objects);
	}

	/**
	 * Logs a debug message associated with a specific <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with or
	 *            <code>null</code>.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void debug(ServiceReference<?> sr, String message) {
		log(sr, LOG_DEBUG, message, null);
	}

	@Override
	public void debug(String message) {
		log(null, LOG_DEBUG, message, null);
	}

	@Override
	public void debug(String message, Throwable e) {
		log(null, LOG_DEBUG, message, e);
	}
	
	@Override
	public void debug(Throwable e) {
		if (e != null) {
			log(null, LOG_DEBUG, e.getLocalizedMessage(), e);
		}
	}
	
	@Override
	public void debug(String message, Object... objects) {
		log(LOG_DEBUG, message, objects);
	}

	/**
	 * Logs an informal message associated with a specific <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with or
	 *            <code>null</code>.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void info(ServiceReference<?> sr, String message) {
		log(sr, LOG_INFO, message, null);
	}

	@Override
	public void info(String message) {
		log(null, LOG_INFO, message, null);
	}
	
	@Override
	public void info(String message, Throwable e) {
		log(null, LOG_INFO, message, e);
	}
	
	@Override
	public void info(Throwable e) {
		if (e != null) {
			log(null, LOG_INFO, e.getLocalizedMessage(), e);
		}
	}
	
	@Override
	public void info(String message, Object... objects) {
		log(LOG_INFO, message, objects);
	}

	/**
	 * Logs a warning message associated with a specific <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with or
	 *            <code>null</code>.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            The exception that reflects the condition or <code>null</code>.
	 */
	public void warn(ServiceReference<?> sr, String message, Throwable e) {
		log(sr, LOG_WARNING, message, e);
	}

	@Override
	public void warn(String message, Throwable e) {
		log(null, LOG_WARNING, message, e);
	}

	@Override
	public void warn(String message) {
		log(null, LOG_WARNING, message, null);
	}

	@Override
	public void warn(Throwable e) {
		if (e != null) {
			log(null, LOG_WARNING, e.getLocalizedMessage(), e);
		}
	}
	
	@Override
	public void warn(String message, Object... objects) {
		log(LOG_WARNING, message, objects);
	}

	/**
	 * Logs a error message associated with a specific <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with or
	 *            <code>null</code>.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            The exception that reflects the condition or <code>null</code>.
	 */
	public void error(ServiceReference<?> sr, String message, Throwable e) {
		log(sr, LOG_ERROR, message, e);
	}

	@Override
	public void error(String message, Throwable e) {
		log(null, LOG_ERROR, message, e);
	}

	@Override
	public void error(String message) {
		log(null, LOG_ERROR, message);
	}

	@Override
	public void error(Throwable e) {
		if (e != null) {
			log(null, LOG_ERROR, e.getLocalizedMessage(), e);
		}
	}
	
	@Override
	public void error(String message, Object... objects) {
		log(LOG_ERROR, message, objects);
	}

	/**
	 * Logs a error message associated with a specific <code>ServiceReference</code> object.
	 * 
	 * @param sr
	 *            The <code>ServiceReference</code> object of the service that this message is associated with or
	 *            <code>null</code>.
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            The exception that reflects the condition or <code>null</code>.
	 */
	public void audit(ServiceReference<?> sr, String message, Throwable e) {
		log(sr, LOG_AUDIT, message, e);
	}

	@Override
	public void audit(String message, Throwable e) {
		log(null, LOG_AUDIT, message, e);
	}

	@Override
	public void audit(String message) {
		log(null, LOG_AUDIT, message);
	}

	@Override
	public void audit(Throwable e) {
		if (e != null) {
			log(null, LOG_AUDIT, e.getLocalizedMessage(), e);
		}
	}
	
	@Override
	public void audit(String message, Object... objects) {
		log(LOG_AUDIT, message, objects);
	}

	/**
	 * Look for a Bundle in the platform.
	 * 
	 * <p>
	 * This method returns a bundle installed in the OSGi environment at the time of the call to this method. However,
	 * since the Framework is a very dynamic environment, bundles can be installed or uninstalled at anytime.
	 * 
	 * @param symbolicName
	 * @return the Bundle object or null if not found.
	 */
	public Bundle findBundle(String symbolicName) {
		if ((context != null) && (symbolicName != null)) {
			for (Bundle bundle : context.getBundles()) {
				if (symbolicName.equals(bundle.getSymbolicName())) {
					return bundle;
				}
			}
		}
		return null;
	}

	/**
	 * Test if the specified bundle is started.
	 * 
	 * @param symbolicName 
	 * @return true if the given bundle is started.
	 */
	public boolean isBundleStarted(String symbolicName) {
		if ((context != null) && (symbolicName != null)) {
			for (Bundle bundle : context.getBundles()) {
				if (symbolicName.equals(bundle.getSymbolicName())) {
					return bundle.getState() == Bundle.ACTIVE;
				}
			}
		}
		return false;
	}
		
	/**
	 * Test if the specified bundles are started.
	 * 
	 * @param symbolicName 
	 * @return true if all the given bundles are started.
	 */
	public boolean isBundleStarted(String[] symbolicNames) {
		if ((context == null) || (symbolicNames == null) || (symbolicNames.length == 0)) {
			return false;
		}
		for (String symbolicName: symbolicNames) {
			if (!isBundleStarted(symbolicName)){
				return false;
			} 
		}
		return true;
	}
		
	/**
	 * Run the given <code>runnable</code> when the bundle will be started.
	 * 
	 *  <p>
	 *  If the target bundle is already started the runnable is run now, in the current thread. If not it will be run into another thread. 
	 * @param symbolicName
	 * @param runnable
	 */
	public void waitBundleStart(final String symbolicName, final Runnable runnable) {
		if (symbolicName == null) {
			return;
		}
		if (isBundleStarted(symbolicName)) {
			runnable.run();
		}
		if (context != null) {
			addBundleListener(new BundleListener() {
				public void bundleChanged(BundleEvent event) {
					if ((event.getType() == BundleEvent.STARTED) && //
							symbolicName.equals(event.getBundle().getSymbolicName())) {
						runnable.run();
						AbstractActivator.this.context.removeBundleListener(this);
						AbstractActivator.this.bundleListenerList.remove(this);
					}
				}
			});
		}
	}
	
	/**
	 * Run the given <code>runnable</code> when the bundle will be started.
	 * 
	 *  <p>
	 *  If the target bundle is already started the runnable is run now, in the current thread. If not it will be run into another thread. 
	 * @param symbolicName
	 * @param runnable
	 */
	public void waitBundleStart(final String[] symbolicNames, final Runnable runnable) {
		if ((symbolicNames == null) || (symbolicNames.length==0)){
			return;
		}
		if (isBundleStarted(symbolicNames)) {
			runnable.run();
		} else {
			if (context != null) {
				addBundleListener(new BundleListener() {
					public void bundleChanged(BundleEvent event) {
						if ((event.getType() == BundleEvent.STARTED) && isBundleStarted(symbolicNames)) {
							runnable.run();
							AbstractActivator.this.context.removeBundleListener(this);
							AbstractActivator.this.bundleListenerList.remove(this);
						}
					}
				});
			} 
		}
	}
		
	/**
	 * Adds the specified {@code BundleListener} object to the context bundle's
	 * list of listeners if not already present. BundleListener objects are
	 * notified when a bundle has a lifecycle state change.
	 * 
	 * <p>
	 * If the context bundle's list of listeners already contains a listener
	 * {@code l} such that {@code (l==listener)}, this method does nothing.
	 * 
	 * @param listener The {@code BundleListener} to be added.
	 */
	public void addBundleListener(final BundleListener bundleListener) {
		if (!bundleListenerList.contains(bundleListener)) {
			bundleListenerList.add(bundleListener);
			try {
				context.addBundleListener(bundleListener);
			} catch (Exception e) {
				bundleListenerList.remove(bundleListener);
			}
		}
	}
	
	/**
	 * Get a file from a bundle ACTIVATED.
	 * 
	 * <p>The specified path is always relative to the root of the specified bundle and may begin with "/". 
	 * A path value of "/" indicates the root of the bundle.</p>
	 * 
	 * <p>Note: Jar and zip files are not required to include directory entries. URLs to directory 
	 * entries will not be returned if the bundle contents do not contain directory entries.</p>
	 * 
	 * @param bundle a Bundle
	 * @param filename a file name.
	 * @return null or the corresponding File.
	 */
	public File getBundleFile(Bundle bundle, String filename) {
		if ((filename == null) || (filename.length() == 0)) {
			return null;
		}
		if (bundle == null) {
			if (context == null) {
				return null;
			}
			bundle = context.getBundle();
		}
		// Do not use bundle classpath.
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
			if (url == null) {
				return null;
			}
		}
		try {
			url = FileLocator.toFileURL(url);
			return new File(new URL(null,url.toString().replace(" ", "%20")).toURI()); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (URISyntaxException e) {
			debug(Messages.getString("AbstractActivator.InvalidFileName") + e.getLocalizedMessage(), e); //$NON-NLS-1$
			try {
				return new File(url.getPath());
			} catch (Exception t) {
				debug(t);
			}
		} catch (Exception e) {
			error(Messages.getString("AbstractActivator.FileSystemNotAvailable"), e); //$NON-NLS-1$
		}
		return null;
	}

	/**
	 * Get a file from the current Bundle.
	 * 
	 * <p>The specified path is always relative to the root of this bundle and may begin with "/". 
	 * A path value of "/" indicates the root of this bundle.</p>
	 * 
	 * <p>Note: Jar and zip files are not required to include directory entries. URLs to directory 
	 * entries will not be returned if the bundle contents do not contain directory entries.</p>
	 * 
	 * @param filename a file name.
	 * @return null of the corresponding File.
	 */
	public File getBundleFile(String filename) {
		return getBundleFile(null, filename);
	}

	/**
	 * Utility method to get a service instance.
	 * 
	 * @param clazz the OSGi service "class" name.
	 * @return null if no services are available.
	 */
	public Object getService(String clazz) {
		if (context == null) {
			return null;
		}
		try {
			ServiceReference<?> sr = context.getServiceReference(clazz);
			if (sr == null) {
				return null;
			}
			return context.getService(sr);
		} catch (IllegalStateException e) {
			debug(e);
			return null;
		}
	}
	
	/**
	 * Utility method to get a service instance.
	 * 
	 * @param clazz the OSGi service "class" name.
	 * @return an empty array if no services where found.
	 */
	public <T> List<T> getServices(Class<T> clazz) {
		try {
			Collection<ServiceReference<T>> srs = context.getServiceReferences(clazz, null);
			if (srs != null) {
				ArrayList<T> rs = new ArrayList<T>(srs.size());
				for (ServiceReference<T> sr: srs) {
					T r = context.getService(sr);
					if (r != null) {
						rs.add(r);
					}
				}
				return rs;
			}
		} catch (IllegalStateException e) {
			debug(e);
		} catch (InvalidSyntaxException e) {
			debug(e);
		};
		return new ArrayList<T>();
	}
	
	/**
	 * Utility method to get a service instance.
	 * 
	 * @param clazz the OSGi service "class" name.
	 * @param filter
	 * @return null if no services are available.
	 */
	public Object getService(String clazz, String filter) {
		ServiceReference<?>[] sr;
		try {
			sr = context.getServiceReferences(clazz, filter);
		} catch (InvalidSyntaxException e) {
			return null;
		}
		if ((sr == null) || (sr.length == 0)) {
			return null;
		}
		return context.getService(sr[0]);
	}
	
	/**
	 *  Utility method to get a service instance.
	 * 
	 * @param <T> The service interface class.
	 * @param serviceClass the interface service class (assume the name to be the services identifier).
	 * @return
	 */
	public <T> T getService(Class<T> serviceClass) {
		ServiceReference<?>[] srs = null;
		try {
			srs = context.getServiceReferences(serviceClass.getName(), null);
		} catch (InvalidSyntaxException e) {
			// Can not occurs (the filter is null).
		}
		if (srs == null) {
			return null;
		}
		for (ServiceReference<?> sr:srs) {
			Object o = context.getService(sr);
			if (serviceClass.isInstance(o)) {
				return serviceClass.cast(o);
			}
		}
		return null;
	}
	
	/**
	 * Get the configuration of the given Bundle name or PID.
	 * 
	 * <p>
	 * This method return null if the Configuration Admin Service is not ready to process the request.
	 * If this method must be used during the initialisation process the best place to do it
	 * is into an <b>initializeConfiguration</b> method.
	 * 
	 * @param pid The bundle symbolic name of the configuration PID.
	 * @return null is the ConfigurationAdmin is not ready or if this configuration does not exists.
	 * @see #setConfiguration(String, Dictionary)
	 */
	public Dictionary<String, Object> getConfiguration(final String pid) {
		ConfigurationAdmin ca = getService(ConfigurationAdmin.class);
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
	 * Set the configuration of another bundle or service.
	 * 
	 * The given properties are merged to the existing ones.
	 * 
	 * @param pid The bundle symbolic name of the configuration PID.
	 * @param props the properties to set.
	 * @see #getConfiguration(String)
	 */
	public void setConfiguration(final String pid, final Dictionary<String, Object> props) {
		if ((props == null) || props.isEmpty()) {
			return;
		}
		ConfigurationAdmin ca = getService(ConfigurationAdmin.class);
		if (ca == null) {
			return;
		}
		try {
			Configuration c = ca.getConfiguration(pid, null);
			if (c != null) {
				Dictionary<String, Object> p = c.getProperties();
				if (p == null) {
					p = props;
				} else {
					Enumeration<String> e = props.keys();
					while (e.hasMoreElements()) {
						String k = e.nextElement();
						p.put(k, props.get(k));
					}
				}
				// Force reallocation of configuration.
				c.setBundleLocation(null);
				// update the new configuration.
				c.update(p);
			}
		} catch (IOException e) {
			debug(e);
		}
	}
	
	/**
	 * Resolve an internal URL from a Bundle (or even a local file system URL !) to an external file, on the local file system.
	 * 
	 * @param url
	 * @return
	 */
	public File toFile(URL url) {
		if (url != null) {
			try {
				url = FileLocator.toFileURL(url);
				return new File(new URI(url.getProtocol(), url.getPath(), null));
			} catch (URISyntaxException e) {
				debug("Invalid file Name: " + e.getLocalizedMessage(), e); //$NON-NLS-1$
				try {
					return new File(url.getPath());
				} catch (Throwable t) {
					debug(t);
				}
			} catch (Throwable e) {
				error("File system no available.", e); //$NON-NLS-1$
			}
		}
		return null;
	}
}
