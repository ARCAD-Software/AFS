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

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.IllegalFormatException;
import java.util.Iterator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONObject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.osgi.framework.Bundle;
import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.EmptyRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;

import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.internal.RestProgressMonitor;

/**
 * This class manage long operation that are start by a Web-Service interaction. 
 * 
 * <p>
 * This communication pattern is based on a PULL mechanisms:
 * 
 * <ul>
 * <li>A POST request is send on the resource path to start the operation, see method {@link #startOperation(IConnectionUserBean, Form, IProgressMonitor)}.
 * <li>This request return a "operation in progress" object, with a unique identifier.
 * <li>A GET on this path return the current state of all operations.
 * <li>A GET on the path /id return the state of the corresponding task.
 * <li>A DELETE on the path /id cancel the corresponding operation. 
 * </ul>
 * 
 * <p>
 * This Restlet support empty body requests. and multiple id selection (with 'id+id+...' strings). It generate JSON or XML responses.
 * 
 * <p>
 * The operation management keep the trace of the operation during a limited time, and the Monitor tasks name may, or may not, be conserved too.
 * 
 * <p>
 * To use this class you just have to implement the <code>startOperation</code> method. 
 * But, this class implement the <code>Closeable</code> interface, it is recommended to call the {@link #close()} method when the resource is detached.
 * You also have to manage access rights within this <code>startOperation</code> method, and with the <code>onRead</code> and <code>onDelete</code> methods.
 * 
 * @author ARCAD Software
 */
public abstract class ProgressMonitoredRestlet extends Restlet implements Closeable {
	
	private static final ArrayList<MediaType> SUPPORTEDMEDIATYPES = new ArrayList<MediaType>(5);
	
	static {
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_XML);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_JSON);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_XML);
	}

	private final ILoggedPlugin activator;
	private final Hashtable<Integer, RestProgressMonitor> cache;
	private final long cacheduration;
	private final Timer cleaner;
	private final boolean keepTaskTrace;
	private final ThreadGroup threads;
	private volatile AtomicInteger lastId;

	/**
	 * Create a new Restlet Resource to manage long operations.
	 * 
	 * @param context the Restlet Context, required.
	 */
	public ProgressMonitoredRestlet(final Context context) {
		this(null, context, 3600000l, true);
	}

	/**
	 * Create a new Restlet Resource to manage long operations.
	 * 
	 * @param activator may be null.
	 * @param context the Restlet Context, required.
	 */
	public ProgressMonitoredRestlet(final ILoggedPlugin activator, final Context context) {
		this(activator, context, 3600000l, true);
	}
	
	/**
	 * Create a new Restlet Resource to manage long operations.
	 * 
	 * @param activator may be null.
	 * @param context the Restlet Context, required.
	 * @param cacheduration the Cache of terminated operation limit, zero for unlimited.
	 * @param keepTaskTrace if true the progressMonitor will keep an history of all the task it goes through.
	 */
	public ProgressMonitoredRestlet(final ILoggedPlugin activator, final Context context, final long cacheduration, final boolean keepTaskTrace) {
		super(context);
		threads = new ThreadGroup("Rest Operation manager");
		threads.setDaemon(false);
		lastId = new AtomicInteger(0);
		this.activator = activator;
		this.keepTaskTrace = keepTaskTrace;
		cache = new Hashtable<Integer, RestProgressMonitor>();
		this.cacheduration = cacheduration;
		cleaner = new Timer("Rest Operation progress cache cleaner", true);
		if (cacheduration > 1000) {
			cleaner.schedule(new TimerTask() {
				@Override
				public void run() {
					ProgressMonitoredRestlet.this.cleanCache();
				}
			}, cacheduration, cacheduration);
		}
	}
	
	/**
	 * Get a new (unique) integer value at each call.
	 * 
	 * @return a positive integer.
	 */
	protected final int getNewId() {
		int id = lastId.incrementAndGet();
		if (id < 0) {
			synchronized (this) {
				if (id < 0) {
					lastId = new AtomicInteger(0);
					id = lastId.incrementAndGet();
				}
			}
		}
		return id;
	}
	
	private final synchronized void cleanCache() {
		long cd = getCacheduration();
		if (cd > 0) {
			long t = System.currentTimeMillis() - cd;
			Iterator<RestProgressMonitor> it = cache.values().iterator();
			while (it.hasNext()) {
				RestProgressMonitor monitor = it.next();
				if ((monitor.isCanceled() || monitor.isDone()) && monitor.isStarted() && (monitor.getStartTime() < t)) {
					it.remove();
				}
			}
		}
	}

	@Override
	public void close() throws IOException {
		cleaner.cancel();
		threads.interrupt();
        if (isStarted()) {
            try {
				stop();
			} catch (Exception e) {
				logDebug(e);
			}
        }
        synchronized (this) {
			for (RestProgressMonitor monitor: cache.values()) {
				if (!monitor.isCanceled() && !monitor.isDone()) {
					monitor.setCanceled(true);
				}
			}
		}
		cache.clear();
	}

	/**
	 * The Activator associated to this resource.
	 * 
	 * @return
	 */
	protected final ILoggedPlugin getActivator() {
		return activator;
	}

	/**
	 * log an error message.
	 * 
	 * @param e
	 */
	protected final void logError(Exception e) {
		if (activator != null) {
			activator.error(e.getLocalizedMessage(), e);
		}
	}

	/**
	 * log an warning message.
	 * 
	 * @param message the warning message.
	 * @param e may be null.
	 */
	protected final void logWarning(String message, Exception e) {
		if (activator != null) {
			activator.error(message, e);
		}
	}

	/**
	 * log a debug message.
	 * 
	 * @param e
	 */
	protected final void logDebug(Exception e) {
		if (activator != null) {
			activator.debug(e);
		}
	}

	/**
	 * log e debug message.
	 * 
	 * @param message
	 */
	protected final void logDebug(String message) {
		if (activator != null) {
			activator.debug(message);
		}
	}
	
	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		final MediaType media = request.getClientInfo().getPreferredMediaType(SUPPORTEDMEDIATYPES);
		final IConnectionUserBean user = (IConnectionUserBean) request.getAttributes().get(IConnectionUserBean.CONNECTED_USER);
		if (user == null) {
			response.setStatus(Status.SERVER_ERROR_INTERNAL, "For securirty reasons, an Authenticated user is required to permform longterm operation !");
		}
		Form form;
		Representation entity = request.getEntity();
		if ((entity != null) && entity.isAvailable() && !(entity instanceof EmptyRepresentation)) {
			form = new Form(entity);
			if ("none".equals(form.getFirstValue("_empty_"))) { //$NON-NLS-1$ //$NON-NLS-2$
				form = request.getResourceRef().getQueryAsForm();
			}
		} else {
			form = request.getResourceRef().getQueryAsForm();
		}
		String rempart = request.getResourceRef().getRemainingPart(true, false);
		if ((rempart == null) || rempart.isEmpty() || "/".equals(rempart)) { //$NON-NLS-1$
			response.getAllowedMethods().add(Method.POST);
			response.getAllowedMethods().add(Method.GET);
			if (Method.POST.equals(request.getMethod())) {
				try {
					RestProgressMonitor monitor = new RestProgressMonitor(user.getId(), isKeepTaskTrace());
					int result = startOperation(user, form, monitor);
					if (result > 0) {
						monitor.setId(result);
						cacheMonitor(monitor);
						response.setEntity(getMonitorRepresentation(media, monitor));
						response.setStatus(Status.SUCCESS_CREATED);
					} else {
						response.setStatus(Status.SERVER_ERROR_SERVICE_UNAVAILABLE);
					}
				} catch (Exception e) {
					logError(e);
					response.setStatus(Status.SERVER_ERROR_INTERNAL, e);
				}
			} else if (Method.GET.equals(request.getMethod())) {
				if (MediaType.APPLICATION_W3C_SCHEMA.equals(media)) {
					String xsd = getXSDText();
					if (xsd == null) {
						response.setStatus(Status.SERVER_ERROR_INSUFFICIENT_STORAGE);
					} else {
						response.setEntity(new StringRepresentation(xsd, media, Language.ENGLISH_US, CharacterSet.UTF_8));
						response.setStatus(Status.SUCCESS_OK);
					}
				} else {
					response.setEntity(getMonitorsRepresentation(media, user, getMonitors()));
					response.setStatus(Status.SUCCESS_OK);
				}
			} else {
				response.setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
			}
		} else {
			if (rempart.charAt(0) == '/') {
				rempart = rempart.substring(1);
			}
			String[] ids = rempart.split("\\+"); //$NON-NLS-1$
			response.getAllowedMethods().add(Method.GET);
			response.getAllowedMethods().add(Method.HEAD);
			response.getAllowedMethods().add(Method.DELETE);
			if (Method.HEAD.equals(request.getMethod())) {
				boolean found = false; 
				for (String id: ids) {
					int i = getID(id);
					if (i > 0) {
						RestProgressMonitor monitor = getMonitor(i);
						if ((monitor != null) && !monitor.isCanceled() && !monitor.isDone()) {
							found = true;
							break;
						}
					}
				}
				if (found) {
					response.setStatus(Status.SUCCESS_NO_CONTENT);
				} else {
					response.setStatus(Status.CLIENT_ERROR_NOT_FOUND);
				}
			} else if (Method.GET.equals(request.getMethod())) {
				if (MediaType.APPLICATION_W3C_SCHEMA.equals(media)) {
					String xsd = getXSDText();
					if (xsd == null) {
						response.setStatus(Status.SERVER_ERROR_INSUFFICIENT_STORAGE);
					} else {
						response.setEntity(new StringRepresentation(xsd, media, Language.ENGLISH_US, CharacterSet.UTF_8));
						response.setStatus(Status.SUCCESS_OK);
					}
				} else {
					ArrayList<IProgressMonitor> result = new ArrayList<IProgressMonitor>();
					for (String id: ids) {
						int i = getID(id);
						if (i > 0) {
							RestProgressMonitor monitor = getMonitor(i);
							if (monitor != null) {
								result.add(monitor);
							}
						}
					}
					if (result.isEmpty()) {
						response.setStatus(Status.SUCCESS_NO_CONTENT);
					} else if (result.size() == 1) {
						final RestProgressMonitor monitor = (RestProgressMonitor) result.get(0); 
						if (onRead(monitor.getID(), monitor.getUserId(), user)) {
							response.setEntity(getMonitorRepresentation(media, monitor));
							response.setStatus(Status.SUCCESS_OK);
						} else {
							response.setStatus(Status.CLIENT_ERROR_FORBIDDEN);
						}
					} else {
						response.setEntity(getMonitorsRepresentation(media, user, result));
						response.setStatus(Status.SUCCESS_OK);
					}
				}
			} else if (Method.DELETE.equals(request.getMethod())) {
				ArrayList<IProgressMonitor> result = new ArrayList<IProgressMonitor>();
				for (String id: ids) {
					int i = getID(id);
					if (i > 0) {
						RestProgressMonitor monitor = getMonitor(i);
						try {
							if ((monitor != null) && onDelete(i, monitor.getUserId(), user)) {
								monitor.setCanceled(true);
								result.add(monitor);
							}
						} catch (Exception e) {
							logError(e);
						}
					}
				}
				if (result.isEmpty()) {
					response.setStatus(Status.SUCCESS_NO_CONTENT);
				} else if (result.size() == 1) {
					RestProgressMonitor monitor = (RestProgressMonitor) result.get(0);
					if (onRead(monitor.getID(), monitor.getUserId(), user)) {
						response.setEntity(getMonitorRepresentation(media, monitor));
						response.setStatus(Status.SUCCESS_OK);
					} else {
						response.setStatus(Status.SUCCESS_NO_CONTENT);
					}
				} else {
					response.setEntity(getMonitorsRepresentation(media, user, result));
					response.setStatus(Status.SUCCESS_OK);
				}
			} else {
				response.setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
			}
		}
	}

	private String getXSDText() {
		if (activator instanceof AbstractActivator) {
			Bundle bundle = ((AbstractActivator) activator).findBundle("com.arcadsoftware.rest"); //$NON-NLS-1$
			if (bundle != null) {
				File file = ((AbstractActivator) activator).getBundleFile(bundle, "schemas/operation.xsd"); //$NON-NLS-1$
				if (file != null) {
					try {
						String result = new String(Files.readAllBytes(file.toPath()), StandardCharsets.UTF_8);
						return String.format(result, getOperationXMLTag(), getOperationListXMLTag());
					} catch (IOException | IllegalFormatException e) {}
				}
			}
		}
		return null;
	}

	/**
	 * Override this method to check the user right on this operation.
	 * 
	 * <p>
	 * On default implementation only the user who started the operation can cancel it.
	 * 
	 * @param id the Operation identifier.
	 * @param uid the original user identifier which started this operation.
	 * @param user the current user.
	 * @return true if the user can cancel this task.
	 */
	protected boolean onDelete(int id, int uid, IConnectionUserBean user) {
		return (user == null) || (uid == user.getId());
	}

	/**
	 * Override this method to check the user right on this operation.
	 * 
	 * <p>
	 * On default implementation only the user who started the operation can read it.
	 * 
	 * @param id the Operation identifier.
	 * @param uid the original user identifier which started this operation.
	 * @param user the current user.
	 * @return true if the user can cancel this task.
	 */
	protected boolean onRead(int id, int uid, IConnectionUserBean user) {
		return (user == null) || (uid == user.getId());
	}

	/**
	 * Get the monitor associated to the given operation identifier.
	 * 
	 * @param id
	 * @return may return null if this monitor does not exist on has bean cleaned.
	 */
	protected IProgressMonitor getProgressMonitor(int id) {
		return getMonitor(id);
	}

	private synchronized RestProgressMonitor getMonitor(int id) {
		return cache.get(id);
	}

	/**
	 * Get the JSON representation of the requested monitor.
	 * 
	 * @param id The monitor id has returned by startOperation method.
	 * @param ascendingTasks Define if the Task name list is returned in historical order, oldest first (true) or with the latest on top (false). 
	 * @return null if this monitor is not present in the cache.
	 */
	public JSONObject getMonitor(int id, boolean ascendingTasks) {
		final RestProgressMonitor monitor = getMonitor(id);
		if (monitor == null) {
			return null;
		}
		return monitor.getJSON(ascendingTasks);
	}
	
	/**
	 * Get the list of all the monitor currently in cache.
	 * 
	 * @return
	 */
	protected synchronized List<IProgressMonitor> getMonitors() {
		ArrayList<IProgressMonitor> result = new ArrayList<IProgressMonitor>(cache.size());
		result.addAll(cache.values());
		return result;
	}

	private int getID(String id) {
		try {
			return Integer.parseInt(id.trim());
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	/**
	 * Get the XML tag name used to represent the progressMonitor object.
	 * 
	 * @return default is "operation". Must be a valid XML tag name.
	 */
	protected String getOperationXMLTag() {
		return "operation"; //$NON-NLS-1$
	}
	
	/**
	 * Get the XML tag name used to represent le list of progressMonitor.
	 * 
	 * @return default is "list". Must be a valid XML tag name.
	 */
	protected String getOperationListXMLTag() {
		return "list"; //$NON-NLS-1$
	}
	
	private Representation getMonitorRepresentation(MediaType media, RestProgressMonitor monitor) {
		if (MediaType.APPLICATION_JSON.equals(media)) {
			return new JSONRepresentation(monitor.getJSON(isSortTasksAscending()).toString());
		}
		return new XMLRepresentation(monitor.getXML(getOperationXMLTag(), isSortTasksAscending()));
	}

	private Representation getMonitorsRepresentation(MediaType media, IConnectionUserBean user, List<IProgressMonitor> monitors) {
		if (MediaType.APPLICATION_JSON.equals(media)) {
			JSONArray result = new JSONArray(monitors.size());
			for (IProgressMonitor m: monitors) {
				RestProgressMonitor monitor = (RestProgressMonitor) m;
				if (onRead(monitor.getID(), monitor.getUserId(), user)) {
					result.put(monitor.getJSON(isSortTasksAscending()));
				}
			}
			return new JSONRepresentation(result.toString());
		}
		StringBuilder result = new StringBuilder('<' + getOperationListXMLTag());
		result.append(" size=\""); //$NON-NLS-1$
		result.append(monitors.size());
		result.append("\">"); //$NON-NLS-1$
		for (IProgressMonitor monitor: monitors) {
			result.append(((RestProgressMonitor) monitor).getXML(getOperationXMLTag(), isSortTasksAscending()));
		}
		result.append("</"); //$NON-NLS-1$
		result.append(getOperationListXMLTag());
		result.append('>');
		return new XMLRepresentation(result.toString());
	}

	private synchronized void cacheMonitor(RestProgressMonitor monitor) {
		if (monitor != null) {
			// Check if there is not another monitor with same ID...
			if (cache.containsKey(monitor.getID())) {
				logWarning("The ProgressMonitor cache of " + this.getClass().getCanonicalName() + " already contains a monitor identified by: " + monitor.getID(), null);
			}
			cache.put(monitor.getID(), monitor);
		}
	}

	/**
	 * This operation must return a unique identifier, and launch the actual operation in another thread.
	 * 
	 * @param currentuser
	 * @param parameters
	 * @param monitor
	 * @return s positive identification number, which must be unique.
	 */
	protected abstract int startOperation(final IConnectionUserBean currentuser, final Form parameters, final IProgressMonitor monitor);

	/**
	 * Run in another Thread with a small delay (enough time to answer to the HTTP client...).
	 * 
	 * @param runnable the long operation to perform.
	 * @param monitor the original monitor object.
	 */
	protected final void delayedRun(final Runnable runnable, final IProgressMonitor monitor) {
		if (runnable != null) {
			if (usedMultiThreadDelayedRun()) {
				new Thread(threads, new Runnable() {
					@Override
					public void run() {
						try {
							Thread.sleep(50);
							runnable.run();
							if ((monitor != null) && !monitor.isCanceled()) {
								monitor.done();
							}
						} catch (Exception e) {
							logError(e);
						} finally {
							if (monitor instanceof RestProgressMonitor) {
								((RestProgressMonitor) monitor).terminate();
							}
						}
					}
				}).start();
			} else {
				cleaner.schedule(new TimerTask() {
					@Override
					public void run() {
						try {
							runnable.run();
							if ((monitor != null) && !monitor.isCanceled()) {
								monitor.done();
							}
						} catch (Exception e) {
							logError(e);
						} finally {
							if (monitor instanceof RestProgressMonitor) {
								((RestProgressMonitor) monitor).terminate();
							}
						}
					}
				}, 50);
			}
		}
	}
	/**
	 * The the actual cache duration (in milliseconds).
	 * 
	 * <p>
	 * Any terminated ProgressMonitor older than the given delay is removed from the cache).
	 * 
	 * @return zero if the cache is not cleaned.
	 */
	public long getCacheduration() {
		return cacheduration;
	}

	/**
	 * Return true is all task name must be conserver, if false only the current task name is conserved.
	 */
	public boolean isKeepTaskTrace() {
		return keepTaskTrace;
	}
	
	/**
	 * Override this method to change the order of the tasts list.
	 * 
	 * <p>
	 * If true the older task will be sorted first, if false the latest task will be on top of the list (default order).
	 * 
	 * @return
	 */
	protected boolean isSortTasksAscending() {
		return false;
	}
	
	/**
	 * Override this method to allow the delayedRun method to launch the Runnable in a single Thread or in multiple threads.
	 * 
	 * <p>
	 * The default value is true, the delayedRun will use multiple thread to run the operations.
	 * 
	 * @return
	 * @see #delayedRun(Runnable, IProgressMonitor)
	 */
	protected boolean usedMultiThreadDelayedRun() {
		return true;
	}
}
