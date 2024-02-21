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
package com.arcadsoftware.osgi.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.TimerTask;
import java.util.Map.Entry;
import java.util.Timer;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;

import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.osgi.IStartTask;

/**
 * Internal class that manage the IStartTask OSGi services. This class register herself as a listener of most OSGi 
 * modules (bundles, services, configurationAdmin end events). This may cause a process overload on some platforms. 
 * 
 * <p>
 * Details <a href="https://wiki.arcadsoftware.com/RD:AFS_-_Start_Task_API">ARCAD-Software Wiki</a>.
 * 
 * @author ARCAD Software
 * @see IStartTask
 */
public class StartTaskManager implements EventHandler, BundleListener, ServiceListener, CommandProvider {

	// Const internes ajoutées ici pour éviter des dépendances inutiles.
	private static final String PREFIX_TOPIC_ENTITY = "com/arcadsoftware/metadata/entity/"; //$NON-NLS-1$
	private static final String TOPIC_ENTITY_CREATED = PREFIX_TOPIC_ENTITY + "create"; //$NON-NLS-1$
	private static final String TOPIC_ENTITY_ATTRIBUTE_CREATED = PREFIX_TOPIC_ENTITY + "attribute/create"; //$NON-NLS-1$
	private static final String TOPIC_MAPPER_CREATED = "com/arcadsoftware/metadata/mapper/create"; //$NON-NLS-1$
	private static final String TOPIC_DS_ADD = "com/arcadsoftware/database/add"; //$NON-NLS-1$
	private static final String DATABASEUPDATED_TOPIC = "com/arcadsoftware/database/updated"; //$NON-NLS-1$

	private static volatile int noidcounter = 1;
	
	private static final String[] TOPIC_ALLEVENTS = new String[] {"com/arcadsoftware/*"}; //$NON-NLS-1$	
	
	private class Requirements {
		
		private final HashMap<String, HashSet<String>> elements = new HashMap<String, HashSet<String>>();
		
		public boolean isEmpty() {
			return elements.isEmpty();
		}
		
		public void add(String req, String... ids) {
			HashSet<String> e = elements.get(req);
			if (e == null) {
				e = new HashSet<String>();
				elements.put(req, e);
			}
			for(String id: ids) {
				if ((id != null) && (id.length() > 0)) {
					e.add(new String(id));
				}
			}
		}
		
		public boolean remove(String req, String... ids) {
			HashSet<String> e = elements.get(req);
			if (e != null) {
				if (ids.length == 0) {
					if (e.isEmpty()) {
						elements.remove(req);
						return elements.isEmpty();
					}
				} else {
					for(String id: ids) { //$NON-NLS-1$
						if ((id != null) && (id.length() > 0)) {
							if (e.remove(id) && e.isEmpty()) {
								elements.remove(req);
								return elements.isEmpty();
							}
						}
					}
				}
			}
			return false;
		}
		
		public boolean remove(String req, HashSet<String> rels) {
			HashSet<String> e = elements.get(req);
			if (e != null) {
				if ((rels == null) || rels.isEmpty()) {
					if (e.isEmpty()) {
						elements.remove(req);
						return elements.isEmpty();
					}
				} else {
					for(String re: rels) { //$NON-NLS-1$
						if (e.remove(re) && e.isEmpty()) {
							elements.remove(req);
							return elements.isEmpty();
						}
					}
				}
			}
			return false;
		}
		
		public boolean remove(Requirements r) {
			for(Entry<String, HashSet<String>> el: r.elements.entrySet()) {
				if (remove(el.getKey(), el.getValue())) {
					return true; // plus rien à enlever.
				}
			}
			return false;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			for(Entry<String, HashSet<String>> e: elements.entrySet()) {
				sb.append(e.getKey());
				sb.append(": "); //$NON-NLS-1$
				sb.append(e.getValue().toString());
				sb.append('\n');
			}
			return sb.toString();
		}
	}
	
	private class TaskRegistered {
		
		private final String id;
		private final IStartTask task;
		private final Requirements requirements;
		private final ILoggedPlugin activator;

		public TaskRegistered(ServiceReference<?> reference, IStartTask task) {
			this.task = task;
			Object o = reference.getProperty("activator"); //$NON-NLS-1$
			if (o instanceof ILoggedPlugin) {
				activator = (ILoggedPlugin) o;
			} else {
				activator = null;
			}
			Object tid = reference.getProperty(IStartTask.TASK_ID);
			if (tid != null) {
				id = tid.toString();
			} else if (activator instanceof AbstractActivator) {
				id = ((AbstractActivator) activator).getContext().getBundle().getSymbolicName();
			} else {
				id = "NOID#" + (noidcounter++); //$NON-NLS-1$
			}
			requirements = new Requirements();
			for(String key : reference.getPropertyKeys()) {
				if (key.startsWith("required_")) { //$NON-NLS-1$
					requirements.add(key, reference.getProperty(key).toString().split(" ")); //$NON-NLS-1$
				}
			}
		}
		
		public boolean execute() {
			try {
				if (activator == null) {
					return task.run(StartTaskManager.this.activator);
				}
				return task.run(activator);
			} catch (Throwable e) {
				if (activator == null) {
					Activator.getInstance().error("Error during IStartTask execution: " + id, e);
				} else {
					activator.error("Error during IStartTask execution: " + id, e);
				}
				return false;
			}
		}

		@Override
		public boolean equals(Object obj) {
			return super.equals(obj) || ((obj instanceof IStartTask) && task.equals((IStartTask) obj));
		}

		public String getId() {
			return id;
		}
	}
	
	private final Requirements history = new Requirements();
	private final ArrayList<TaskRegistered> tasks = new ArrayList<TaskRegistered>();
	private final Timer timer;
	private final Activator activator;
	
	public StartTaskManager(final Activator activator) {
		super();
		this.activator = activator;
		// Enregistre tout les bundles déjà démarés...
		for(Bundle b: activator.getContext().getBundles()) {
			if (b.getState() == Bundle.ACTIVE) {
				history.add(IStartTask.REQUIRE_BUNDLES, b.getSymbolicName());
			}
		}
		activator.getContext().addBundleListener(this);
		// Traite tout les services déjà enregistrés...
		try {
			for(ServiceReference<?> sr: activator.getContext().getAllServiceReferences(null, null)) {
				history.add(IStartTask.REQUIRE_SERVICES, (String[]) sr.getProperty(Constants.OBJECTCLASS));
			}
		} catch (InvalidSyntaxException e) {
			activator.debug(e);
		}
		activator.getContext().addServiceListener(this);
		// FIXME impossible de traiter tous les évènement déjà déclenchés !!!
		activator.registerService(EventHandler.class.getName(), this, EventConstants.EVENT_TOPIC, TOPIC_ALLEVENTS);
		// prépare le déclenchement du "late run" !
		timer = new Timer("Late run of StartTask clock"); //$NON-NLS-1$
		timer.schedule(new TimerTask() {
			
			private int count;
			
			@Override
			public void run() {
				count++;
				if (count == 1) {
					// Traitement des configuration qu'on aurait raté !!!
					ConfigurationAdmin ca = (ConfigurationAdmin) activator.getService(ConfigurationAdmin.class.getName());
					if (ca != null) {
						try {
							Configuration[] confs = ca.listConfigurations(null);
							if (confs != null) {
								for(Configuration c: confs) {
									StartTaskManager.this.handle(IStartTask.REQUIRE_CONFIGURATIONS, c.getPid());
								}
							}
						} catch (Exception e) {}
					}
					StartTaskManager.this.handle(IStartTask.REQUIRE_LATERUN);
				} else {
					StartTaskManager.this.handle(IStartTask.REQUIRE_VERYLATERUN);
					timer.cancel();
				}
			}
		}, 240000, 120000);
	}

	public void close() {
		timer.cancel();
		activator.getContext().removeServiceListener(this);
		activator.getContext().removeBundleListener(this);
		logRemaningTasks();
	}

	private synchronized void logRemaningTasks() {
		StringBuilder sb = new StringBuilder();
		for(TaskRegistered t: tasks) {
			sb.append("    "); //$NON-NLS-1$
			sb.append(t.getId());
			sb.append(": "); //$NON-NLS-1$
			sb.append(t.toString());
			sb.append("\n"); //$NON-NLS-1$
		}
		tasks.clear();
		if (sb.length() > 0) {
			activator.warn("Some IStartTask where still waiting to launch:\n" + sb.toString()); //$NON-NLS-1$
		} else {
			activator.debug("No IStartTask where waiting to run."); //$NON-NLS-1$
		}
	}

	public void serviceChanged(ServiceEvent event) {
		try {
			final String[] services = (String[]) event.getServiceReference().getProperty(Constants.OBJECTCLASS);
			boolean isStartTask = false;
			if (services != null) {
				for (String s: services) {
					if ((s != null) && s.equals(IStartTask.clazz)) {
						isStartTask = true;
						break;
					}
				}
				if (isStartTask) {
					 // FIXME the class may be not accessible here if the event was "Unregistering" and the original bundle is stoping !
					Object service = activator.getContext().getService(event.getServiceReference());
					if (service instanceof IStartTask) {
						switch (event.getType()) {
						case ServiceEvent.REGISTERED:
							add(event.getServiceReference(), (IStartTask) service);
							break;
						case ServiceEvent.UNREGISTERING:
							remove((IStartTask) service);
							break;
						case ServiceEvent.MODIFIED:
							remove((IStartTask) service);
							add(event.getServiceReference(), (IStartTask) service);
							break;
						}
					}
				}
				if (ServiceEvent.REGISTERED == event.getType()) {
					// Exécution non synchronisée (pour ne pas bloquer le démarrage de bundles...)
					if (services.length > 0) {
						new Thread(new Runnable() {
							public void run() {
								handle(IStartTask.REQUIRE_SERVICES, services);
							}
						});
					}
				}
			}
		} catch (Exception e) {
			activator.warn(e);
		}
	}

	public void bundleChanged(BundleEvent event) {
		try {
			if (event.getType() == BundleEvent.STARTED) {
				// Execution synchronisée...
				handle(IStartTask.REQUIRE_BUNDLES, event.getBundle().getSymbolicName());
			}
		} catch (Exception e) {
			activator.warn(e);
		}
	}
	
	public void handleEvent(Event event) {
		try {
			if (TOPIC_ENTITY_CREATED.equals(event.getTopic())) {
				handle(IStartTask.REQUIRE_ENTITIES, (String) event.getProperty("type")); //$NON-NLS-1$
			} else if (TOPIC_ENTITY_ATTRIBUTE_CREATED.equals(event.getTopic())) {
				// Gérer les ajout d'attributs et de lien (patcher le registre d'entité ???)
				String[] ids = splitString(event.getProperty("attributes"));
				String prefix = ((String) event.getProperty("type")) + '.';
				for(int i = 0; i < ids.length; i++) {
					ids[i] = prefix + ids[i];
				}
				handle(IStartTask.REQUIRE_ENTITIES, ids); //$NON-NLS-1$
			} else if (TOPIC_MAPPER_CREATED.equals(event.getTopic())) {
				handle(IStartTask.REQUIRE_MAPPERS, splitString(event.getProperty("domains"))); //$NON-NLS-1$
			} else if (TOPIC_DS_ADD.equals(event.getTopic())) {
				handle(IStartTask.REQUIRE_DATASOURCES, (String) event.getProperty("datasource.id")); //$NON-NLS-1$
			} else if (BundleManagedService.CONFIGURATIONUPDATED_TOPIC.equals(event.getTopic())) {
				handle(IStartTask.REQUIRE_CONFIGURATIONS, (String) event.getProperty("pid")); //$NON-NLS-1$
			} else if (DATABASEUPDATED_TOPIC.equals(event.getTopic())) {
				handle(IStartTask.REQUIRE_DBUPDATES, (String) event.getProperty("dsid")); //$NON-NLS-1$
			}
			handle(IStartTask.REQUIRE_EVENTS, event.getTopic());
		} catch (Exception e) {
			activator.warn(e);
		}
	}
	
	private String[] splitString(Object p) {
		if (p == null) {
			return new String[0];
		}
		String s = p.toString();
		if ((s == null) || (s.length() == 0)) {
			return new String[0];
		}
		s = s.trim();
		if (s.length() == 0) {
			return new String[0];
		}
		return s.split(" "); //$NON-NLS-1$
	}

	public synchronized void handle(String req, String... id) {
		ArrayList<TaskRegistered> runtasks = new ArrayList<TaskRegistered>();
		// Retirer toutes les contraintes de la liste des tâches.
		Iterator<TaskRegistered> it = tasks.iterator();
		while (it.hasNext()) {
			TaskRegistered t = it.next();
			if (t.requirements.remove(req, id)) {
				it.remove();
				runtasks.add(t);
			}
		}
		// Exécuter toutes les tâches qui n'ont plus de contraintes.
		runs(runtasks);
		// Ajouter la contrainte à l'historique.
		history.add(req, id);
	}

	private synchronized void add(ServiceReference<?> reference, IStartTask task) {
		TaskRegistered tr = new TaskRegistered(reference, task);
		// Utiliser l'historique pour mettre à jour la tâche.
		tr.requirements.remove(history);
		// Exécuter la tâche si complète.
		if (tr.requirements.isEmpty()) {
			runs(tr);
		} else {
			// Sinon l'ajouter à la liste des tâches à traiter.
			tasks.add(tr);
		}
	}

	private void runs(TaskRegistered task) {
		if (task.execute()) {
			tasks.remove(task);
			handle(IStartTask.REQUIRE_TASKS, task.getId());
		} else {
			tasks.remove(task);
		}
	}

	private void runs(ArrayList<TaskRegistered> tasks) {
		if (tasks.size() > 0) {
			HashSet<String> ids = new HashSet<String>();
			for(TaskRegistered task: tasks) {
				if (task.execute()) {
					ids.add(task.getId());
				}
				this.tasks.remove(task);
			}
			if (ids.size() > 0) {
				handle(IStartTask.REQUIRE_TASKS, ids.toArray(new String[0]));
			}
		}
	}
	
	private synchronized void remove(Object task) {
		tasks.remove(task);
	}

	public String getHelp() {
		return "\tstartTasks [[cancel] <task id>] - without parameters list all starting tack not ececuted. Only with a task id list the constraints currently not satified for this task, with the keyword \"cancel\" remove the task from the list and cancel its execution.";
	}
	
	public void _startTasks(CommandInterpreter ci) throws Exception {
		String s = ci.nextArgument();
		boolean cancel = "cancel".equalsIgnoreCase(s);
		boolean force = "force".equalsIgnoreCase(s);
		if (cancel) {
			s = ci.nextArgument();
		}
		if (s == null) {
			ci.println("Waiting tasks:"); //$NON-NLS-1$
			if (tasks.isEmpty()) {
				ci.println("none.");
			} else {
				for(TaskRegistered t: tasks) {
					ci.println("    " + t.getId() + ": " + t.toString()); //$NON-NLS-1$ //$NON-NLS-2$ 
				}
			}
			return;
		}
		ci.print("Task \"" + s); //$NON-NLS-1$
		for(TaskRegistered t: tasks) {
			if (s.equalsIgnoreCase(t.getId())) {
				if (cancel) {
					remove(t);
					ci.println("\" cancelled."); //$NON-NLS-1$
				} else if (force) {
					ci.println("\" executing..."); //$NON-NLS-1$
					runs(t);
				} else {
					ci.println("\" waiting:"); //$NON-NLS-1$
					ci.println(t.requirements);
				}
				return;
			}
		}
		ci.println("\" not found."); //$NON-NLS-1$
	}
}
