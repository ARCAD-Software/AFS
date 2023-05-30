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
package com.arcadsoftware.osgi;

import java.util.Hashtable;

/**
 * Allow the implementation of a Start task.
 * 
 * A Start Task will be executed when all the required constraints will be met.
 * This could be now, or never, but a task will be executed only once when 
 * there is no more constraint left.
 * 
 * @author ARCAD Software
 * @see IStartTask
 * @see AbstractActivator#registerStartTask(AbstractStartTask)
 */
public abstract class AbstractStartTask implements IStartTask {
	
	private final Hashtable<String, Object> constraints;
	
	/**
	 * Create an unconfigured Task.
	 * 
	 * <p>
	 * Implementations must define a set of constraints overriding the {@link #getConstraints()} method.
	 */
	public AbstractStartTask() {
		super();
		constraints = new Hashtable<String, Object>();
	}

	/**
	 * Create an Task with an 
	 * @param activator
	 */
	public AbstractStartTask(AbstractActivator activator) {
		this();
		constraints.put("activator", activator); //$NON-NLS-1$
		constraints.put(TASK_ID, activator.getContext().getBundle().getSymbolicName());
	}

	/**
	 * Create a Task.
	 * 
	 * <p>
	 * Implementations must define a set of constraints using one, or many, of the
	 * <code>addConstraint*</code> methods.
	 * 
	 * @param id optional Id that may be used by other tasks to 
	 */
	public AbstractStartTask(final String id) {
		this();
		if (id != null) {
			constraints.put(TASK_ID, id);
		}
	}

	public AbstractStartTask(AbstractActivator activator, final String id) {
		this();
		constraints.put("activator", activator); //$NON-NLS-1$
		if (id != null) {
			constraints.put(TASK_ID, id);
		} else {
			constraints.put(TASK_ID, activator.getContext().getBundle().getSymbolicName());
		}
	}

	private void addConstraint(String req, String... values) {
		StringBuilder sb = new StringBuilder();
		String v = (String) constraints.get(req);
		boolean first = true;
		if (v != null) {
			sb.append(v);
			first = false;
		}
		for(String value: values) {
			value = value.trim();
			if ((value != null) && (value.length() > 0)) {
				if (first) {
					first = false;
				} else {
					sb.append(' ');
				}
				sb.append(value);
			}
		}
		constraints.put(req, sb.toString());
	}
	
	/**
	 * Add one or many METADATA mappers domains required by this task.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param domains Mapper domain names.
	 * @see IStartTask#REQUIRE_MAPPERS
	 */
	public final void addConstraintMapper(String... domains) {
		addConstraint(REQUIRE_MAPPERS, domains);
	}

	/**
	 * Add one or many started Bundles required by this task.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param symbolicNames Bundles symbolic names.
	 * @see IStartTask#REQUIRE_BUNDLES
	 */
	public final void addConstraintBundle(String... symbolicNames) {
		addConstraint(REQUIRE_BUNDLES, symbolicNames);
	}

	/**
	 * Add one or many configurations stored by the ConfigurationAdmin OSGi service and required by this task.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param pid The configurations persistent Identifier (most of the time, equal to the bundle symbolic identifier). 
	 * @see IStartTask#REQUIRE_CONFIGURATIONS
	 */
	public final void addConstraintConfig(String... pid) {
		addConstraint(REQUIRE_CONFIGURATIONS, pid);
	}

	/**
	 * Add one or many Data Source required by this task.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param did The DataSource identifiers. 
	 * @see IStartTask#REQUIRE_DATASOURCES
	 */
	public final void addConstraintDataSource(String... did) {
		addConstraint(REQUIRE_DATASOURCES, did);
	}

	/**
	 * Add one or many Database update required by this task.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param did The DataSource identifiers. 
	 * @see IStartTask#REQUIRE_DBUPDATES
	 */
	public final void addConstraintDBUpdate(String... did) {
		addConstraint(REQUIRE_DBUPDATES, did);
	}

	/**
	 * Add one or many METADATA entities types required by this task. accept reference to Attributes or links in the formulation "type.code". 
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param entities The MetaDataEntity types. 
	 * @see IStartTask#REQUIRE_ENTITIES
	 */
	public final void addConstraintEntity(String... entities) {
		addConstraint(REQUIRE_ENTITIES, entities);
	}

	/**
	 * State that this task is a "late run" task. It will be run at least 4 minutes after the platform launch. 
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @see IStartTask#REQUIRE_LATERUN
	 */
	public final void addConstraintLateRun() {
		addConstraint(REQUIRE_LATERUN, ""); //$NON-NLS-1$
	}

	/**
	 * State that this task is a "very late run" task. It will be run at least 2 minutes after the "late run" tasks.
	 * In other words, at least 6 minutes after the platform launch. 
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @see IStartTask#REQUIRE_VERYLATERUN
	 */
	public final void addConstraintVeryLateRun() {
		addConstraint(REQUIRE_VERYLATERUN, ""); //$NON-NLS-1$
	}

	/**
	 * Add one or many OSGi services required by this task.
	 * 
	 * <p>
	 * Note that IStarTask themselves are not concerned by this constraint.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param class The class names associated to the required services. 
	 * @see IStartTask#REQUIRE_SERVICES
	 */
	public final void addConstraintService(String... clazz) {
		addConstraint(REQUIRE_SERVICES, clazz);
	}

	/**
	 * Add one or many OSGi event required by this task.
	 * 
	 * <p>
	 * Note that if an event if triggered before the Event OSGi service is started then it will be not
	 * trapped by the Task manager.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param topic The topics of the required events. 
	 * @see IStartTask#REQUIRE_EVENTS
	 */
	public final void addConstraintEvent(String... topic) {
		addConstraint(REQUIRE_EVENTS, topic);
	}

	/**
	 * Add one or many IStartTask required to be executed before this one.
	 * 
	 * <p>
	 * If multiple element are relative to this constraint, you can repeatedly call this method with
	 * each one, set more than one at a time, or set a single string with identifiers separated with spaces. 
	 * 
	 * @param tid The Task identifiers. 
	 * @see IStartTask#REQUIRE_TASKS
	 */
	public final void addConstaintTask(String... tid) {
		addConstraint(REQUIRE_TASKS, tid);
	}

	/**
	 * Get the constraints defined for this task.
	 * 
	 * <p>
	 * This property may be consulted during the execution of this task
	 * but any modification will be reported to the task management process,
	 * because the task IS already running.
	 * 
	 * <p>
	 * You can override this method to return a specific list of constraints.
	 * In this case do not forget to add a {@link IStartTask#TASK_ID} to the map
	 * to identify this task.
	 * 
	 * @return A non null Map of constraints.
	 */
	public Hashtable<String, Object> getConstraints() {
		return constraints;
	}

	/**
	 * Get the current task Id.
	 * @return
	 */
	public String getTaskId() {
		return (String) constraints.get(TASK_ID);
	}

	/**
	 * Define this task ID.
	 * 
	 * <p>
	 * You can not change the effective task id after this task is registered.
	 * But it could be usefull if you what to re-register it !
	 * @param id
	 */
	public void setTaskId(String id) {
		constraints.put(TASK_ID, id);
	}
	
	/**
	 * If this object constructor used and Activator reference, then this method
	 * return this activator object. Return null otherwise.
	 * @return
	 */
	public AbstractActivator getActivator() {
		return (AbstractActivator) constraints.get("activator"); //$NON-NLS-1$
	}
	
	/**
	 * Register a task.
	 * 
	 * <p>
	 * This throw a NullPointerException if this object constructor do not use an AbstractActivator. 
	 * @param task
	 */
	public void registerTask(AbstractStartTask task) {
		getActivator().registerStartTask(task);
	}
}
