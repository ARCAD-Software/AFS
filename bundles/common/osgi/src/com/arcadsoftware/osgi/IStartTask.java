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

/**
 * This OSGi service allow a bundle to register unsynchronized task to run 
 * after startup and according to a set of constraint.
 * 
 * <p>
 * The correct implementation of this process require that the OSGi Event Service implementation is started before
 * the first task registration and after any event are thrown...
 * 
 * <p>
 * Details and tutorial at <a href="https://wiki.arcadsoftware.com/RD:AFS_-_Start_Task_API">ARCAD-Software Wiki</a>.
 * 
 * @author ARCAD Software
 * @see AbstractActivator#registerStartTask(AbstractStartTask)
 */
public interface IStartTask {

	public static final String clazz = IStartTask.class.getName();

	public static final String TASK_ID = "tid"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed entities will be loaded.
	 * 
	 * <p>Support any list of, space separated, types of entities, and type.code to test the existence
	 * of an attribute or a link into the given entity.  
	 */
	public static final String REQUIRE_ENTITIES = "required_entities"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed MataData mappers will be loaded.
	 */
	public static final String REQUIRE_MAPPERS = "required_mappers"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed bundles will be activated.
	 */
	public static final String REQUIRE_BUNDLES = "required_bundles"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed OSGi services will be registered.
	 * 
	 * <p>
	 * Note that if one of the service is "unregistered" before all of the other where
	 * registered, it may be not available at the moment of the execution of the task.
	 * 
	 * <p>
	 * the IStartTask OSGi services are not managed with this constraint, use the "REQUIRE_TASKS"
	 * constraint to lock until another task is executed.
	 */
	public static final String REQUIRE_SERVICES = "required_services"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed OSGi events will be triggered.
	 * 
	 * <p>
	 * Note that if an event is triggered before the OSGi Event service is started then
	 * no one can handle it and the corresponding task may be stay locked.
	 * 
	 */
	public static final String REQUIRE_EVENTS = "required_events"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed configurations will be loaded.
	 * 
	 * <p>
	 * Note that a configuration is not a mandatory part of a bundle. An empty configuration
	 * correspond to no configuration at all (especially on the first run). 
	 */
	public static final String REQUIRE_CONFIGURATIONS = "required_configurations"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed data sources will be declared.
	 */
	public static final String REQUIRE_DATASOURCES = "required_datasources"; //$NON-NLS-1$
	
	/**
	 * This task will not be executed until all the listed data sources related database are not up to date.
	 * Note that this constraint is not taken into account for database that are not managed by automatic
	 * update process, then task relative to this constraint will stay locked if the "auto" update
	 * scripts of the corresponding database are not accessible to the platform. 
	 * 
	 * <p>This constraint implicitly include the Data source require, no need to add both.
	 */
	public static final String REQUIRE_DBUPDATES = "required_dbupdates"; //$NON-NLS-1$
	
	/**
	 * This task will be executed only when the listed task will be completed (and have returned "true").
	 * 
	 * <p>
	 * Many task can have the same Id, in this case this constraint is not enough the determine which of
	 * the dependent task have been completed (this can be used to implement disjunction in constraints !).
	 * 
	 * <p>
	 * Note: the default task Id is equal to the bundle symbolic name.
	 */
	public static final String REQUIRE_TASKS = "required_tasks"; //$NON-NLS-1$
	
	/**
	 * The "late run" is executed only when there is no more "starting" bundles and not before 4 minutes after
	 * the start of the platform
	 */
	public static final String REQUIRE_LATERUN = "required_late"; //$NON-NLS-1$
	
	/**
	 * The "very late run" is executed 2 minutes after the late run. 
	 */
	public static final String REQUIRE_VERYLATERUN = "required_verylate"; //$NON-NLS-1$
	
	/**
	 * Implement the task to run. this method is called when all requirement properties
	 * are 
	 * 
	 * <p>
	 * This method is called into a specific thread and should thread-safe.
	 * (avoid to initialize singletons into this method...)
	 * 
	 * @param activator The bundle activator that have registered this task. 
	 * Or another class that facade the access to the log if this bundle activator does not implement the ILoggedPlugin interface. 
	 * @return true if this task is considered as completed. Then another dependent task may be started.
	 */
	public boolean run(ILoggedPlugin activator);
	
}
