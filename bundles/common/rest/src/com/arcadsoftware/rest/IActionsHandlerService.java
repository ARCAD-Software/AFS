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
package com.arcadsoftware.rest;

import org.restlet.data.Language;

import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Action Handlers are called each time the "action" service is called.
 * 
 * <p>
 * Due to state-less nature of the service a same action may be run many times with the same parameters.  
 * 
 * <p>
 * TODO Reimplement this service to allow the Metadata services to register supported actions with corresponding entities (use services properties to perform a maping between types and actions).
 * 
 * @author ARCAD Software
 */
public interface IActionsHandlerService {

	/**
	 * OSGi service reference.
	 */
	public static final String clazz = IActionsHandlerService.class.getName();
	
	/**
	 * Return true if this handler will process the given action.
	 * 
	 * <p>
	 * If true then the method run will be called.
	 * 
	 * @param actionCode The Action code given by the user.
	 * @param referenceNumber
	 * @param user
	 * @return
	 */
	public boolean handle(String actionCode, String referenceNumber, IConnectionUserBean user);
	
	/**
	 * Execute the given action and collect a response from the server.
	 * 
	 * <p>
	 * The message will be sent to the user.
	 *  
	 * @param actionCode
	 * @param referenceNumber
	 * @param user
	 * @param language
	 * @return
	 */
	public String run(String actionCode, String referenceNumber, IConnectionUserBean user, Language language);
	
}
