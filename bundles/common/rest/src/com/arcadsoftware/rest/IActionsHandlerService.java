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
package com.arcadsoftware.rest;

import org.restlet.data.Language;

import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This interface define an OGSI service.
 * 
 * <p>
 * Action Handlers are called each time the "action" service is called from a REST web-services.
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
	 * If true then the method run could be called.
	 * 
	 * @param actionCode The identifier for the action requested by the user.
	 * @param referenceNumber A reference number associated with the request.
	 * @param user An object representing the user making the request.
	 * @return true → The handler will process the action.
	 *         false → The action is ignored, meaning another handler may process it.
	 */
	public boolean handle(String actionCode, String referenceNumber, IConnectionUserBean user);
	
	/**
	 * Execute the given action and collect a response from the server.
	 * 
	 * <p>
	 * The returned message will be sent to the user.
	 *  
	 * @param actionCode The identifier for the action requested by the user.
	 * @param referenceNumber A reference number associated with the request.
	 * @param user An object representing the user making the request.
	 * @param language The user requested language.
	 * @return A String representing the response message.
	 */
	public String run(String actionCode, String referenceNumber, IConnectionUserBean user, Language language);
	
}
