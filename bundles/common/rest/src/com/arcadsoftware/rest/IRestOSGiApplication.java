package com.arcadsoftware.rest;

import org.restlet.routing.Router;

/**
 * This OSGi service allow to get a direct access to the Restlet Application and get some configuration parameters of the underlying HTTP Server.
 * 
 * @author ARCAD Software
 * @see OSGiApplication
 */
public interface IRestOSGiApplication {

	/**
	 * This router is the functional router used to attach the service of this application. This router is available
	 * only when the application is active.
	 * 
	 * @return the root router.
	 */
	public Router getRouter();

	/**
	 * This router is used when the application is switch into an inactive mode.
	 * @return
	 */
	public Router getInactiveRouter();

	/**
	 * Define if the REST application is active or not. if inactive no resource are accessible. Any request get a 503 Error state. 
	 * @param active
	 *            the new active state
	 */
	public void setActive(boolean active);
	
	/**
	 * Define if the REST application is active or not. if inactive no resource are accessible. Any request get a 503 Error state. 
	 * @return the active
	 */
	public boolean isActive();

}
