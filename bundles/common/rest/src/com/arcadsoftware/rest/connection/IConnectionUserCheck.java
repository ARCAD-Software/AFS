package com.arcadsoftware.rest.connection;

import org.restlet.resource.ResourceException;

/**
 * Define an OSGi service allowing to validate the connection of a user.
 * 
 * <p>
 * This service is called 
 * 
 * @author ARCAD Software
 */
public interface IConnectionUserCheck {

	/**
	 * This method is called each time a validated user access a resource.
	 * 
	 * <p>
	 * This method lock the current resource call and should ends quickly.
	 * 
	 * @param user
	 * @throws ResourceException
	 */
	public void check(IConnectionUserBean user) throws ResourceException;
}
