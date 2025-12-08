package com.arcadsoftware.rest.console;

import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This interface allow to filter a Console Section if the current user does not match the required expectation.
 * 
 * @author ARCAD Software
 * @see IRestConsoleSection
 */
public interface ISecuredConsoleSection {

	/**
	 * Return true if and only if the given current user match the expected constraints to access to this Console section.
	 * 
	 * @param currentUser anon non user.
	 * @return
	 */
	public boolean hasRight(IConnectionUserBean currentUser);
}
