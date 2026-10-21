package com.arcadsoftware.rest.console;

import java.util.List;

import org.restlet.data.Form;
import org.restlet.data.Language;

import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This interface allow to filter a Console Section if the current user does not match the required expectation.
 * 
 * @author ARCAD Software
 * @see IRestConsoleSection
 */
public interface ISecuredConsoleSection extends IRestConsoleSection {

	/**
	 * Return true if and only if the given current user match the expected constraints to access to this Console section.
	 * 
	 * @param currentUser anon non user.
	 * @return
	 */
	public boolean hasRight(IConnectionUserBean currentUser);
	
	/**
	 * Run the selected action (that have been declared into a previous form). 
	 * 
	 * <p>
	 * The action result is a form that can be a part of the previous form or a new form to show into a dialog box.
	 * 
	 * <p>
	 * This method replace the method {@link #performAction(String, Language, Form)}. Default implementation fallback on this version of the method.
	 *  
	 * @param actionId
	 * @param currentUser
	 * @param language
	 * @param params
	 * @return
	 */
	public default List<ConsoleField> performAction(String actionId, IConnectionUserBean currentUser, Language language, Form params) {
		return performAction(actionId, language, params);
	}

	@Override
	public default List<ConsoleField> performAction(String actionId, Language language, Form params) {
		return null;
	}
	
}
