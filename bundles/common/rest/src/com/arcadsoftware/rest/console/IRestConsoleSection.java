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
package com.arcadsoftware.rest.console;

import java.util.List;

import org.restlet.data.Form;
import org.restlet.data.Language;

/**
 * Define a REST Console Section.
 * 
 * <p>This OSGi service will be raised as a Console form element.  
 * 
 * 
 * Creation Date: 29 mars 2011
 */
public interface IRestConsoleSection {

	/**
	 * OSGi Service ID.
	 */
	public static final String clazz = IRestConsoleSection.class.getName();

	/**
	 * Return the section label as shown to the user into the categories list.
	 * 
	 * @param language
	 * @return
	 */
	public String getLabel(Language language);
	
	/**
	 * 
	 * @return
	 */
	public String getId();
	
	/**
	 * Return the name of the console's category as shown the user.
	 * 
	 * @param language
	 * @return
	 */
	public String getCategory(Language language);
	
	/**
	 * The keywords may be used to filter the sections displayed in the section list.
	 * 
	 * @return null, or a string of word separated by white space.
	 */
	public String getKeywords();
	
	/**
	 * Return a relative order used to sort the console into its categories.
	 * <p>By convention, <1000 are first place sections, >10000 are the less important sections 
	 * @return
	 */
	public int getOrder();
	
	/**
	 * Return an icon number that can be shown with the section into the selection menu or title bar.
	 * 
	 * this value refer to a number corresponding to the constant defined into the class <code>ConsoleField</code>.
	 * @return zero if none icon has to be shown.
	 * @see ConsoleField
	 */
	public int getIcon();
	
	/**
	 * Return the Section form to send to the client.
	 * 
	 * @param language The client desired language.
	 * @return A list of field and action that compose the section form. 
	 */
	public List<ConsoleField> getForm(Language language);
	
	/**
	 * Return Help page associated to this section.
	 * 
	 * @param language The client desired language.
	 * @return
	 */
	public String getHelp(Language language);

	/**
	 * Run the selected action (that have been declared into a previous form). 
	 * 
	 * <p>The action result is a form that can be a part of the previous form or a new form to show into a dialog box.
	 * 
	 * @param actionId
	 * @param language
	 * @param params
	 * @return
	 */
	public List<ConsoleField> performAction(String actionId, Language language, Form params);
}
