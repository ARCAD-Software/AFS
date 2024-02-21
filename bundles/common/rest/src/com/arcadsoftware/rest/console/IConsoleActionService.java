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
package com.arcadsoftware.rest.console;

import java.util.Dictionary;
import java.util.List;

import org.restlet.data.Language;

/**
 * This service allow to add an action to a set of Configuration console Sections. 
 * 
 * @author ARCAD Software
 * @see ConsoleAction
 */
public interface IConsoleActionService {

	/**
	 * OSGi Service identifier.
	 */
	public static final String clazz = IConsoleActionService.class.getName(); 
	
	/**
	 * A space separated list of Console Section identifiers to which this action
	 * will be associated. 
	 */
	public static final String PROP_SECTIONIDS = "com.arcadsoftware.console.sections"; //$NON-NLS-1$

	/**
	 * The identifier of this action.
	 * 
	 * <p>
	 * For a given console section this code is supposed to be unique.
	 * 
	 * <p>
	 * "save" should not be used, as this is the default Section action code.
	 * @return
	 */
	public String getCode();
	
	/**
	 * The localized label of this action. This is the text taht will be présented to the user into the menu or button caption.
	 * 
	 * @param language
	 * @return
	 */
	public String getLabel(Language language);
	
	/**
	 * The localized help message of this action.
	 * @param language
	 * @return
	 */
	public String getHelp(Language language);
	
	/**
	 * The icon id of this action.
	 * @return
	 * @see ConsoleField
	 */
	public int getIcon();
	
	/**
	 * Indicate if this action must be listed into the list of available action of the section.
	 * 
	 * <p>
	 * Hidden action are useful to create chained actions (i.e. one action called into the form returned by another one and so on...).
	 * 
	 * @return
	 */
	public boolean isHidden();
	
	/**
	 * Process the action.
	 * 
	 * @param section The Console section id from where this action is run.
	 * @param language The current user language.
	 * @param currentConf The current configuration associated with this section if any.
	 * @param properties The values of the console section properties passed with the action call.
	 * @return a form of an empty list, or null if the action is a success.
	 */
	@SuppressWarnings("rawtypes")
	public List<ConsoleField> run(String section, Language language, Dictionary currentConf, Dictionary properties);
}
