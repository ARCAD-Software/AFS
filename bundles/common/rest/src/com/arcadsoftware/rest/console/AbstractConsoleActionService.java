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

import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.restlet.data.Language;

import com.arcadsoftware.rest.MultiLanguageMessages;

/**
 * Abstract implementation of the IConsoleActionService with translation utilities and messages builders. 
 * @author ARCAD Software
 */
public abstract class AbstractConsoleActionService implements IConsoleActionService {

	@SuppressWarnings("rawtypes")
	public static final Dictionary sections(String... sectionID) {
		if ((sectionID == null) || (sectionID.length == 0)) {
			return null;
		}
		StringBuilder ids = new StringBuilder(sectionID[0]);
		for (int i = 1; i < sectionID.length; i++) {
			ids.append(' ');
			ids.append(sectionID[i]);
		}
		Properties result = new Properties();
		result.put(PROP_SECTIONIDS, ids.toString());
		return result;
	}
	
	private MultiLanguageMessages messages;
	
	/**
	 * Create a new Console Action with an existing messages bundle reader (sharing this object between
	 * multiple action is more efficient.
	 * 
	 * @param messages A non null messages bundle reader.
	 */
	public AbstractConsoleActionService(MultiLanguageMessages messages) {
		super();
		this.messages = messages;
	}
	
	/**
	 * Create a new console action with a multi-language resource bundle reader.
	 * 
	 * @param resourceBundleName The resource path name of the 
	 * @param classloader The class loader to use to load the properties file resources.
	 */
	public AbstractConsoleActionService(String resourceBundleName, ClassLoader classloader) {
		super();
		this.messages = new MultiLanguageMessages(resourceBundleName, classloader);
	}

	/**
	 * Translate a test into the desired language.
	 * 
	 * @param label
	 * @param language
	 * @return
	 */
	protected String localize(String label, Language language) {
		return localize(label, language, label);
	}

	/**
	 * Translate a test into the desired language.
	 * 
	 * @param label
	 * @param language
	 * @param defaultValue
	 * @return
	 */
	protected String localize(String label, Language language, String defaultValue) {
		if (messages == null) {
			return defaultValue;
		}
		return messages.get(label, language, defaultValue);
	}

	/**
	 * Generate an information message that can be returned by this action.
	 * 
	 * <p>
	 * This method translate the given strings if required.
	 * 
	 * @param label The Label of the message
	 * @param message The message it self.
	 * @param help The help message, can be null.
	 * @param language The current client language.
	 * @return A Message form.
	 */
	protected List<ConsoleField> infoForm(String label, String message, String help, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(1);
		result.add(new ConsoleMessage(localize(label, language), localize(message, language), ConsoleField.ICON_NONE,
				localize(help, language)));
		return result;
	}

	/**
	 * Generate an error message that can be returned by this action.
	 * 
	 * <p>
	 * This method translate the given strings if required.
	 * 
	 * @param label The Label of the message
	 * @param message The message it self.
	 * @param help The help message, can be null.
	 * @param language The current client language.
	 * @return A Message form.
	 */
	protected List<ConsoleField> errorForm(String label, String message, String help, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(1);
		ConsoleMessage msg = new ConsoleMessage(localize(label, language), localize(message, language), ConsoleField.ICON_ERROR, localize(help, language));
		msg.setMessageType(ConsoleMessage.ERROR);
		result.add(msg);
		return result;
	}

	/**
	 * Generate an warning message that can be returned by this action.
	 * 
	 * <p>
	 * This method translate the given strings if required.
	 * 
	 * @param label The Label of the message
	 * @param message The message it self.
	 * @param help The help message, can be null.
	 * @param language The current client language.
	 * @return A Message form.
	 */
	protected List<ConsoleField> warnForm(String label, String message, String help, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(1);
		ConsoleMessage msg = new ConsoleMessage(localize(label, language), localize(message, language), ConsoleField.ICON_WARN, localize(help, language));
		msg.setMessageType(ConsoleMessage.WARNING);
		result.add(msg);
		return result;
	}

	/**
	 * Generate an debug message that can be returned by this action.
	 * 
	 * <p>
	 * This method translate the given strings if required.
	 * 
	 * @param label The Label of the message
	 * @param message The message it self.
	 * @param help The help message, can be null.
	 * @param language The current client language.
	 * @return A Message form.
	 */
	protected List<ConsoleField> debugForm(String label, String message, String help, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(1);
		ConsoleMessage msg = new ConsoleMessage(localize(label, language), localize(message, language), ConsoleField.ICON_DIAGNOSTIC, localize(help, language));
		msg.setMessageType(ConsoleMessage.DEBUG);
		result.add(msg);
		return result;
	}

	/**
	 * Initialise an "empty" form which can be used to generate a new form as a response to the current action.
	 * 
	 * <p>
	 * If this form may return to the current action use a "actionLabel" then the current action will be included to
	 * this form will a new (or the same) label.
	 * 
	 * <p>
	 * Note: when an action is run you can know if the caller come from the section form of any other action 
	 * generated form by including some hidden properties into your forms.
	 * 
	 * @param message If not null will add a translated message on the top of the new form.
	 * @param actionLabel If not null will add the current action as a new action.
	 * @param properties If not null will report all the original form values as hidden fields.
	 * @param language The current client language.
	 * @return A new form.
	 * @see ConsoleProperty
	 */
	@SuppressWarnings("rawtypes")
	protected List<ConsoleField> form(String message, String actionLabel, Dictionary properties, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>();
		// ajout du message
		if (message != null) {
			result.add(new ConsoleText(localize(message, language)));
		}
		// Report des propriété en champs caché !
		if (properties != null) {
			Enumeration keys = properties.keys();
			while(keys.hasMoreElements()) {
				String key = keys.nextElement().toString();
				result.add(new ConsoleProperty(key, null, 0, properties.get(key).toString(), null, null, true, true, null));
			}
		}
		if (actionLabel != null) {
			if (actionLabel.equals(getLabel())) {
				result.add(new ConsoleAction(getCode(), null, getLabel(language), getIcon(), false, getHelp(language)));
			} else {
				result.add(new ConsoleAction(getCode(), null, localize(actionLabel, language)));
			}
		}
		return result;
	}
	
	public final String getHelp(Language language) {
		String help = getHelp();
		if (help == null) {
			return null;
		}
		return localize(help, language, help);
	}

	/**
	 * Get the untranslated Help message, i.e. the key that will be read into the associated resource bundle.
	 * 
	 * <p>
	 * null if none.
	 * 
	 * @return
	 */
	protected String getHelp() {
		return getCode() + ".help"; //$NON-NLS-1$
	}

	public final String getLabel(Language language) {
		String label = getLabel();
		if (label == null) {
			return localize(getCode(), language, getCode());
		}
		return localize(label, language, label);
	}

	/**
	 * Get the untranslated label, i.e. the key that will be read into the associated resource bundle.
	 * 
	 * @return
	 */
	protected String getLabel() {
		return getCode();
	}

	public int getIcon() {
		return 0;
	}

	public boolean isHidden() {
		return false;
	}
	
	/**
	 * Try to parse an integer configuration parameter. Support Integer and Strings
	 * representations of this value.
	 *  
	 * @param param
	 * @param defaultValue
	 * @return
	 */
	public int parseIntegerParameter(Object param,int defaultValue) {
		if (param == null) {
			return defaultValue; 
		}
		if (param instanceof Integer) {
			return (Integer)param;
		}
		try {
			return Integer.parseInt(param.toString());
		} catch (NumberFormatException e) {
			return defaultValue;
		}
	}

	/**
	 * Try to parse a boolean parameter.
	 * 
	 * @param param The boolean object to parse.
	 * @return the Object boolean value or false if it is null.
	 */
	public boolean parseBooleanParameter(Object param) {
		return parseBooleanParameter(param, false);
	}
	
	/**
	 * Try to parse a boolean parameter.
	 * 
	 * @param param
	 * @param defaultValue The returned value if <code>param</code> is null. 
	 * @return
	 */
	public boolean parseBooleanParameter(Object param, boolean defaultValue) {
		if (param == null) {
			return defaultValue;
		}
		if (param instanceof Boolean) {
			return (Boolean) param;
		}
		if (param instanceof Integer) {
			return (Integer) param != 0;
		}
		if (defaultValue) {
			return !(param.toString().equalsIgnoreCase("false") || param.toString().equalsIgnoreCase("no")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return param.toString().equalsIgnoreCase("true") || param.toString().equalsIgnoreCase("yes"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/**
	 * Return the map of properties corresponding to the given prefix.
	 * 
	 * <p>The prefix is removed from the map keys.
	 * @param properties
	 * @param prefix the String prefix. Test is case sensitive.
	 * @return a Map, this map can be empty but never null.
	 */
	public Map<String,?> getMapProperties(Dictionary<?, ?> properties,String prefix) {
		HashMap<String, Object> result = new HashMap<String, Object>(properties.size());
		Enumeration<?> keys = properties.keys();
		int pl = prefix.length();
		while (keys.hasMoreElements()) {
			Object k = keys.nextElement();
			if (k != null) {
				String ks = k.toString();
				if (ks.startsWith(prefix)) {
					result.put(ks.substring(pl), properties.get(k));
				}
			}
		}
		return result;
	}

}
