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
package com.arcadsoftware.rest;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Set;

import org.restlet.data.Language;

import com.arcadsoftware.osgi.MultiLocaleMessages;

/**
 * This class implement a over simplified multi-language ResourceBundle repository.
 * Resources bundles are loaded on demands and stored into an hashtable.
 */
public class MultiLanguageMessages extends MultiLocaleMessages {
	
	/**
	 * Transform a Restlet Language object into a Java Locale one.
	 * @param language
	 * @return
	 */
	public static Locale toLocale(Language language) {
		if (language != null) {
			String[] langs = language.getName().split("-"); //$NON-NLS-1$
			if (langs.length > 1) {
				return new Locale(langs[0],langs[1],""); //$NON-NLS-1$
			} else if (langs.length == 1) {
				return new Locale(langs[0],"",""); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		return Locale.US;
	}
	
	/**
	 * Transform a Java Locale object into a Restlet Language one.
	 * 
	 * @param locale
	 * @return
	 */
	public static Language toLanguage(Locale locale) {
		if (locale != null) {
			if ((locale.getCountry() != null) && (locale.getCountry().length() > 0)) {
				return new Language(locale.getLanguage() + "-" + locale.getCountry()); //$NON-NLS-1$
			} else {
				return new Language(locale.getLanguage()); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		return Language.ENGLISH_US;
	}
	
	/**
	 * Return the language code part of the Restlet Language object.
	 * 
	 * @param language
	 * @return a language code, or "en" by default.
	 */
	public static String getLangCode(Language language) {
		String langname = language.getName(); 
		if (Language.ALL.equals(language) || (langname.length() == 0)) { //$NON-NLS-1$
			return "en"; //$NON-NLS-1$
		}
		if (langname.length() < 2) {
			return "en"; //$NON-NLS-1$
		}
		return langname.substring(0, 2);
	}
	
	/**
	 * Return the country code part of the Restlet Language object.
	 * 
	 * @param language
	 * @return a language code, or null by default.
	 */
	public static String getCountryCode(Language language) {
		String langname = language.getName(); 
		if (Language.ALL.equals(language) || (langname.length() == 0)) { //$NON-NLS-1$
			return null;
		}
		if (langname.length() < 5) {
			return null;
		}
		return langname.substring(3, 5);
	}
	
	/**
	 * Contruct the repository.
	 * @param resourceBundleName
	 * @param classloader
	 */
	public MultiLanguageMessages(String resourceBundleName, ClassLoader classloader) {
		super(resourceBundleName, classloader);
	}

	/**
	 * Return the specified resourceBundle.
	 * @param language
	 * @return
	 */
	public ResourceBundle getRB(Language language) {
		return getRB(toLocale(language));
	}

    /**
     * Gets a string for the given key from this resource bundle or one of its parents.
     * Calling this method is equivalent to calling get(key,language,key).
     *
     * @param key the key for the desired string
     * @param language the desired language
     * @exception NullPointerException if <code>key</code> is <code>null</code>
     * @return the string for the given key
     */
	public String get(String key, Language language) {
		return get(key, toLocale(language));
	}

    /**
     * Gets a string for the given key from this resource bundle or one of its parents.
     *
     * @param key the key for the desired string
     * @param language the desired language
     * @exception NullPointerException if <code>key</code> is <code>null</code>
     * @return the string for the given key
     */
	public String get(String key, Language language, String defaultValue) {
		return get(key, toLocale(language), defaultValue);
	}
	
	/**
	 * Get the result of a format string in the given language.
	 * 
     * @param key the key for the desired string array
     * @param locale the desired language
	 * @param args the format arguments.
	 * @return The formatted String.
	 * @see String#format(Locale, String, Object...)
	 */
	public String get(String key, Language language, Object... values) {
		return get(key, toLocale(language), values);
	}
	
    /**
     * Gets a string array for the given key from this resource bundle or one of its parents.
     *
     * @param key the key for the desired string array
     * @param language the desired language
     * @exception NullPointerException if <code>key</code> is <code>null</code>
     * @exception MissingResourceException if no object for the given key can be found
     * @exception ClassCastException if the object found for the given key is not a string array
     * @return the string array for the given key
     */
	public String[] getStringArray(String key, Language language) {
		return getStringArray(key, toLocale(language));
	}
	
	/**
	 * Get all the key accessible to the given language.
	 * 
	 * @param language
	 * @return
	 */
	public Set<String> getKeys(Language language) {
		return getRB(toLocale(language)).keySet();
	}
}
