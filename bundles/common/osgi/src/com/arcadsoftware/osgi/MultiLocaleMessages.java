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
package com.arcadsoftware.osgi;

import java.util.Hashtable;
import java.util.IllegalFormatException;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

/**
 * A Message Bundle access class, which support multiple locale at the same time. 
 * 
 * <p>
 * Use this class into a multi-language context (especially into a client server context).
 *
 * @author ARCAD Software
 */
public class MultiLocaleMessages {

	private Hashtable<String, ResourceBundle> messages = new Hashtable<String, ResourceBundle>(8);
	private ClassLoader classloader;
	private String resourceBundleName;

	/**
	 * Contruct the repository.
	 * @param resourceBundleName
	 * @param classloader
	 */
	public MultiLocaleMessages(String resourceBundleName,ClassLoader classloader) {
		super();
		this.classloader = classloader;
		this.resourceBundleName = resourceBundleName;
	}

	/**
	 * Return the specified resourceBundle.
	 * @param locale
	 * @return
	 */
	public ResourceBundle getRB(Locale locale) {
		if (locale == null) {
			locale = Locale.getDefault();
		}
		String name = locale.toString();
		ResourceBundle rb = messages.get(name);
		if (rb == null) {
			if (Locale.ENGLISH.getLanguage().equalsIgnoreCase(locale.getLanguage())) {
				locale = Locale.ROOT;
			}
			// Attention, si le locale n'est pas trouvé alors  le fonctionnement 
			// par défaut de ResourceBundle est d'utiliser le Locale de la machine, 
			// sans autre conditions ni avertissement. Pour éviter cela on 
			// force ici à l'utilisation du Locale ROOT qui correspond au "locale
			// par défaut".
			try {
				rb = ResourceBundle.getBundle(resourceBundleName, locale, classloader, new ResourceBundle.Control(){
					@Override
					public Locale getFallbackLocale(String baseName, Locale locale) {
					    if (Locale.ROOT.equals(locale)) {
					    	return null;
					    }
					    return Locale.ROOT;
					}
				});
			} catch (Throwable e) {
				rb = ResourceBundle.getBundle(resourceBundleName, Locale.ROOT, classloader);
			}
			messages.put(name, rb);
		}
		return rb;
	}

    /**
     * Gets a string for the given key from this resource bundle or one of its parents.
     * Calling this method is equivalent to calling get(key,language,key).
     *
     * @param key the key for the desired string
     * @param locale the desired language
     * @exception NullPointerException if <code>key</code> is <code>null</code>
     * @return the string for the given key
     */
	public String get(String key, Locale locale) {
		if (key == null) {
			return null;
		}
		try {
			return getRB(locale).getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

    /**
     * Gets a string for the given key from this resource bundle or one of its parents.
     * Calling this method is equivalent to calling
     *
     * @param key the key for the desired string
     * @param locale the desired language
     * @exception NullPointerException if <code>key</code> is <code>null</code>
     * @return the string for the given key
     */
	public String get(String key, Locale locale, String defaultValue) {
		if (key == null) {
			return defaultValue;
		}
		try {
			return getRB(locale).getString(key);
		} catch (MissingResourceException e) {
			return defaultValue;
		}
	}
	
    /**
     * Gets a string array for the given key from this resource bundle or one of its parents.
     * Calling this method is equivalent to calling
     *
     * @param key the key for the desired string array
     * @param locale the desired language
     * @exception NullPointerException if <code>key</code> is <code>null</code>
     * @exception MissingResourceException if no object for the given key can be found
     * @exception ClassCastException if the object found for the given key is not a string array
     * @return the string array for the given key
     */
	public String[] getStringArray(String key, Locale locale) {
		if (key == null) {
			return new String[]{};
		}
		try {
			return getRB(locale).getStringArray(key);
		} catch (MissingResourceException e) {
			return new String[] {key};
		}
	}

	/**
	 * Get the result of a format string in the given language.
	 * 
     * @param key the key for the desired string array
     * @param locale the desired language
	 * @param args
	 * @return The formatted String.
	 * @see String#format(Locale, String, Object...)
	 */
	public String get(String key, Locale locale, Object... args) {
		if (key == null) {
			return null;
		}
		String format = null;
		try {
			format = getRB(locale).getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
		if (format == null) {
			return key;
		}
		try {
			return String.format(locale, format, args);
		} catch (IllegalFormatException e) {
			return key;
		}
	}
	
	/**
	 * Get all the key accessible to the given language.
	 * 
	 * @param locale
	 * @return
	 */
	public Set<String> getKeys(Locale locale) {
		return getRB(locale).keySet();
	}
}
