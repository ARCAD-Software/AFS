/*******************************************************************************
 * Copyright (c) 2005, 2013 Cognos Incorporated, IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Cognos Incorporated - initial API and implementation
 *     IBM Corporation - bug fixes and enhancements
 *******************************************************************************/
package org.eclipse.equinox.internal.cm;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.*;
import java.util.Map.Entry;

/**
 * ConfigurationDictionary holds the actual configuration data and meets the various comparison
 * requirements of the Configuration Admin Service specification.
 */

public class ConfigurationDictionary extends Dictionary<String, Object> implements Serializable {

	private static final long serialVersionUID = -3583299578203095532L;
	private static final Collection<Class<?>> simples = Arrays.asList(new Class<?>[] {String.class, Integer.class, Long.class, Float.class, Double.class, Byte.class, Short.class, Character.class, Boolean.class});
	private static final Collection<Class<?>> simpleArrays = Arrays.asList(new Class<?>[] {String[].class, Integer[].class, Long[].class, Float[].class, Double[].class, Byte[].class, Short[].class, Character[].class, Boolean[].class});
	private static final Collection<Class<?>> primitiveArrays = Arrays.asList(new Class<?>[] {long[].class, int[].class, short[].class, char[].class, byte[].class, double[].class, float[].class, boolean[].class});

	static class CaseInsensitiveStringComparator implements Comparator<String>, Serializable {
		private static final long serialVersionUID = 6501536810492374044L;

		public int compare(String s1, String s2) {
			return (s1).compareToIgnoreCase(s2);
		}
	}

	protected final Map<String, Object> configurationProperties = Collections.synchronizedMap(new TreeMap<String, Object>(new CaseInsensitiveStringComparator()));

	private static void validateValue(Object value) {
		Class<?> clazz = value.getClass();

		// Is it in the set of simple types	
		if (simples.contains(clazz))
			return;

		// Is it an array of primitives or simples
		if (simpleArrays.contains(clazz) || primitiveArrays.contains(clazz))
			return;

		// Is it a Collection of simples
		if (value instanceof Collection) {
			Collection<?> valueCollection = (Collection<?>) value;
			for (Iterator<?> it = valueCollection.iterator(); it.hasNext();) {
				Class<?> containedClazz = it.next().getClass();
				if (!simples.contains(containedClazz)) {
					throw new IllegalArgumentException(containedClazz.getName() + " in " + clazz.getName()); //$NON-NLS-1$
				}
			}
			return;
		}
		throw new IllegalArgumentException(clazz.getName());
	}

	public Enumeration<Object> elements() {
		return new Enumeration<Object>() {
			final Iterator<Object> valuesIterator = configurationProperties.values().iterator();

			public boolean hasMoreElements() {
				return valuesIterator.hasNext();
			}

			public Object nextElement() {
				return valuesIterator.next();
			}
		};
	}

	public Object get(Object key) {
		if (key == null)
			throw new NullPointerException();
		return configurationProperties.get(key);
	}

	public boolean isEmpty() {
		return configurationProperties.isEmpty();
	}

	public Enumeration<String> keys() {
		return new Enumeration<String>() {
			Iterator<String> keysIterator = configurationProperties.keySet().iterator();

			public boolean hasMoreElements() {
				return keysIterator.hasNext();
			}

			public String nextElement() {
				return keysIterator.next();
			}
		};
	}

	public Object put(String key, Object value) {
		if (key == null || value == null)
			throw new NullPointerException();

		// Will throw an illegal argument exception if not a valid configuration property type
		validateValue(value);

		return configurationProperties.put(key, value);
	}

	public Object remove(Object key) {
		if (key == null)
			throw new NullPointerException();
		return configurationProperties.remove(key);
	}

	public int size() {
		return configurationProperties.size();
	}

	ConfigurationDictionary copy() {
		ConfigurationDictionary result = new ConfigurationDictionary();
		for (Entry<String, Object> entry : configurationProperties.entrySet()) {
			String key = entry.getKey();
			Object value = entry.getValue();
			if (value.getClass().isArray()) {
				int arrayLength = Array.getLength(value);
				Object copyOfArray = Array.newInstance(value.getClass().getComponentType(), arrayLength);
				System.arraycopy(value, 0, copyOfArray, 0, arrayLength);
				result.configurationProperties.put(key, copyOfArray);
			} else if (value instanceof Vector)
				result.configurationProperties.put(key, ((Vector<?>) value).clone());
			else
				result.configurationProperties.put(key, value);
		}
		return result;
	}
}
