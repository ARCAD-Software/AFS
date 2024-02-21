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
package com.arcadsoftware.osgi;

import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;

public class MapDictionary<K, V> extends Dictionary<K, V> {

	private final Map<K, V> map;
	
	public MapDictionary(Map<K, V> map) {
		super();
		this.map = map;
	}

	@Override
	public int size() {
		return map.size();
	}

	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}

	@Override
	public Enumeration<K> keys() {
		return new Enumeration<K>() {
			
			private final Iterator<K> keys = map.keySet().iterator();
			
			@Override
			public boolean hasMoreElements() {
				return keys.hasNext();
			}

			@Override
			public K nextElement() {
				return keys.next();
			}
		};
	}

	@Override
	public Enumeration<V> elements() {
		return new Enumeration<V>() {
			
			private final Iterator<V> values = map.values().iterator();
			
			@Override
			public boolean hasMoreElements() {
				return values.hasNext();
			}

			@Override
			public V nextElement() {
				return values.next();
			}
		};
	}

	@Override
	public V get(Object key) {
		return map.get(key);
	}

	@Override
	public V put(K key, V value) {
		return map.put(key, value);
	}

	@Override
	public V remove(Object key) {
		return map.remove(key);
	}
}
