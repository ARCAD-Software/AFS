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

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Set;

/**
 * Wrap a Dictionary has a Map.
 * 
 * @author ARCAD Software
 * @param <K>
 * @param <V>
 */
public class DictionaryMap<K, V> extends AbstractMap<K, V> {

	private final Dictionary<K, V> properties;
	
	public DictionaryMap(Dictionary<K, V> properties) {
		super();
		this.properties = properties;
	}
	
	@Override
	public Set<Entry<K, V>> entrySet() {
		return new AbstractSet<Entry<K, V>>() {
            @Override
            public Iterator<Entry<K, V>> iterator() {
                return new Iterator<Entry<K, V>>() {
                	
                    private final Enumeration<K> keys = properties.keys();
                    private final Enumeration<V> elements = properties.elements();
                    private K k = null;

                    @Override
                    public boolean hasNext() {
                        return keys.hasMoreElements();
                    }

                    @Override
                    public Entry<K, V> next() {
                        k = keys.nextElement();
                        return new SimpleEntry<>(k,
                                elements.nextElement());
                    }

                    @Override
                    public void remove() {
                        if (k == null) {
                            throw new IllegalStateException();
                        }
                        properties.remove(k);
                        k = null;
                    }
                };
            }

            @Override
            public int size() {
                return properties.size();
            }
        };
	}
	
    @Override
    public int size() {
        return properties.size();
    }

    @Override
    public boolean containsKey(Object key) {
        return properties.get(key) != null;
    }

    @Override
    public V get(Object key) {
        return properties.get(key);
    }

    @Override
    public V put(K key, V value) {
        return properties.put(key, value);
    }

    @Override
    public V remove(Object key) {
        return properties.remove(key);
    }
}
