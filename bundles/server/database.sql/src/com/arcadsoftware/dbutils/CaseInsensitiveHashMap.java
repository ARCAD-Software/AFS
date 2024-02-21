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
package com.arcadsoftware.dbutils;

import java.util.HashMap;
import java.util.Map;

/**
 * A Map that converts all keys to lower case Strings for case insensitive
 * lookups.  This is needed for the toMap() implementation because 
 * databases don't consistently handle the casing of column names. 
 */
public class CaseInsensitiveHashMap extends HashMap<String, Object> {

	private static final long serialVersionUID = 6330638367672127283L;

	@Override
    public boolean containsKey(Object key) {
        return super.containsKey(key.toString().toLowerCase());
    }

	@Override
    public Object get(Object key) {
        return super.get(key.toString().toLowerCase());
    }

	@Override
	public Object put(String key, Object value) {
        return super.put(key.toString().toLowerCase(), value);
    }

	@Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        for(java.util.Map.Entry<? extends String, ? extends Object> e: m.entrySet()) {
            put(e.getKey(), e.getValue());
        }
    }

	@Override
    public Object remove(Object key) {
        return super.remove(key.toString().toLowerCase());
    }
}
