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
package com.arcadsoftware.metadata.sql.internal;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Properties;
import java.util.Map.Entry;

/**
 *
 */
public class Escapes {

	private static final String BUNDLENAME = "com.arcadsoftware.metadata.sql.internal.escapes"; //$NON-NLS-1$
	private static final String PROPS = ".properties"; //$NON-NLS-1$
	private static final String KEY = ".key"; //$NON-NLS-1$
	private static final String VALUE = ".value"; //$NON-NLS-1$
	
	public final HashMap<String,String> escapes = new HashMap<String,String>(4);
	
	public Escapes(String name) {
		// Initialization.
		load(BUNDLENAME);
		if ((name != null) && (name.length() > 0)) {
			load(BUNDLENAME + '.' + name);
		}
	}

	private void load(String name) {
		InputStream inStream = Fragments.class.getClassLoader().getResourceAsStream(name.replace('.', '/') + PROPS);
		if (inStream != null) {
			try {
				Properties props = new Properties();
				props.load(inStream);
				for(Object keyo:props.keySet()) {
					String key = keyo.toString();
					if (key.endsWith(KEY)) {
						String prefix = key.substring(0, key.length() - KEY.length());
						String k = props.getProperty(prefix + KEY);
						String v = props.getProperty(prefix + VALUE);
						if (k != null) {
							if (v != null) {
								escapes.put(k,v);
							} else {
								escapes.remove(k);
							}
						}
					}
				}
			} catch (IOException e) {
				Activator.getInstance().debug(e);
			} finally {
				try {
					inStream.close();
				} catch (IOException e) {
					Activator.getInstance().debug(e);
				}
			}
		}
	}

	/**
	 * @param string
	 */
	public String escape(String string) {
		for (Entry<String, String> entry : escapes.entrySet()) {
			string = string.replace(entry.getKey(), entry.getValue());
		}
		return string;
	}
	
}
