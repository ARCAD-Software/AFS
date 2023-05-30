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
package com.arcadsoftware.afs.framework.application.internal;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class Messages {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName().concat(".messages"); //$NON-NLS-1$	
	private static ResourceBundle resourceBundle;
	
	private Messages() {
		super();
	
	}

	private static ResourceBundle getResourceBundle(){
		try {
			resourceBundle = ResourceBundle.getBundle(
					BUNDLE_NAME, Locale.getDefault()); //$NON-NLS-1$
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}	
		return resourceBundle;
	}
	
	
	public static String resString(String key) {
		if(getResourceBundle()!=null) {
			try {
				String value = resourceBundle.getString(key);
				if (value.startsWith("!")) //$NON-NLS-1$
					return resString(value.substring(1));
				else
					return value;
			} catch (MissingResourceException e) {
				return key;
			}
		}
		return key;
	}
	

}
