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
package com.arcadsoftware.rest.internal;

import java.util.ResourceBundle;

import org.restlet.data.Language;

import com.arcadsoftware.rest.MultiLanguageMessages;

public class ClientMessages extends MultiLanguageMessages {

	static private ClientMessages instance = new ClientMessages();
	
	protected ClientMessages() {
		super("com.arcadsoftware.rest.internal.clientmessages", ClientMessages.class.getClassLoader()); //$NON-NLS-1$
	}

	static public String getString(String key, Language language) {
		if (instance != null) {
			return instance.get(key, language);
		}
		return key;
	}
	
	static public ResourceBundle getResourceBundle(Language language) {
		if (instance != null) {
			return instance.getRB(language);
		}
		return null;
	}
}
