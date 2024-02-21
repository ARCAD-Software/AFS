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

import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.representation.StringRepresentation;

/**
 * Wrapper around a JSON string representation.
 * 
 * @deprecated use JSONRepresentation.
 * @see JSOMRepresentation.
 */
public class XJsonRepresentation extends StringRepresentation {

	/**
	 * Create a Application/JSON representation. 
	 * Encoded with UTF-8 and with English language.
	 *  
	 * @param text
	 */
	public XJsonRepresentation(CharSequence text) {
		super(text, MediaType.APPLICATION_JSON, Language.ENGLISH, CharacterSet.UTF_8);
	}

	/**
	 * Create a Application/JSON representation.
	 * Encoded with UTF-8. 
	 *  
	 * @param text
	 * @param language
	 */
	public XJsonRepresentation(CharSequence text, Language language) {
		super(text, MediaType.APPLICATION_JSON, language, CharacterSet.UTF_8);
	}

}
