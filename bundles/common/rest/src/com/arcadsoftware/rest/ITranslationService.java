/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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

import org.restlet.data.Language;

/**
 * This OSGi service allow to translate codes according to external specifications.
 * 
 * Creation Date: 7 jan. 2011
 */
public interface ITranslationService {

	/**
	 * OSGi service class name.
	 */
	public static final String clazz = ITranslationService.class.getName();

	/**
	 * 
	 * Translate a string code (or return the code if no translation is found).
	 * 
	 * @param domainName The global language key domain. 
	 * @param code The key code to translate.
	 * @param language The language to translate to.
	 * @return
	 */
	public String translate(String domainName, String code, Language language);
	
}
