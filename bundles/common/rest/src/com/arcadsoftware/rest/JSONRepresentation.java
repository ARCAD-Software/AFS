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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Date;

import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.representation.StringRepresentation;

/**
 * Utility class to represent a JSON document.
 */
public class JSONRepresentation extends StringRepresentation {

	/**
	 * JSon representation from a File.
	 * 
	 * @param file
	 * @param language
	 * @return
	 */
	public static JSONRepresentation fromFile(File file, Language language) {
		StringBuilder sb = new StringBuilder();
		FileInputStream fip;
		try {
			fip = new FileInputStream(file);
			try {
				byte[] buffer = new byte[1024]; // more than needed
				int x;
				while ((x = fip.read(buffer)) != -1) {
					sb.append(new String(buffer, 0, x));
				}
			} catch (IOException e) {
			} finally {
				try {
					fip.close();
				} catch (IOException e) {
				}
			}
		} catch (FileNotFoundException e) {
			return null;
		}
		return new JSONRepresentation(sb, language, new Date(file.lastModified()));
	}
	
	/**
	 * JSON representation using ENGLISH language and UTF8 character set.
	 * 
	 * @param text
	 */
	public JSONRepresentation(CharSequence text) {
		super(text, MediaType.APPLICATION_JSON, Language.ENGLISH, CharacterSet.UTF_8);
	}

	/**
	 * JSON representation using UTF8 character set.
	 * 
	 * @param text
	 * @param language
	 */
	public JSONRepresentation(CharSequence text, Language language) {
		super(text, MediaType.APPLICATION_JSON, language, CharacterSet.UTF_8);
	}

	/**
	 * JSON representation using ENGLISH language and UTF8 character set.
	 * 
	 * @param text
	 * @param lastModification
	 */
	public JSONRepresentation(CharSequence text, Date lastModification) {
		super(text, MediaType.APPLICATION_JSON, Language.ENGLISH, CharacterSet.UTF_8);
		setModificationDate(lastModification);
	}

	/**
	 * JSON representation using UTF8 character set.
	 * 
	 * @param text
	 * @param language
	 * @param lastModification
	 */
	public JSONRepresentation(CharSequence text, Language language, Date lastModification) {
		super(text, MediaType.APPLICATION_JSON, language, CharacterSet.UTF_8);
		setModificationDate(lastModification);
	}

}
