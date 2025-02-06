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
package com.arcadsoftware.metadata.criteria.natural;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;

public class CriteriaParser extends BaseCriteriaParser {

	public static ISearchCriteria parse(String criteria) throws ResourceException {
		if ((criteria == null) || criteria.isEmpty()) {
			return null;
		}
		try (StringReader sr = new StringReader(criteria)) {
			return new CriteriaParser(sr).criteria();
		} catch (Exception e) {
			if (Activator.getInstance() != null) {
				Activator.getInstance().error(e);
			} else {
				e.printStackTrace();
			}
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, e.getLocalizedMessage());
		}
	}
	
	public CriteriaParser(InputStream stream) {
		super(stream, "UTF-8");
	}

	public CriteriaParser(Reader stream) {
		super(stream);
	}
}
