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
package com.arcadsoftware.database;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Abstract implementation of a simple Query Repository using a simple Resource bundle for
 * 
 * <p>
 * Concrete implementation must provide the ResourceBundle.
 * 
 * TODO Process dynamic modifications of the ResourceBundle.
 */
public abstract class QueryBundleRepository implements IQueryRepository {

	private ResourceBundle rb;
	private ResourceBundle sperb;
	
	public QueryBundleRepository() {
		super();
		rb = getRb();
		sperb = getSpeRb();
	}

	@Override
	public List<String> getQueriesList(String prefix) {
		List<String> result = new ArrayList<String>();
		Enumeration<String> e = rb.getKeys();
		String k;
		while (e.hasMoreElements()) {
			k = e.nextElement();
			if (k.startsWith(prefix)) {
				result.add(k.substring(prefix.length()));
			}
		}
		if (sperb != null) {
			e = sperb.getKeys();
			while (e.hasMoreElements()) {
				k = e.nextElement();
				if (k.startsWith(prefix)) {
					k = k.substring(prefix.length());
					if (result.indexOf(k) < 0) {
						result.add(k);
					}
				}
			}
		}
		return result;
	}

	@Override
	public String getQuery(String id) {
		try {
			if (sperb != null) {
				try {
					return sperb.getString(id);
				} catch (Exception e) {}
			}
			return rb.getString(id);
		} catch (MissingResourceException e) {
			return null;
		}
	}

	protected abstract ResourceBundle getRb();

	protected abstract ResourceBundle getSpeRb();

}
