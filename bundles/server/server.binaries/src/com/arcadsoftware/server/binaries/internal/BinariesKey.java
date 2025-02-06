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
package com.arcadsoftware.server.binaries.internal;

import java.util.Date;

import com.arcadsoftware.crypt.Crypto;

/**
 * Key of accessible resource.
 */
public class BinariesKey {

	private final Date limit;
	private final int id;
	private final String category;
	private final int hc;
	private final String keyString;
	private final boolean readOnly;

	/**
	 * @param limit
	 * @param id
	 * @param category
	 */
	public BinariesKey(Date limit, int id, String category,boolean readOnly) {
		super();
		this.limit = limit;
		this.id = id;
		this.category = category;
		this.readOnly = readOnly;
		StringBuilder sb = new StringBuilder(55);
		sb.append(category);
		sb.append('#');
		sb.append(id);
		sb.append('*');
		sb.append(limit);
		sb.append(readOnly);
		keyString = Crypto.md5(sb.toString().toCharArray());
		byte[] key = Crypto.hexStringToByteArray(keyString); 
		if (key.length >= 4) {
			hc = key[0] << 24 | (key[1] & 0xff) << 16 | (key[2] & 0xff) << 8 | (key[3] & 0xff);
		} else {
			hc = super.hashCode();
		}
	}
	
	@Override
	public int hashCode() {
		return hc;
	}
	
	@Override
	public boolean equals(Object obj) {
		return (obj instanceof BinariesKey) && keyString.equals(((BinariesKey)obj).getKey());
	}

	public Date getLimit() {
		return limit;
	}

	public int getId() {
		return id;
	}

	public String getCategory() {
		return category;
	}

	public String getKey() {
		return keyString;
	}

	public boolean isReadOnly() {
		return readOnly;
	}
}
