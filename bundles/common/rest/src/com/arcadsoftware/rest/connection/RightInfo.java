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
package com.arcadsoftware.rest.connection;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

/**
 * Contain some contextual (translated) information about an Access Right (this is the translated transposition of the actual Right Metadata Entity values if
 * this service is available.
 * 
 * @author ARCAD Software
 * @see IProfileRightsListService
 */
public class RightInfo implements Comparable<RightInfo> {

	private final int id;
	private final int param;
	private final String code;
	private final String label;
	
	public RightInfo(int id, int param, String code, String label) {
		super();
		this.id = id;
		this.param = param;
		this.code = code;
		this.label = label;
	}

	public int getId() {
		return id;
	}

	public String getCode() {
		return code;
	}

	public String getLabel() {
		return label;
	}

	@Override
	public int hashCode() {
		return id;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Integer) {
			return id == (Integer) obj;
		}
		return (obj instanceof RightInfo) && (id == ((RightInfo) obj).id);
	}

	@Override
	public String toString() {
		return '[' + id + "] " + code + ": " + label;
	}

	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		try {
			result.put("id", id); //$NON-NLS-1$
			if (param != 0) {
				result.put("param", param); //$NON-NLS-1$
			}
			if (code != null) {
				result.put("code", code); //$NON-NLS-1$
			}
			if (label != null) {
				result.put("label", label); //$NON-NLS-1$
			}
		} catch (JSONException e) {}
		return result;
	}

	public int getParam() {
		return param;
	}

	@Override
	public int compareTo(RightInfo o) {
		int r = id - o.id;
		if (r != 0) {
			return r;
		}
		return param - o.param;
	}
}
