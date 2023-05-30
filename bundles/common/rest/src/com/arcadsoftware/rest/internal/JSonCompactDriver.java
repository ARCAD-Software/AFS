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
package com.arcadsoftware.rest.internal;

import java.io.Writer;
import java.util.List;

import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.json.JsonHierarchicalStreamDriver;
import com.thoughtworks.xstream.io.json.JsonWriter;
import com.thoughtworks.xstream.io.naming.NameCoder;

public class JSonCompactDriver extends JsonHierarchicalStreamDriver {

	private final boolean listArray;
	private final boolean noRoot;
	private final boolean addClass;
	
	public JSonCompactDriver() {
		super();
		this.listArray = false;
		noRoot = false;
		addClass = false;
	}

	public JSonCompactDriver(NameCoder nameCoder) {
		super(nameCoder);
		this.listArray = false;
		noRoot = false;
		addClass = false;
	}

	public JSonCompactDriver(boolean listArray) {
		super();
		this.listArray = listArray;
		noRoot = false;
		addClass = false;
	}

	public JSonCompactDriver(boolean listArray, boolean noRoot, boolean addClass) {
		super();
		this.listArray = listArray;
		this.noRoot = noRoot;
		this.addClass = addClass;
	}

	@Override
	public HierarchicalStreamWriter createWriter(Writer writer) {
		int mode = 0;
		if (noRoot) {
			mode = JsonWriter.DROP_ROOT_MODE;
		}
		return new JsonWriter(writer, mode, 
				new JsonWriter.Format(new char[0], new char[0], 0)) {
			
			@Override
			protected boolean isArray(@SuppressWarnings("rawtypes") Class clazz) {
				if (clazz == null) {
					return false;
				}
				if (listArray && List.class.isAssignableFrom(clazz)) {
					return true;
				}
				// Only Arrays are arrays !
		        return clazz.isArray();
			}
			
			@Override
			public String encodeAttribute(String name) {
				// Remove the "@" prefix that XSTream add !!!
				if ((name != null) && !name.isEmpty()) {
					return super.encodeAttribute(name.substring(1));
				}
				return super.encodeAttribute(name);
			}
			
			@Override
			protected void addValue(String value, Type type) {
				if (value == null) {
					super.addValue("null", Type.NULL);
				} else {
					super.addValue(value, type);
				}
			}

			@Override
			public void startNode(String name, @SuppressWarnings("rawtypes") Class clazz) {
				super.startNode(name, clazz);
				if (addClass && (clazz != null) && clazz.getName().startsWith("com.")) { //$NON-NLS-1$
					super.addAttribute("class", clazz.getSimpleName());
				}
			}
		};
	}
}
