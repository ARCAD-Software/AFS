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
package com.arcadsoftware.metadata.template;

/**
 * Define a modification condition that should enable a ITemplate. 
 * 
 * <p>
 * This condition may include an MetaDataAttribute identified by its code
 * (references may be used).
 * 
 * <p>
 * If a value is given the the condition is activated only if the attribute
 * gain this specific value. If none value is given then any modification
 * of the attribute is checked.
 * 
 * <p>
 * If an old value is given then the condition is activated only if the last 
 * value of the attribute was equal to the given one. If non "old value" is 
 * given then the condition is activated for any last value.
 * 
 * @see ITemplate#getChanged()
 */
public class ChangedAttribute {

	private String code;
	private String value;
	private String oldValue;

	public ChangedAttribute(String code) {
		super();
		this.code = code;
	}

	public ChangedAttribute(String code, String value) {
		super();
		this.code = code;
		this.value = value;
	}

	public ChangedAttribute(String code, String value, String oldValue) {
		super();
		this.code = code;
		this.value = value;
		this.oldValue = oldValue;
	}

	public String getCode() {
		return code;
	}
	
	public String getValue() {
		return value;
	}
	
	public String getOldValue() {
		return oldValue;
	}
	
	public void setCode(String code) {
		this.code = code;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
	
	public void setOldValue(String oldValue) {
		this.oldValue = oldValue;
	}
	
}
