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
package com.arcadsoftware.editor.swt;

public class MandatoryAttribute {

	public String code;
	public String conditionedBy;

	public MandatoryAttribute(String code) {
		this.code = code;
	}

	public MandatoryAttribute(String code, String conditionedBy) {
		this(code);
		this.conditionedBy = conditionedBy;
	}

	public boolean isConditionned() {
		return (conditionedBy != null) && (conditionedBy.length() > 0);
	}

	public String getCode() {
		return code;
	}

	public String getConditionedBy() {
		return conditionedBy;
	}
}
