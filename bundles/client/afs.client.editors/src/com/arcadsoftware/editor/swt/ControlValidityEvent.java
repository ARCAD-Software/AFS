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

import java.util.EventObject;

import com.arcadsoftware.metadata.MetaDataTest;

/**
 * This event is fired whan a Control test fail. It is fired just the first time the test fail. It is fired a second
 * time when the test is successful again.
 * <p>
 * If this test is a critical one the the BeanMap will not be saved until the test is successful again.
 */
public class ControlValidityEvent extends EventObject {

	private static final long serialVersionUID = 7863051522611179630L;

	private final boolean valid;

	public ControlValidityEvent(MetaDataTest source, boolean valid) {
		super(source);
		this.valid = valid;
	}

	/**
	 * @return the test that has been executed.
	 */
	@Override
	public MetaDataTest getSource() {
		return (MetaDataTest) super.getSource();
	}

	/**
	 * @return True if this test was successful.
	 */
	public boolean isValid() {
		return valid;
	}
}
