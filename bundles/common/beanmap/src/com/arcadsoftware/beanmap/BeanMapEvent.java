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
package com.arcadsoftware.beanmap;

import java.util.EventObject;

/**
 * This event is used when a BeanMap is changed or loaded.
 * 
 * @see IBeanMapListener
 */
public class BeanMapEvent extends EventObject {

	private static final long serialVersionUID = 2760357139630075260L;

	/**
	 * @param source The BeanMap linked to this Event.
	 */
	public BeanMapEvent(IBeanMap source) {
		super(source);
	}

	/* (non-Javadoc)
	 * @see java.util.EventObject#getSource()
	 */
	public IBeanMap getSource() {
		return (IBeanMap)source;
	}
}
