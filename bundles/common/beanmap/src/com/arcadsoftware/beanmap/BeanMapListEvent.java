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
package com.arcadsoftware.beanmap;

import java.util.EventObject;

/**
 * This event is used when a BeanMapList is changed or loaded.
 * 
 * @see IBeanMapListListener
 */
public class BeanMapListEvent extends EventObject {

	private static final long serialVersionUID = -498449401408413048L;

	/**
	 * @param source
	 */
	public BeanMapListEvent(BeanMapList source) {
		super(source);
	}


	/* (non-Javadoc)
	 * @see java.util.EventObject#getSource()
	 */
	public BeanMapList getSource() {
		return (BeanMapList)source;
	}
}
