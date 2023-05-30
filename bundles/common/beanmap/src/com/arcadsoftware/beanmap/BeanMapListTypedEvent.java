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


/**
 * This event is used when a BeanMapList is changed or loaded.
 * 
 * @see IBeanMapListListener
 */
public class BeanMapListTypedEvent extends BeanMapListEvent {

	private static final long serialVersionUID = 1309474420050426071L;

	private String type;

	/**
	 * @param source
	 */
	public BeanMapListTypedEvent(BeanMapList source) {
		super(source);
	}
	
	/**
	 * @param source
	 */
	public BeanMapListTypedEvent(BeanMapList source, String type) {
		this(source);
		setType(type);
	}

	public String getType() {
        return type;
    }
	
	public void setType(String type) {
       this.type = type;
    }

    /**
     * Returns a String representation of this EventObject.
     *
     * @return  A a String representation of this EventObject.
     */
    public String toString() {
        return super.toString() + "[type=" + type + "]";
    }
}
