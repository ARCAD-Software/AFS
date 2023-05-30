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
package com.arcadsoftware.client.editors.swtwidgets.model;

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.beanmap.BeanMap;

/**
 * Represents an ArcadEntity with beanMap.
 */
public class BeanMapArcadEntity extends ArcadEntity {

	private final BeanMap beanMap;

	/**
	 * Creates a new instance of <code>BeanMapArcadEntity</code>.
	 * 
	 * @param beanMap
	 *            the beanMap value.
	 */
	public BeanMapArcadEntity(BeanMap beanMap) {
		super();
		this.beanMap = beanMap;
	}

	public String getLabel() {
		if (beanMap == null) {
			return null;
		}
		return beanMap.toString();
	}

	/**
	 * @return the beanMap
	 */
	public BeanMap getBeanMap() {
		return beanMap;
	}

}
