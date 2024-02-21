/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.beanmap.internal;

import java.util.Iterator;

import com.arcadsoftware.beanmap.BeanMap;

public class BeanMapIterator implements Iterator<BeanMap> {

	private final Iterator<BeanMap> listIterator;
	private final String attribute;
	private final Object value;
	private BeanMap next;
	private boolean given;
	
	public BeanMapIterator(Iterator<BeanMap> listIterator, String attribute, Object value) {
		super();
		given = true;
		this.listIterator = listIterator;
		this.attribute = attribute;
		this.value = value;
	}

	@Override
	public boolean hasNext() {
		if (given) {
			given = false;
			if (value == null) {
				while (listIterator.hasNext()) {
					next = listIterator.next();
					if (next.get(attribute) == null) {
						return true;
					}
				}
			} else {
				while (listIterator.hasNext()) {
					next = listIterator.next();
					if (value.equals(next.get(attribute))) {
						return true;
					}
				}
			}
			next = null;
			return false;
		}
		return next != null;
	}

	@Override
	public BeanMap next() {
		given = true;
		return next;
	}

	@Override
	public void remove() {
		if (next != null) {
			given = true;
			listIterator.remove();
		}
	}

}
