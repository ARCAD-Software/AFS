/**
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
 * 
 */
package com.arcadsoftware.metadata.criteria;

import java.util.Collection;

/**
 * 
 * @author ARCAD Software
 * @deprecated use {@link IdInListCriteria}
 */
public class IdInListCriteria extends InListCriteria {

	/**
	 * @param id
	 */
	public IdInListCriteria(int... id) {
		super(id);
	}

	/**
	 * @param ids
	 */
	public IdInListCriteria(Collection<Integer> ids) {
		super(ids);
	}

}
