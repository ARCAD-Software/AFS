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
package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This abstract class is the base implementation of a Search criteria.
 */
public abstract class AbstractSearchCriteria implements ISearchCriteria {

	/* (non-Javadoc)
	 * @see com.arcadsoftware.server.system.criteria.ISearchCriteria#reduce(com.arcadsoftware.server.system.criteria.ICriteriaContext)
	 */
	public ISearchCriteria reduce(ICriteriaContext context) {
		return this;
	}

	protected boolean nullsOrEquals(Object obj1, Object obj2) {
		if (obj1 == null) {
			return obj2 == null;
		} else {
			return obj1.equals(obj2);
		}
	}
	
	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}
	
	public BeanMapList select(BeanMapList list, IConnectionUserBean user) {
		if (list == null) {
			return new BeanMapList();
		}
		BeanMapList result = new BeanMapList(list.size());
		for(BeanMap bean:list) {
			if (test(bean, user)) {
				result.add(bean);
			}
		}
		return result;
	}

}
