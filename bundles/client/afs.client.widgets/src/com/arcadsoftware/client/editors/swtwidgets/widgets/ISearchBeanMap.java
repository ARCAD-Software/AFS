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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This interface permit to process a search from an MetaDataEntity.
 */
public interface ISearchBeanMap {

	/**
	 * Search a BeanMap.
	 * <p>
	 * This method allows to process a search and return the result as a BeanMap .
	 *
	 * @param structure
	 *            the BeanMap structure to search. shell
	 * @return a BeanMap or null if an error occurs or search is cancelled.
	 */
	public BeanMap search(MetaDataEntity structure);

}
