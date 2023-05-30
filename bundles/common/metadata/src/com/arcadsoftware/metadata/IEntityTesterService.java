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
package com.arcadsoftware.metadata;

import java.util.ArrayList;
import java.util.List;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public interface IEntityTesterService {

	public static final String clazz = IEntityTesterService.class.getName();

	public boolean test(String event, MetaDataEntity entity, BeanMapList list, IConnectionUserBean user, Language language);

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language);

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, IConnectionUserBean user, Language language);

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, BeanMap newBean, ArrayList<MetaDataAttribute> attributes, IConnectionUserBean user, Language language);
	
}
