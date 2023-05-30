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
package com.arcadsoftware.server.scheduler.internal.resources;

import org.restlet.data.Method;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.rest.BeanMapParentResource;
import com.arcadsoftware.server.scheduler.ISchedulerService;

public class ScheduledItemListResource extends BeanMapParentResource {

	private final static String ENTITY_GEN_SCHEDULED_ITEM = "scheduleditem"; //$NON-NLS-1$
	private final static String ATTRIBUTE_TYPE = "type"; //$NON-NLS-1$

	@Override
	public boolean hasRight(Method method) {
		return Method.GET.equals(method);
	}

	@Override
	protected BeanMapList list(BeanMap parameters) throws ResourceException {
		ISchedulerService service = getOSGiService(ISchedulerService.class);
		if (service == null) {
			return new BeanMapList();
		}
		return service.listScheduledItems(getItemsType());
	}

	@Override
	public String getType() {
		return ENTITY_GEN_SCHEDULED_ITEM;
	}
	
	@Override
	protected BeanMap post(BeanMap request) throws ResourceException {
		// abstract implementation, never called.
		return null;
	}

	private String getItemsType() {
		return getAttribute(ATTRIBUTE_TYPE);
	}
}
