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
package com.arcadsoftware.metadata.server.user.internal;

import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

import com.arcadsoftware.metadata.MetaDataEventHandler;

public class AllRightProfileUpgrade implements EventHandler {

	private final Activator activator;
	
	public AllRightProfileUpgrade(Activator activator) {
		super();
		this.activator = activator;
	}
	
	@Override
	public void handleEvent(Event event) {
		if (MetaDataEventHandler.TOPIC_ENTITY_CREATED.equals(event.getTopic())) {
			Object type = event.getProperty(MetaDataEventHandler.EVENT_PROP_TYPE);
			if (!Activator.TYPE_PROFILE.equals(type) && !Activator.TYPE_PROFILERIGHT.equals(type) && !Activator.TYPE_RIGHT.equals(type)) {
				return;
			}
		}
		activator.updateAllRightProfile();
	}

}
