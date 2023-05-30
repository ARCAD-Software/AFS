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

import java.util.Dictionary;
import java.util.Hashtable;

import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

import com.arcadsoftware.metadata.MetaDataEventHandler;

public class MapperAliasConfiguration implements EventHandler {

	private final Activator activator;
	
	public MapperAliasConfiguration(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public void handleEvent(Event event) {
		// Auto-declaration of a UserDB mapper domain alias...
		String domains = (String) event.getProperty(MetaDataEventHandler.EVENT_PROP_DOMAINS);
		if ((domains != null) && domains.contains(Activator.JDBC_PREFIX) && !domains.contains(Activator.USERDB)) {
			activator.debug("User Management: Automatic association to the first SQL Mapper...");
			String domain = null;
			for (String d: domains.split(" ")) { //$NON-NLS-1$
				if (d.startsWith(Activator.JDBC_PREFIX)) {
					domain = d;
				}
			}
			if (domain != null) {
				// A JDBC Mapper has been declared and it is used for userdb !
				Dictionary<String, Object> props = activator.getConfiguration(Activator.METADATAPID);
				// If there is no alias defined for the "userdb" use this domain !
				if (props == null) {
					props = new Hashtable<String, Object>();
					props.put(Activator.ALIAS_USERDB, domain);
					activator.setConfiguration(Activator.METADATAPID, props);
				} else if (props.get(Activator.ALIAS_USERDB) == null) {
					props.put(Activator.ALIAS_USERDB, domain);
					activator.setConfiguration(Activator.METADATAPID, props);
				}
			}
			// FIXME: The following thread may not work because "userdb" has been declared just before... the Entities are not yet updated !
			if (Activator.UPDATEALLRIGHTPROFILE) {
				activator.updateAllRightProfile();
			}
		}
	}
}
