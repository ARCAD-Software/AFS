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
package com.arcadsoftware.afs.client.server.admin.common.core.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.arcadsoftware.afs.client.core.connection.AbstractConnector;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.rest.console.Category;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.SectionId;
import com.arcadsoftware.rest.console.XmlConsoleStream;

public class ConsoleConnector extends AbstractConnector {

	public ConsoleConnector(ServerConnection server) {
		super(server);
	}

	public boolean save(String sectionId, Map<String, Object> values) {
		try {
			getDataAccess().getWebServicesAccess().post("/remote/console/" + sectionId + "/save", values); //$NON-NLS-1$ //$NON-NLS-2$
			return true;
		} catch (final ServerErrorException e) {
			manageErrorException(e);
		}
		return false;
	}

	public Form performAction(SectionId section, String actionId, Map<String, Object> values) {
		try {
			final String xml = getDataAccess().getWebServicesAccess()
					.post("/remote/console/" + section.getId() + "/" + actionId, values); //$NON-NLS-1$ //$NON-NLS-2$
			List<ConsoleField> result;
			if (!xml.equals("")) { //$NON-NLS-1$
				final XmlConsoleStream xmlStream = new XmlConsoleStream();
				result = xmlStream.getSectionForm(xml);
			} else {
				result = new ArrayList<>();
			}
			return new Form(section, result);
		} catch (final ServerErrorException e) {
			manageErrorException(e);
		}
		return null;
	}

	public List<Category> getCategories() {
		try {
			String xml;
			xml = getDataAccess().getWebServicesAccess().get("/remote/console/", true, false); //$NON-NLS-1$
			final XmlConsoleStream xmlStream = new XmlConsoleStream();
			return xmlStream.getCategories(xml);
		} catch (final ServerErrorException e) {
			manageErrorException(e);
		}
		return null;
	}

	public Form getSection(SectionId section) {
		try {
			String xml;
			xml = getDataAccess().getWebServicesAccess().get("/remote/console/" + section.getId(), true, false); //$NON-NLS-1$
			final XmlConsoleStream xmlStream = new XmlConsoleStream();
			// renvoie des objets

			// ConsoleSet

			// Action = save
			// Pour sauvagarder
			// dataAccess.getWebServicesAccess().post("/remote/console/<sectionid>/<save>",Map
			// [<id_de_la_property>,<value>))
			final List<ConsoleField> result = xmlStream.getSectionForm(xml);
			return new Form(section, result);
		} catch (final ServerErrorException e) {
			manageErrorException(e);
		}
		return null;
	}

}
