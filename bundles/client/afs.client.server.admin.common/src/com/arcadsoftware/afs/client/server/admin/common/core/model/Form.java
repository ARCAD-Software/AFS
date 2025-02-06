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
package com.arcadsoftware.afs.client.server.admin.common.core.model;

import java.util.List;

import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleMessage;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.SectionId;

public class Form {
	SectionId section;
	List<ConsoleField> fields;

	public Form(SectionId section, List<ConsoleField> fields) {
		this.fields = fields;
		this.section = section;
	}

	public boolean containsAction() {
		for (final ConsoleField c : fields) {
			if (c instanceof ConsoleAction) {
				return true;
			}
		}
		return false;
	}

	public List<ConsoleField> getFields() {
		return fields;
	}

	public boolean isMessageOnly() {
		return ((fields.size() == 1) && (fields.get(0) instanceof ConsoleMessage));
	}

	public SectionId getSection() {
		return section;
	}

	public ConsoleProperty getProperty(String id) {
		for (final ConsoleField field : fields) {
			if (field instanceof ConsoleProperty) {
				final ConsoleProperty prop = (ConsoleProperty) field;
				if (prop.getId().equalsIgnoreCase(id)) {
					return prop;
				}
			}
		}
		return null;
	}

}
