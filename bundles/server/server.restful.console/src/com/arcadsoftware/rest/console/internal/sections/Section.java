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
package com.arcadsoftware.rest.console.internal.sections;

import java.util.ArrayList;
import java.util.List;

import org.restlet.data.Language;

import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleProperty;

public abstract class Section extends AbstractSection {

	private ArrayList<ConsoleField> list = new ArrayList<ConsoleField>();

	public List<ConsoleField> getForm(Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(list.size());
		for (ConsoleField c : list) {
			if ((!(c instanceof ConsoleAction)) || (!((ConsoleAction)c).isHidden())) {
				result.add(toClientView(c,language));
			}
		}
		return result;
	}
	
	protected List<ConsoleProperty> getProperties() {
		ArrayList<ConsoleProperty> result = new ArrayList<ConsoleProperty>(list.size());
		for (ConsoleField c : list) {
			if (c instanceof ConsoleProperty) {
				result.add((ConsoleProperty)c);
			}
		}
		return result;
	}

	protected ConsoleAction getPublicAction(String id) {
		for (ConsoleField c : list) {
			if ((c instanceof ConsoleAction) && id.equalsIgnoreCase(((ConsoleAction)c).getId())) {
				ConsoleAction ca = ((ConsoleAction) c).clone();
				ca.setCode(null);
				ca.setHidden(false);
				return ca;
			}
		}
		return null;
	}

	protected ConsoleAction getAction(String id) {
		for (ConsoleField c :list) {
			if ((c instanceof ConsoleAction) && id.equalsIgnoreCase(((ConsoleAction)c).getId())) {
				return (ConsoleAction) c;
			}
		}
		return null;
	}
	
}
