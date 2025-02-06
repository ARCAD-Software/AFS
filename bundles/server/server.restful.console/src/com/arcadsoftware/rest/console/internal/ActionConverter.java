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
package com.arcadsoftware.rest.console.internal;

import com.arcadsoftware.rest.console.ConsoleAction;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class ActionConverter implements Converter {

	public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
		return ConsoleAction.class.equals(arg0);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		ConsoleAction action = (ConsoleAction)source;
		if (action.getId() != null) {
			writer.addAttribute("id", action.getId()); //$NON-NLS-1$
		}
		if (action.getLabel() != null) {
			writer.addAttribute("label", action.getLabel()); //$NON-NLS-1$
		}
		if (action.getHelp() != null) {
			writer.addAttribute("help", action.getHelp()); //$NON-NLS-1$
		}
		if (action.getIcon() > 0) {
			writer.addAttribute("icon", Integer.toString(action.getIcon())); //$NON-NLS-1$
		}
		if (action.isHidden()) {
			writer.addAttribute("hidden", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (action.getCode() != null) {
			writer.setValue(action.getCode());
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String id = reader.getAttribute("id"); //$NON-NLS-1$
		String label = reader.getAttribute("label"); //$NON-NLS-1$
		String help = reader.getAttribute("help"); //$NON-NLS-1$
		boolean hidden = Boolean.parseBoolean(reader.getAttribute("hidden")); //$NON-NLS-1$
		int icon = 0;
		try {
			icon = Integer.parseInt(reader.getAttribute("icon")); //$NON-NLS-1$
		} catch (NumberFormatException e) {}
		return new ConsoleAction(id, reader.getValue(), label, icon, hidden, help);
	}

}
