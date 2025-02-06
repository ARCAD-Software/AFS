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
package com.arcadsoftware.rest.console;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.io.HierarchicalStreamDriver;

public class XmlConsoleStream extends XStreamCompact {

	public XmlConsoleStream() {
		super(XmlConsoleStream.class.getClassLoader());
		initialize();
	}

	public XmlConsoleStream(ClassLoader classLoader) {
		super(classLoader);
		initialize();
	}

	public XmlConsoleStream(ClassLoader classLoader,
			HierarchicalStreamDriver compactDriver) {
		super(classLoader, compactDriver);
		initialize();
	}

	private void initialize() {
		alias("categories", ArrayList.class); //$NON-NLS-1$
		alias("category", Category.class); //$NON-NLS-1$
		useAttributeFor(Category.class, "label"); //$NON-NLS-1$
		addImplicitCollection(Category.class, "list"); //$NON-NLS-1$
		alias("section", SectionId.class); //$NON-NLS-1$
		useAttributeFor(SectionId.class, "label"); //$NON-NLS-1$
		useAttributeFor(SectionId.class, "id"); //$NON-NLS-1$
		useAttributeFor(SectionId.class, "icon"); //$NON-NLS-1$
		useAttributeFor(SectionId.class, "help"); //$NON-NLS-1$
		alias("action", ConsoleAction.class); //$NON-NLS-1$
		useAttributeFor(ConsoleAction.class, "id"); //$NON-NLS-1$
		useAttributeFor(ConsoleAction.class, "hidden"); //$NON-NLS-1$
		useAttributeFor(ConsoleAction.class, "recall"); //$NON-NLS-1$
		alias("set", ConsoleSet.class); //$NON-NLS-1$
		useAttributeFor(ConsoleSet.class, "anchor"); //$NON-NLS-1$
		alias("text", ConsoleText.class); //$NON-NLS-1$
		alias("property", ConsoleProperty.class); //$NON-NLS-1$
		addImplicitCollection(ConsoleProperty.class, "list"); //$NON-NLS-1$
		useAttributeFor(ConsoleProperty.class, "id"); //$NON-NLS-1$
		useAttributeFor(ConsoleProperty.class, "password"); //$NON-NLS-1$
		useAttributeFor(ConsoleProperty.class, "defaultvalue"); //$NON-NLS-1$
		useAttributeFor(ConsoleProperty.class, "readonly"); //$NON-NLS-1$
		useAttributeFor(ConsoleProperty.class, "hidden"); //$NON-NLS-1$
		useAttributeFor(ConsoleProperty.class, "type"); //$NON-NLS-1$
		useAttributeFor(ConsoleField.class, "help"); //$NON-NLS-1$
		useAttributeFor(ConsoleField.class, "icon"); //$NON-NLS-1$
		useAttributeFor(ConsoleField.class, "label"); //$NON-NLS-1$
		// Console Message mapping
		alias("message", ConsoleMessage.class); //$NON-NLS-1$
		useAttributeFor(ConsoleMessage.class, "title"); //$NON-NLS-1$
		useAttributeFor(ConsoleMessage.class, "messageType"); //$NON-NLS-1$
		// Console Properties mapping
		aliasAttribute("default", "defaultvalue"); //$NON-NLS-1$ //$NON-NLS-2$
		useAttributeFor(ConsoleProperty.class, "id"); //$NON-NLS-1$
		aliasAttribute("sid", "id"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<Category> getCategories(String xml) {
		return (List)fromXML(xml);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<ConsoleField> getSectionForm(String xml) {
		if ((xml == null) || (xml.length() == 0)) {
			return null;
		}
		return (List)fromXML(xml);
	}

}
