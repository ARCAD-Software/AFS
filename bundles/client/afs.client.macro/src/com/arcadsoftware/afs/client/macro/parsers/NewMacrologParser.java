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
package com.arcadsoftware.afs.client.macro.parsers;

import java.io.IOException;
import java.io.Reader;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

import com.arcadsoftware.afs.client.macro.internal.Activator;
import com.arcadsoftware.afs.client.macro.model.IMacrologParser;
import com.arcadsoftware.afs.client.macro.model.MacroLogItemDefinition;
import com.arcadsoftware.afs.client.macro.model.MacroLogItemDefinitions;

public class NewMacrologParser implements IMacrologParser {

	private static final String ITEM_MACRO = "macroCommand"; //$NON-NLS-1$
	private static final String ITEM_MACLOGS = "macroLogs";//$NON-NLS-1$
	private static final String ITEM_LOG = "macroLog";//$NON-NLS-1$

	public static final String ATTR_MSGID = "id";//$NON-NLS-1$
	public static final String ATTR_MSGTYPE = "type";//$NON-NLS-1$
	public static final String ATTR_MSGSTS = "status";//$NON-NLS-1$
	public static final String ATTR_MSGSEVERITY = "severity";//$NON-NLS-1$
	public static final String ATTR_MSGSEQ = "sequence";//$NON-NLS-1$
	public static final String ATTR_MSGLEVEL1 = "level1";//$NON-NLS-1$
	public static final String ATTR_MSGLEVEL2 = "level2";//$NON-NLS-1$

	public static final String TAG_ITEM = "item";//$NON-NLS-1$

	@Override
	public void parse(Reader reader, MacroLogItemDefinitions macros) {
		MacroLogItemDefinition def = null;
		String itemType = ""; //$NON-NLS-1$
		// MXParser parser = new MXParser();

		try {
			final XmlPullParserFactory factory = XmlPullParserFactory.newInstance();
			factory.setNamespaceAware(true);
			final XmlPullParser parser = factory.newPullParser();
			parser.setInput(reader);
			int eventType = parser.getEventType();
			do {
				if (eventType == XmlPullParser.START_TAG) {
					final String tag = parser.getName();
					if (tag.equals(ITEM_MACLOGS)) {
						itemType = tag;
					}

					if (tag.equals(ITEM_MACRO)) {
						if (parser.getAttributeCount() == 3) {
							final String macroLibrary = parser.getAttributeValue(0).trim();
							final String macroName = parser.getAttributeValue(1).trim();
							macros.setMacrolibrary(macroLibrary);
							macros.setMacroname(macroName);
						}
					}

					if (tag.equals(ITEM_LOG)) {
						final int count = 5;
						if (parser.getAttributeCount() == count) {
							final String id = parser.getAttributeValue(0).trim();
							final String sequence = parser.getAttributeValue(1);
							final String severity = parser.getAttributeValue(2).trim();
							final String status = parser.getAttributeValue(3).trim();
							final String type = parser.getAttributeValue(4).trim();
							def = new MacroLogItemDefinition();
							def.setItemType(itemType);
							def.setId(id);
							def.setType(type);
							def.setStatus(status);
							def.setSeverity(severity);
							def.setSequence(sequence);
							macros.add(def);
						}
					}
					if (tag.equals(ATTR_MSGLEVEL1)) {
						if (def != null) {
							eventType = parser.next();
							if (eventType != XmlPullParser.END_TAG) {
								def.setLevel1(parser.getText());
							}
						}
					}
					if (tag.equals(ATTR_MSGLEVEL2)) {
						if (def != null) {
							eventType = parser.next();
							if (eventType != XmlPullParser.END_TAG) {
								def.setLevel2(parser.getText());
							}
						}
					}
				}
				eventType = parser.next();
			} while (eventType != XmlPullParser.END_DOCUMENT);
		} catch (final XmlPullParserException e) {
			Activator.getDefault().debug(e);
		} catch (final IOException e1) {
			Activator.getDefault().debug(e1);
		}
	}

}
