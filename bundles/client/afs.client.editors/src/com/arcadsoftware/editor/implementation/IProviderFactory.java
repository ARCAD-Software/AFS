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
package com.arcadsoftware.editor.implementation;

import java.io.IOException;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.script.IScriptAction;

/**
 * @author ARCAD Software
 */
public interface IProviderFactory {

	public LayoutElement getInput(String name, String type, Element element, XmlPullParser xpp)
			throws XmlPullParserException, IOException;

	public LayoutElement getDecorator(String name, XmlPullParser xpp) throws XmlPullParserException, IOException;

	public LayoutElement getContainer(EditorEngine editorEngine, String name, XmlPullParser xpp)
			throws XmlPullParserException, IOException;

	/**
	 * Load a IScriptAction defined into the platform.
	 *
	 * @param name
	 * @return
	 */
	public IScriptAction getScriptAction(String name);
}
