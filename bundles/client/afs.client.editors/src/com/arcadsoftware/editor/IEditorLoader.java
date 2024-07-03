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
package com.arcadsoftware.editor;

import java.util.Properties;

import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This interface describe the Extension point used to load the Layout and the Data structure.
 * <p>
 * This interface is used in all the use case of the Dynamic Editor Engine and is not linked to a particular
 * implementation (SWT, Ajax, ...)
 */
public interface IEditorLoader {

	public static final int LAYOUT_KIND_ANY = 0;
	public static final int LAYOUT_KIND_SWT = 1;
	public static final int LAYOUT_KIND_AJAX = 2;

	/**
	 * Load a layout document. This document is an XML fragment. This XML format is dependent from the editor
	 * capabilities so it is not parsed by the loader and just a string is returned.
	 *
	 * @param name
	 *            unique name of this layout document.
	 * @param type
	 *            is the entity type, a same layout name may be used for different entity types.
	 * @param kind
	 *            define the editor type, zero select any type of layout.
	 * @return an XML representation.
	 */
	public String loadXMLLayoutDocument(String name, String type, int kind);

	/**
	 * Load an Entity structure definition. This definition can be different from the entity linked currently edited.
	 *
	 * @param type
	 *            the entity type.
	 * @return the entity structure bean.
	 */
	public MetaDataEntity loadMetaDataEntity(String type);

	/**
	 * Load a properties file from the current realm.
	 *
	 * @param name
	 *            the key identifying the properties resource bundle.
	 * @return The Properties list.
	 */
	public Properties loadProperties(String name);
}
