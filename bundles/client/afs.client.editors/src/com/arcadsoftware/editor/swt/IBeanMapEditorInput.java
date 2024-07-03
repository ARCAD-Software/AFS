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
package com.arcadsoftware.editor.swt;

import org.eclipse.ui.IEditorInput;

/**
 *
 */
public interface IBeanMapEditorInput extends IEditorInput {

	/**
	 * @return the corresponding entity Type.
	 */
	public String getType();

	/**
	 * @return the corresponding Item Id.
	 */
	public int getId();

	/**
	 * @return The dynamic Editor Realm. (Default implementation should use "unified" realm or return null).
	 */
	public String getRealm();

	/**
	 * This method must return the Layout Name used to load the Layout Document.
	 * <p>
	 * This method is called during the <code>createPartControl</code> process it is not required until the editor is
	 * shown to the screen.
	 * <p>
	 * In the end, the "layout name" means the XML layout file name. So this name should be a file name or a url part as
	 * needed to be loaded by the
	 *
	 * @return a non null String.
	 */
	public String getLayoutName();

	public boolean isReadOnly();
}
