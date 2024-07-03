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

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This interface implement a SWT Widget provider for the dynamic editors.
 * <p>
 * Theses provider are in charge of instantiate and disposing the widget when asked for.
 */
public interface IInputSWTProvider {

	/**
	 * Build the editor widget and optional secondary ones.
	 *
	 * @param renderer
	 *            The SWT Renderer.
	 * @param parameters
	 *            the parameter list as declared in the extension point
	 * @param element
	 *            the binded attribute or link that can be used to bind the corresponding widget with.
	 * @param structure
	 *            The current entity structure informations.
	 */
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity structure);

	/**
	 * Called to dispose the provider.
	 */
	public void dispose();
}
