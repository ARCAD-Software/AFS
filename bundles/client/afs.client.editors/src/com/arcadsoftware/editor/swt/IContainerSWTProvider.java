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
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * Provide a SWT container factory. Each call to <code>create</code> method should create a container and proceed to
 * layout this container according to the given parameters.
 */
public interface IContainerSWTProvider {

	/**
	 * Create the container. If this container must create widget itmust place them into the
	 * <code>renderer.getParent()</code> composite.
	 * <p>
	 * To create the sub-widget the container must call the <code>renderer.createSubContainer(...)</code> method.
	 *
	 * @param renderer
	 *            The SWT Renderer.
	 * @param parameters
	 *            the parameter list as declared in the extension point
	 * @param isEmpty
	 *            true if this container is empty.
	 * @param structure
	 *            The current entity structure informations.
	 */
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, boolean isEmpty, MetaDataEntity structure);

	/**
	 * Called to dispose the provider.
	 */
	public void dispose();
}
