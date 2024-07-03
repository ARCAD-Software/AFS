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

import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.swt.renderer.IRendererBinding;

/**
 * A BeanMap Container is a graphical object that is bind to a BeanMap value.
 * <p>
 * This interface is not intended to be instantiated. You should use one of the following interfaces.
 *
 * @see IBeanMapContainerLinkedList
 * @see IBeanMapContainerValue
 * @see IBeanMapContainerList
 * @see IRendererBinding#bindElement(com.arcadsoftware.metadata.Element, IBeanMapContainer)
 */
public interface IBeanMapContainer {

	/**
	 * Get an optional SWT object associated with this container.
	 * <p>
	 * If not null and is an instance of Control then this object will be automatically added to the management of user
	 * messages.
	 *
	 * @return The SWT Widget. Can be null.
	 */
	public Widget getWidget();

}
