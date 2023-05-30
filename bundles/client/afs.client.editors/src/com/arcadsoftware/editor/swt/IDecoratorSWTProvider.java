/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * Provide and SWT decorator provider.
 * 
 * <p> Decorators are graphical object that are not specifically linked to an particular 
 * attribute or link, and can not contains sub elements. By the way they can be what ever
 * it is necessary. 
 */
public interface IDecoratorSWTProvider {

	/**
	 * Create the decorator object. Use the interface <code>renderer</code> to add
	 * any behavior to the built widgets. Your are free to examine the entity <code>structure</code>
	 * to known which is the binded BeanMap, but must not make any assumption about its contains.
	 * 
	 * @param renderer The SWT Renderer.
	 * @param parameters the parameter list as declared in the extension point
	 * @param structure The current entity structure informations.
	 */
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure);

	/**
	 * Called to dispose the provider.
	 */
	public void dispose();
}
