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

/**
 * This interface can be implemented by <code>IContainerSWTProvider</code> to 
 * test the sub element before they are included into the content.
 */
public interface IValidatingSubWidgets {

	/**
	 * Test an input provider. 
	 * 
	 * @param provider the input provider.
	 * @return True if the given provider can be added.
	 */
	public boolean acceptInput(IInputSWTProvider provider);

	/**
	 * Test a container provider. 
	 * 
	 * @param provider the container provider.
	 * @return True if the given provider can be added.
	 */
	public boolean acceptSubContainer(IContainerSWTProvider provider);

	/**
	 * Test an decoration provider. 
	 * 
	 * @param provider the decoration provider.
	 * @return True if the given provider can be added.
	 */
	public boolean acceptDecorator(IDecoratorSWTProvider provider);
}
