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
package com.arcadsoftware.editor;

/**
 * An Action is an small dynamic program that can perform come operations depending on the renderer capabilities. they
 * can be used as macro-commands.
 * <p>
 * Theses Actions can be attached to the Editor or to a Container.
 */
public interface IActionElement {

	/**
	 * @return The identity code of this Action.
	 */
	public String getCode();

	/**
	 * @return The localized action name.
	 */
	public String getName();

	/**
	 * @return The javascript executed.
	 */
	public String getScript();

	/**
	 * @return The icon image name.
	 */
	public String getIcon();

	/**
	 * @return The list impacted elements (Attributes and Link that must be accessible to this action script).
	 */
	public String[] getElements();
}
