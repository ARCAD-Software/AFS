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
package com.arcadsoftware.editor.implementation.swt.renderer;

import org.eclipse.jface.action.Action;

import com.arcadsoftware.editor.IActionElement;

/**
 *
 */
public class SWTScriptAction extends Action {

	private final SWTRenderer renderer;
	private final IActionElement action;

	/**
	 * @param renderer
	 * @param action
	 */
	public SWTScriptAction(SWTRenderer renderer, IActionElement action) {
		super(action.getName(), renderer.getImageDescriptor(action.getIcon()));
		this.renderer = renderer;
		this.action = action;
	}

	@Override
	public void run() {
		renderer.getRendererBinding().execute(action.getElements(), action.getScript());
	}

}
