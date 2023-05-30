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
package com.arcadsoftware.editor.implementation;

import com.arcadsoftware.editor.IActionElement;

public class ActionElement implements IActionElement {

	private String code;
	private String name;
	private String icon;
	private String script;
	private String[] elements;

	/* (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.IActionElement#getName()
	 */
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	/* (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.IActionElement#getIcon()
	 */
	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	/* (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.IActionElement#getScript()
	 */
	public String getScript() {
		return script;
	}

	public void setScript(String script) {
		this.script = script;
	}

	public void setCode(String code) {
		this.code = code;
	}

	/* (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.IActionElement#getCode()
	 */
	public String getCode() {
		return code;
	}

	public void setElements(String els) {
		elements = els.split(" "); //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.editor.IActionElement#getElements()
	 */
	public String[] getElements() {
		return elements;
	}

}
