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
package com.arcadsoftware.rest.console;

/**
 * Console Section widget that represent a Text information.
 */
public class ConsoleText extends ConsoleField {

	public ConsoleText() {
		super();
	}
	
	public ConsoleText(String label, int icon, String help) {
		super();
		setLabel(label);
		setIcon(icon);
		setHelp(help);
	}

	public ConsoleText(String label, int icon) {
		this(label);
		setIcon(icon);
	}

	public ConsoleText(String label, String help) {
		this(label);
		setHelp(help);
	}

	public ConsoleText(String label) {
		super();
		setLabel(label);
	}

	@Override
	public ConsoleText clone() {
		return new ConsoleText(getLabel(), getIcon(), getHelp());
	}
	
}
