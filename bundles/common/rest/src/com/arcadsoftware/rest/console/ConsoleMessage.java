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
package com.arcadsoftware.rest.console;

public class ConsoleMessage extends ConsoleField {
	
	public static final int ERROR = 0;
	public static final  int INFO = 1;
	public static final  int WARNING = 2;
	public static final  int DEBUG = 3;
	
	private String title = ""; //$NON-NLS-1$
	private int messageType = INFO;
	
	public ConsoleMessage() {
		super();
	}
	
	public ConsoleMessage(String title, String message, int icon, String help) {
		super();
		setLabel(message);
		setIcon(icon);
		setHelp(help);
		this.title = title;
	}

	public ConsoleMessage(String title, String message, int icon) {
		this(title,message);
		setIcon(icon);
	}

	public ConsoleMessage(String title, String message,  String help) {
		this(title,message);
		setHelp(help);		
	}

	public ConsoleMessage(String title, String message) {
		super();
		setLabel(message);
	}

	public int getMessageType() {
		return messageType;
	}

	public void setMessageType(int messageType) {
		this.messageType = messageType;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@Override
	public ConsoleMessage clone() {
		return new ConsoleMessage(getTitle(),getLabel(), getIcon(), getHelp());
	}
}
