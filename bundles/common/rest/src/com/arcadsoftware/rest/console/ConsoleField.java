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

/**
 * An element of a console section.
 * 
 * @author ARCAD Software
 */
public abstract class ConsoleField implements Cloneable {

	public static final int ICON_NONE = 0;
	public static final int ICON_OK = 1;
	public static final int ICON_CANCEL = 2;
	public static final int ICON_REFRESH = 3;
	public static final int ICON_SHUFFLE = 4;
	public static final int ICON_TRANSFER = 5;
	public static final int ICON_FOLDER_CLOSE = 6;
	public static final int ICON_FOLDER_OPEN = 7;
	public static final int ICON_DOCUMENT = 8;
	public static final int ICON_NOTE = 9;
	public static final int ICON_MAIL = 10;
	public static final int ICON_SUITCASE = 11;
	public static final int ICON_COMMENT = 12;
	public static final int ICON_USER = 13;
	public static final int ICON_PRINT = 14;
	public static final int ICON_TRASH = 15;
	public static final int ICON_LOCKED = 16;
	public static final int ICON_UNLOCKED = 17;
	public static final int ICON_BOOKMARK = 18;
	public static final int ICON_TAG = 19;
	public static final int ICON_HOME = 20;
	public static final int ICON_CALENDAR = 21;
	public static final int ICON_CART = 22;
	public static final int ICON_PENCIL = 23;
	public static final int ICON_CLOCK = 24;
	public static final int ICON_DISK = 25;
	public static final int ICON_CALCUILATOR = 26;
	public static final int ICON_SEARCH = 27;
	public static final int ICON_WRENCH = 28;
	public static final int ICON_GEAR = 29;
	public static final int ICON_HEART = 30;
	public static final int ICON_STAR = 31;
	public static final int ICON_LINK = 32;
	public static final int ICON_PLUS = 33;
	public static final int ICON_MINUS = 34;
	public static final int ICON_CLOSE = 35;
	public static final int ICON_KEY = 36;
	public static final int ICON_CUT = 37;
	public static final int ICON_PASTE = 38;
	public static final int ICON_COPY = 39;
	public static final int ICON_CONTACT = 40;
	public static final int ICON_IMAGE = 41;
	public static final int ICON_VIDEO = 42;
	public static final int ICON_SCRIPT = 43;
	// Messages Boxes...
	public static final int ICON_ERROR = 44;
	public static final int ICON_INFO = 45;
	public static final int ICON_NOTICE = 46;
	public static final int ICON_HELP = 47;
	public static final int ICON_WARN = 48;
	// Execution console...
	public static final int ICON_RUN = 49;
	public static final int ICON_PAUSE = 50;
	public static final int ICON_STOP = 51;
	public static final int ICON_EJECT = 52;
	public static final int ICON_POWER = 53;
	public static final int ICON_DIAGNOSTIC = 54;
	public static final int ICON_SIGNAL = 55;
	// Progress bar...
	public static final int ICON_PROGRESS_0 = 56;
	public static final int ICON_PROGRESS_1 = 57;
	public static final int ICON_PROGRESS_2 = 58;
	public static final int ICON_PROGRESS_3 = 59;
	// Trademarks...
	public static final int ICON_ARCAD = 60;
	public static final int ICON_IBM = 61;
	public static final int ICON_JAVA = 62;
	public static final int ICON_APACHE = 63;
	public static final int ICON_NOVELL = 64;
	public static final int ICON_BUNDLE = 65;
	public static final int ICON_ECLIPSE = 66;
	public static final int ICON_OPS4J = 67;
	public static final int ICON_RESTLET = 68;
	public static final int ICON_SPRING = 69;
	//public static final int ICON_ = ;
	
	private String label;
	private int icon;
	private String help;

	/**
	 * @param label the label to set
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * @return the label
	 */
	public String getLabel() {
		return label;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	/**
	 * @param icon the icon to set
	 */
	public void setIcon(int icon) {
		this.icon = icon;
	}

	/**
	 * @return the icon
	 */
	public int getIcon() {
		return icon;
	}

	/**
	 * @param help the help to set
	 */
	public void setHelp(String help) {
		this.help = help;
	}

	/**
	 * @return the help
	 */
	public String getHelp() {
		return help;
	}

}
