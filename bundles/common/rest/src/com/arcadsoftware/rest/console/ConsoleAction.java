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
 * An action define a menu option or a button into the form used to give to the user the opportunity to run a process. 
 * 
 * 
 */
public class ConsoleAction extends ConsoleField {

	private String id;
	private String code;
	private boolean hidden;
	private int recall;

	public ConsoleAction() {
		super();
	}

	public ConsoleAction(String id, String label) {
		this();
		this.id = id;
		this.code = ""; //$NON-NLS-1$
		setLabel(label);
	}

	public ConsoleAction(String id, String code, String label) {
		this();
		this.id = id;
		this.code = code;
		setLabel(label);
	}

	public ConsoleAction(String id, String code, String label, int icon, boolean hidden, String help) {
		this(id, code, label);
		this.id = id;
		this.code = code;
		setLabel(label);
	}

	public ConsoleAction(String id, String code, String label, int icon, boolean hidden, String help, int recall) {
		this(id, code, label, icon, hidden, help);
		this.recall = recall;
	}

	public String getId() {
		return id;
	}

	public String getCode() {
		return code;
	}

	public void setId(String id) {
		this.id = id;
	}

	public void setCode(String code) {
		this.code = code;
	}

	/**
	 * Determine if this action should be shown to the user or not.
	 * 
	 * @param hidden
	 *            the hidden to set
	 */
	public void setHidden(boolean hidden) {
		this.hidden = hidden;
	}

	/**
	 * State if this action should be shown to the user or not.
	 * 
	 * <p>Hidden actions are reserver to the Client interface. some technical operation use this feature to run
	 * specific operation like delayed actions or progressive form loading.
	 * 
	 * @return the hidden
	 */
	public boolean isHidden() {
		return hidden;
	}

	@Override
	public ConsoleAction clone() {
		return new ConsoleAction(id, code, getLabel(), getIcon(), hidden, getHelp());
	}

	/**
	 * Define the delay (in seconds) used to recall this action. 
	 * 
	 * @param recall The recall delay to set
	 */
	public void setRecall(int recall) {
		this.recall = recall;
	}

	/**
	 * The recal delay define the time, in seconds, in witch this action can be thrown.
	 * <p>For an hidden action the delay define the period for next call.
	 * @return the recall delay in seconds.
	 */
	public int getRecall() {
		return recall;
	}
	
}
