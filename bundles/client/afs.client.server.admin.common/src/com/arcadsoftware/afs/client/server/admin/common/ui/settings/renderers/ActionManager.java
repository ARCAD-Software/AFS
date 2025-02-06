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
package com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers;

import java.util.HashMap;
import java.util.Map;

import com.arcadsoftware.afs.client.server.admin.common.Activator;
import com.arcadsoftware.afs.client.server.admin.common.core.model.ConsoleConnector;
import com.arcadsoftware.afs.client.server.admin.common.core.model.Form;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.editors.SectionSettingEditorInput;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleMessage;
import com.arcadsoftware.rest.console.SectionId;

public class ActionManager {

	private ConsoleConnector connection;
	private SectionId sectionId;
	private ConsoleAction action;
	private Map<String, Object> values;
	private Form form;

	public ActionManager(ConsoleConnector connection, SectionId sectionId) {
		this.connection = connection;
		this.sectionId = sectionId;
		values = new HashMap<>();
	}

	public ActionManager(ConsoleConnector connection, SectionId sectionId, ConsoleAction action) {
		this(connection, sectionId);
		this.action = action;
	}

	public ConsoleConnector getConnector() {
		return connection;
	}

	public void setConnection(ConsoleConnector connection) {
		this.connection = connection;
	}

	public SectionId getSectionId() {
		return sectionId;
	}

	public void setSectionId(SectionId sectionId) {
		this.sectionId = sectionId;
	}

	public Map<String, Object> getValues() {
		return values;
	}

	public ConsoleAction getAction() {
		return action;
	}

	public void setAction(ConsoleAction action) {
		this.action = action;
	}

	public void setValues(Map<String, Object> values) {
		this.values = values;
	}

	public void setForm(Form form) {
		this.form = form;
	}

	private void showMessage(ConsoleMessage message) {
		final int level = message.getMessageType();
		switch (level) {
		case ConsoleMessage.ERROR:
			Activator.getInstance().openError(message.getTitle(), message.getLabel());
			break;
		case ConsoleMessage.INFO:
		case ConsoleMessage.DEBUG:
			Activator.getInstance().openInformation(message.getTitle(), message.getLabel());
			break;
		case ConsoleMessage.WARNING:
			Activator.getInstance().openWarning(message.getTitle(), message.getLabel());
			break;
		default:
			break;
		}
	}

	public Form getForm() {
		return form;
	}

	public boolean execute() {
		final ConsoleConnector cnx = getConnector();
		final SectionId section = getSectionId();
		if (action == null) {
			form = cnx.getSection(section);
		} else {
			form = connection.performAction(sectionId, action.getId(), values);
		}
		if (form != null) {
			if (form.isMessageOnly()) {
				showMessage((ConsoleMessage) form.getFields().get(0));
			} else if (form.getFields().size() > 0) {
				SectionSettingEditorInput.openEditor(new SectionSettingEditorInput(this));
			}
		} else {
			SectionSettingEditorInput.openEditor(new SectionSettingEditorInput(this));
		}
		return true;
	}

	public ActionManager clone(boolean withValues) {
		final ActionManager result = new ActionManager(getConnector(), getSectionId());
		if (withValues) {
			result.getValues().putAll(values);
		}
		return result;
	}

}
