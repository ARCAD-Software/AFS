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
package com.arcadsoftware.afs.framework.application.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.events.HelpListener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.PlatformUI;

import com.arcadsoftware.afs.framework.application.internal.Messages;

// Reprise de RestartWorkbench dans com.arcadsoftware.rcp.base.action
//<FM number="2017/00177" version="10.07.00" user="jbeauquis" date="june 2017" >
public class RestartWorkbenchAction implements IAction {

	public String getId() {
		return "com.arcadsoftware.afs.framework.application.conf.internal.action.RestartWorkbench";
	}

	public String getText() {
		return Messages.resString("menu.file.restart.text");
	}

	public String getToolTipText() {
		return Messages.resString("menu.file.restart.tooltip");
	}

	public void run() {
		PlatformUI.getWorkbench().restart();
	}

	public void runWithEvent(Event event) {
		run();
	}

	public boolean isEnabled() {
		return true;
	}

	public void addPropertyChangeListener(IPropertyChangeListener listener) {
	}

	public int getAccelerator() {
		return 0;
	}

	public String getActionDefinitionId() {
		return null;
	}

	public String getDescription() {
		return null;
	}

	public ImageDescriptor getDisabledImageDescriptor() {
		return null;
	}

	public HelpListener getHelpListener() {
		return null;
	}

	public ImageDescriptor getHoverImageDescriptor() {
		return null;
	}

	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	public IMenuCreator getMenuCreator() {
		return null;
	}

	public int getStyle() {
		return 0;
	}

	public boolean isChecked() {
		return false;
	}

	public boolean isHandled() {
		return false;
	}

	public void removePropertyChangeListener(IPropertyChangeListener listener) {
	}

	public void setActionDefinitionId(String id) {
	}

	public void setChecked(boolean checked) {
	}

	public void setDescription(String text) {
	}

	public void setDisabledImageDescriptor(ImageDescriptor newImage) {
	}

	public void setEnabled(boolean enabled) {
	}

	public void setHelpListener(HelpListener listener) {
	}

	public void setHoverImageDescriptor(ImageDescriptor newImage) {
	}

	public void setId(String id) {
	}

	public void setImageDescriptor(ImageDescriptor newImage) {
	}

	public void setMenuCreator(IMenuCreator creator) {
	}

	public void setText(String text) {
	}

	public void setToolTipText(String text) {
	}

	public void setAccelerator(int keycode) {
	}
}