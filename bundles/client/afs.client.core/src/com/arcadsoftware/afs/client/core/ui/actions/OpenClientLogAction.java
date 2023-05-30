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
package com.arcadsoftware.afs.client.core.ui.actions;

import java.io.File;

import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.file.FileManager;
import com.arcadsoftware.afs.client.core.internal.Activator;

public class OpenClientLogAction extends ArcadAction {

	public OpenClientLogAction() {
		super(
			Activator.resString("open.client.log.action.text"),
			Activator.resString("open.client.log.action.tooltip"),
			AFSIcon.LOG.imageDescriptor()
		);
	}
	
	@Override
	public void run() {
		final File logFile = Platform.getLogFileLocation().toFile();
		if(logFile.exists()) {	
			FileManager.openFile(logFile, null, Activator.resString("open.client.log.editor.title"));
		}
	}
}
