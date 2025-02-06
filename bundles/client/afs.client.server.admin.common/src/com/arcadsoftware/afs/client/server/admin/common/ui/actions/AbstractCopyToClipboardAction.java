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
package com.arcadsoftware.afs.client.server.admin.common.ui.actions;

import org.eclipse.jface.action.Action;

import com.arcadsoftware.afs.client.server.admin.common.Activator;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers.ICopyContentProvider;

public class AbstractCopyToClipboardAction extends Action {

	private ICopyContentProvider contentProvider = null;

	public AbstractCopyToClipboardAction() {
		setText(Activator.resString("settingseditor.actions.copy.text")); //$NON-NLS-1$
	}

	public void setContentProvider(ICopyContentProvider provider) {
		contentProvider = provider;
	}

	public String getContent() {
		if (contentProvider != null) {
			return contentProvider.getContentToCopy();
		} else {
			return ""; //$NON-NLS-1$
		}
	}
}
