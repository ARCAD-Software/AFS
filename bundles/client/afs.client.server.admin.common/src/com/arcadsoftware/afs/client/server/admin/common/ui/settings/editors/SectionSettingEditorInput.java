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
package com.arcadsoftware.afs.client.server.admin.common.ui.settings.editors;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.PartInitException;

import com.arcadsoftware.afs.client.server.admin.common.Activator;
import com.arcadsoftware.afs.client.server.admin.common.core.model.ConsoleConnector;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers.ActionManager;

public class SectionSettingEditorInput implements IEditorInput {

	private final ActionManager actionManager;

	public SectionSettingEditorInput(ActionManager actionManager) {
		this.actionManager = actionManager;
	}

	public ConsoleConnector getConnector() {
		return actionManager.getConnector();
	}

	public ActionManager getActionManager() {
		return actionManager;
	}

	@Override
	public <T> T getAdapter(Class<T> adapter) {
		return null;
	}

	@Override
	public boolean exists() {
		return true;
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	@Override
	public String getName() {
		return actionManager.getSectionId().getLabel();
	}

	@Override
	public IPersistableElement getPersistable() {
		return null;
	}

	@Override
	public String getToolTipText() {
		final String h = actionManager.getSectionId().getHelp();
		if (h == null) {
			return ""; //$NON-NLS-1$
		}
		return h;
	}

	/**
	 * Ouverture de l'éditeur associé à un input.
	 *
	 * @param input
	 * @return
	 */
	public static SectionSettingEditor openEditor(SectionSettingEditorInput input) {
		if (input.getConnector().getServerConnection().isConnected()) {
			try {
				final IEditorPart editor = Activator.getInstance()
						.getWorkbench()
						.getActiveWorkbenchWindow()
						.getActivePage()
						.openEditor(input, SectionSettingEditor.ID);
				if (editor instanceof SectionSettingEditor) {
					return (SectionSettingEditor) editor;
				}
				return null;
			} catch (final PartInitException e) {
				Activator.getInstance().log(Activator.LOGLVL_FATAL, e.getLocalizedMessage(), e);
				return null;
			}
		}
		return null;
	}

}
