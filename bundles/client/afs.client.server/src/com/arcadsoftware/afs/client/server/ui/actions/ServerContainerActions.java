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
package com.arcadsoftware.afs.client.server.ui.actions;

import org.eclipse.jface.resource.ImageDescriptor;

import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.afs.client.server.ui.containers.ServerContainer;
import com.arcadsoftware.afs.framework.ui.containers.ContainerEntryActions;

public class ServerContainerActions extends ContainerEntryActions {

	public ServerContainerActions() {
		super();
	}

	public ServerContainerActions(Container container) {
		super(container, true);
	}

	@Override
	public void makeAction() {
		final ServerAddAction addAction = new ServerAddAction() {

			@Override
			protected void setInterface() {
				super.setInterface();
				final ServerContainer sc = (ServerContainer) getContainer();

				final String text = sc.getAddServerText();
				final String tooltip = sc.getAddServerTooltip();
				final ImageDescriptor image = sc.getAddServerImage();
				if (text != null) {
					setText(text);
				}
				if (tooltip != null) {
					setToolTipText(tooltip);
				}
				if (image != null) {
					setImageDescriptor(image);
				}
			}

			@Override
			protected void doAfterRun() {
				if (runOk) {
					final ServerContainer sc = (ServerContainer) getContainer();
					if (sc != null) {
						sc.addServer(getAddedServer());
						sc.getViewer().refresh(sc);
					}
				}
			}
		};

		addAction(addAction);
	}

}
