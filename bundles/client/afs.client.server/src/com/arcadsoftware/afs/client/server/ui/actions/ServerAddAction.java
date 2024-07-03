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
package com.arcadsoftware.afs.client.server.ui.actions;

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.aev.core.ui.actions.AbstractSimpleItemWithWizardAction;
import com.arcadsoftware.aev.core.ui.wizards.AbstractSimpleItemWizard;
import com.arcadsoftware.aev.core.ui.wizards.AbstractSimpleItemWizardPage;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.internals.ui.wizards.ServerAddMainWizardPage;
import com.arcadsoftware.afs.client.server.internals.ui.wizards.ServerAddWizard;

public class ServerAddAction extends AbstractSimpleItemWithWizardAction {

	private ServerAddMainWizardPage page;
	private Server addedServer;

	public ServerAddAction() {
		super();
		setActionsToRunAfterWizard(false);
	}

	@Override
	public AbstractSimpleItemWizard createWizard(
			AbstractSimpleItemWithWizardAction action, String title) {
		return new ServerAddWizard(action, title);
	}

	@Override
	public AbstractSimpleItemWizardPage[] getPages() {
		page = new ServerAddMainWizardPage("main", //$NON-NLS-1$
				Activator.resString("server.wizard.add.page.title"), //$NON-NLS-1$
				Activator.resString("server.wizard.add.page.description")); //$NON-NLS-1$
		return new AbstractSimpleItemWizardPage[] { page };
	}

	@Override
	public String getWizardTitle() {
		return Activator.resString("server.wizard.add.title");//$NON-NLS-1$
	}

	@Override
	public boolean runActions() {
		addedServer = new Server();
		addedServer.setName(page.getServerName());
		addedServer.setUrl(page.getUrl());
		return registerServer(addedServer);
	}

	protected boolean registerServer(Server addedServer) {
		return ServerLoader.getInstance().add(addedServer);
	}

	@Override
	public ArcadEntity getItem() {
		return null;
	}

	public Server getAddedServer() {
		return addedServer;
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("server.action.add.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("server.action.add.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.ADD.imageDescriptor());
	}
}
