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
package com.arcadsoftware.afs.client.server.ui.views;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.viewers.columned.ColumnedActionSeparator;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.core.servers.model.Servers;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.actions.ServerAddAction;
import com.arcadsoftware.afs.client.server.ui.actions.ServerDeleteAction;
import com.arcadsoftware.afs.client.server.ui.actions.ServerManageAction;
import com.arcadsoftware.afs.client.server.ui.actions.ServerPropertiesAction;
import com.arcadsoftware.afs.client.server.ui.viewers.ServerTableViewer;
import com.arcadsoftware.afs.framework.ui.views.AbstractAFSView;

public class ServerView extends AbstractAFSView {

	public static final String ID = "com.arcadsoftware.afs.client.server.ui.views.ServerView"; //$NON-NLS-1$

	ServerAddAction addAction;
	ServerDeleteAction deleteAction;
	ServerManageAction manageAction;
	ServerPropertiesAction propertiesAction;
	Action refreshAction;
	private Servers servers;

	ServerTableViewer serverTableViewer;

	public ServerView() {
		super();
		servers = ServerLoader.getInstance().load();
		defineActions();
	}

	@Override
	public void createPartControl(final Composite parent) {
		serverTableViewer = new ServerTableViewer(parent, SWT.BORDER | SWT.FULL_SELECTION) {
			@Override
			protected void doOnDoubleClick(final IStructuredSelection selection) {
				final Server selectedServer = getServerFromSelection(selection);
				if (selectedServer != null) {
					manageAction.run();
				}
			}

			@Override
			protected Action[] getViewerActions() {
				return ServerView.this.getViewerActions();
			}

		};
		serverTableViewer.setInput(servers);
		super.createPartControl(parent);
	}

	@Override
	protected void defineActions() {

		addAction = new ServerAddAction() {
			@Override
			protected void doAfterRun() {
				if (isRunOk()) {
					final Server s = getAddedServer();
					if (s != null) {
						servers.add(s);
						serverTableViewer.refresh();
					}
				}
			}
		};
		deleteAction = new ServerDeleteAction() {
			@Override
			protected void doAfterRun() {
				if (isRunOk()) {
					final Server s = serverTableViewer.getSelectedServer();
					servers.remove(s);
					serverTableViewer.refresh();
				}
			}

			@Override
			protected Server getServerToManage() {
				return serverTableViewer.getSelectedServer();
			}
		};
		propertiesAction = new ServerPropertiesAction() {
			@Override
			protected void doAfterRun() {
				if (isRunOk()) {
					serverTableViewer.refresh();
				}
			}

			@Override
			protected Server getServerToManage() {
				return serverTableViewer.getSelectedServer();
			}
		};
		manageAction = new ServerManageAction() {
			@Override
			protected Server getServerToManage() {
				return serverTableViewer.getSelectedServer();
			}

			@Override
			public boolean manageUser() {
				return ServerView.this.manageUser();
			}
		};
		refreshAction = new Action() {
			@Override
			public void run() {
				servers = ServerLoader.getInstance().load();
				serverTableViewer.setInput(servers);
			}
		};
		refreshAction.setText(Activator.resString("server.action.refresh.text")); //$NON-NLS-1$
		refreshAction.setToolTipText(Activator.resString("server.action.refresh.tooltip")); //$NON-NLS-1$
		refreshAction.setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
	}

	@Override
	protected void fillLocalToolBar(final IToolBarManager manager) {
		manager.add(addAction);
		manager.add(new Separator());
		manager.add(refreshAction);
	}

	public Action[] getViewerActions() {
		return new Action[] {
				addAction,
				manageAction,
				new ColumnedActionSeparator(),
				deleteAction,
				new ColumnedActionSeparator(),
				propertiesAction
		};
	}

	public boolean manageUser() {
		return true;
	}

}
