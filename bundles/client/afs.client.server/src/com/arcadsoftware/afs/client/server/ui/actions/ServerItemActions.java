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
package com.arcadsoftware.afs.client.server.ui.actions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.aev.core.ui.container.ContainerProvider;
import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.IRightManagerExtension;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.server.IServerRights;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.containers.ServerContainer;
import com.arcadsoftware.afs.client.server.ui.containers.ServerItem;
import com.arcadsoftware.afs.framework.ui.containers.ContainerEntryActions;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;

public class ServerItemActions extends ContainerEntryActions {

	public static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$
	public static final String EXTENSION_ID = "com.arcadsoftware.afs.client.server.action"; //$NON-NLS-1$

	private ServerConnectAction connectionAction;
	private ServerPreferencesAction preferencesAction;

	public ServerItemActions(final Shell shell) {
		super();
		preferencesAction.setShell(shell);
	}

	public ServerItemActions(final Shell shell, final Container container, final ContainerProvider containerProvider) {
		super(false);
		setContainer(container);
		setContainerProvider(containerProvider);
		makeAction();
		preferencesAction.setShell(shell);
	}

	public ServerConnectAction getConnectionAction() {
		return connectionAction;
	}

	private List<AbstractExtendedServerAction> getExtentedActions() {
		final List<AbstractExtendedServerAction> actions = new ArrayList<>();
		final IExtensionRegistry registry = Platform.getExtensionRegistry();
		final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
		for (final IConfigurationElement element : elements) {
			try {
				final AbstractExtendedServerAction action = (AbstractExtendedServerAction) element
						.createExecutableExtension(CLASS_ATTRIBUTE);
				action.setSelectedServer(((ServerItem) getContainerProvider()).getServer());
				action.setServerContainer((ServerContainer) getContainer());
				actions.add(action);
			} catch (final CoreException e) {
				LogUITools.logWarning(Activator.getDefault().getBundle(), e);
			}
		}
		return actions;
	}

	private Server getServer() {
		return ((ServerItem) getContainerProvider()).getServer();
	}

	private ServerConnection getServerConnection() {
		return ((ServerItem) getContainerProvider()).getServerConnection();
	}

	@Override
	public void makeAction() {
		connectionAction = new ServerConnectAction() {
			@Override
			protected void doAfterRun() {
				if (runOk) {
					final ServerItem sc = (ServerItem) getContainerProvider();
					if (sc != null) {
						sc.setServerConnection(getConnection());
						sc.getViewer().refresh(sc);
					}
				}
			}

			@Override
			protected Server getServerToManage() {
				return getServer();
			}
		};
		final ArcadAction deleteAction = new ServerDeleteAction() {
			@Override
			protected void doAfterRun() {
				if (runOk) {
					final ServerContainer sc = (ServerContainer) getContainer();
					sc.deleteServer(getServerToManage());
					sc.getViewer().refresh(sc);
				}
			}

			@Override
			protected Server getServerToManage() {
				return getServer();
			}
		};

		preferencesAction = new ServerPreferencesAction() {
			@Override
			public List<Integer> getExpectedRigths() {
				return null;
			}

			@Override
			protected ServerConnection getServerConnection() {
				return ServerItemActions.this.getServerConnection();
			}

			@Override
			protected Server getServerToManage() {
				return getServer();
			}

			@Override
			protected void setServerConnection(final ServerConnection connection) {
				ServerItemActions.this.setServerConnection(connection);
			}
		};

		final ArcadAction configureAction = new ServerConfigureAction() {

			@Override
			public List<Integer> getExpectedRigths() {
				final IRightManagerExtension manager = AFSRightManager.getRightManager();
				if (manager != null) {
					return manager.getExpectedRights(IServerRights.ADMINISTRATION_CONFIGURE, true);
				}
				return null;
			}

			@Override
			protected ServerConnection getServerConnection() {
				return ServerItemActions.this.getServerConnection();
			}

			@Override
			protected Server getServerToManage() {
				return getServer();
			}

			@Override
			protected void setServerConnection(final ServerConnection connection) {
				ServerItemActions.this.setServerConnection(connection);
			}

		};

		final ArcadAction getLogFile = new ServerGetLogfileAction() {
			@Override
			protected ServerConnection getServerConnection() {
				return ServerItemActions.this.getServerConnection();
			}

			@Override
			protected Server getServerToManage() {
				return getServer();
			}

		};

		final ArcadAction aboutServerAction = new AboutServerAction() {
			@Override
			protected ServerConnection getServerConnection() {
				return ServerItemActions.this.getServerConnection();
			}
		};
		//aboutServerAction.setEnabled(true);

		addAction(connectionAction);
		addAction(aboutServerAction);
		addSeparator();
		addAction(configureAction);
		addAction(preferencesAction);
		addAction(getLogFile);
		addSeparator();
		addAction(deleteAction);		

		final List<AbstractExtendedServerAction> actions = getExtentedActions();
		if (!actions.isEmpty()) {
			addSeparator();
			for (final AbstractExtendedServerAction a : actions) {
				addAction(a);
			}
		}
	}

	private void setServerConnection(final ServerConnection serverConnection) {
		((ServerItem) getContainerProvider()).setServerConnection(serverConnection);
	}

}
