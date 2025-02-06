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
package com.arcadsoftware.afs.client.server.ui.containers;

import java.util.ArrayList;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.core.servers.model.Servers;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.actions.ServerContainerActions;

public class ServerContainer extends Container {

	protected Servers serverList;
	private final boolean autoExpandOnConnection;

	public ServerContainer(Container parent, boolean autoExpandOnConnection) {
		super(parent);
		this.autoExpandOnConnection = autoExpandOnConnection;
		actions = new ServerContainerActions(this);
		serverList = ServerLoader.getInstance().load();
	}

	public void addServer(Server server) {
		serverList.add(server);
	}

	public void deleteServer(Server server) {
		serverList.remove(server);
		refresh();
	}

	@Override
	public String getLabel() {
		return Activator.resString("navigationView.node.connections"); //$NON-NLS-1$
	}

	@Override
	public Image getImage() {
		return AFSIcon.SERVERS.image();
	}

	@Override
	public Object[] getChildren() {
		final ArrayList<ServerItem> list = new ArrayList<>();
		for (int i = 0; i < serverList.size(); i++) {
			list.add(createServerItem(serverList.get(i)));
		}
		return list.toArray();
	}

	/**
	 * Create server Item. Can be redefined by overriding
	 *
	 * @param server
	 * @return
	 */
	protected ServerItem createServerItem(Server server) {
		return new ServerItem(this, server, autoExpandOnConnection) {
			@Override
			public Image getImage() {
				return getServerItemImage();
			}
		};
	}

	protected Image getServerItemImage() {
		return AFSIcon.SERVER.image();
	}

	@Override
	public boolean hasChildren() {
		return !serverList.isEmpty();
	}

	@Override
	public String getUniqueKey() {
		return getParent().getUniqueKey().concat("/SRVMAN"); //$NON-NLS-1$
	}

	@Override
	public void refresh() {
		serverList = ServerLoader.getInstance().load();
	}

	public String getAddServerText() {
		return Activator.resString("server.action.add.text");
	}

	public String getAddServerTooltip() {
		return Activator.resString("server.action.add.tooltip");
	}

	public ImageDescriptor getAddServerImage() {
		return AFSIcon.ADD.imageDescriptor();
	}

}
