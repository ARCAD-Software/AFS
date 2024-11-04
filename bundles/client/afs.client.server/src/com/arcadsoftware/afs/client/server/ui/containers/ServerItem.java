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
package com.arcadsoftware.afs.client.server.ui.containers;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.ui.containers.AbstractConnectedContainer;
import com.arcadsoftware.afs.client.core.ui.containers.AbstractConnectedContainerProvider;
import com.arcadsoftware.afs.client.core.ui.propertysource.AbstractAFSProperties;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.actions.ServerConnectAction;
import com.arcadsoftware.afs.client.server.ui.actions.ServerItemActions;
import com.arcadsoftware.afs.client.server.ui.propertysource.ServerPropertySource;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.afs.framework.ui.views.AFSRootContainerInput;

public class ServerItem extends AbstractConnectedContainerProvider {

	public static final String EXTENSION_ID = "com.arcadsoftware.afs.client.server.main.container"; //$NON-NLS-1$
	public static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$
	public static final String VIEWID_ATTRIBUTE = "viewId"; //$NON-NLS-1$
	public static final String ORDER_ATTRIBUTE = "order"; //$NON-NLS-1$
	public static final String VISIBLE_ATTRIBUTE = "visible"; //$NON-NLS-1$

	private List<AbstractConnectedContainer> mainContainers;
	private boolean loaded;
	private final Server server;
	private ServerPropertySource propertySource;
	private final boolean autoExpandOnConnection;

	public ServerItem(Container parent, Server server, boolean autoExpandOnConnection) {
		super(parent);
		this.server = server;
		actions = new ServerItemActions(parent.getViewer().getControl().getShell(), parent, this);
		this.autoExpandOnConnection = autoExpandOnConnection;
	}

	public void connect() {
		if (actions instanceof ServerItemActions) {
			final ServerConnectAction connectionAction = ((ServerItemActions) actions).getConnectionAction();
			connectionAction.run();
			if (connectionAction.isRunOk() && autoExpandOnConnection) {
				expand();
			}
		}
	}

	@Override
	public String getLabel() {
		return server.getName();
	}

	public Server getServer() {
		return server;
	}

	@Override
	public Image getImage() {
		return AFSIcon.SERVER.image();
	}

	@Override
	public String getUniqueKey() {
		return getParent().getUniqueKey().concat("/SRV").concat(server.getName()); //$NON-NLS-1$
	}

	@Override
	public Object[] getContainers() {
		if (isConnected()) {
			if (!loaded) {
				loadContainers();
				loaded = true;
			}
			if (mainContainers != null) {
				return mainContainers.toArray();
			}
		}
		return new Object[0];
	}

	private int getOrder(IConfigurationElement element) {
		final String orderValue = element.getAttribute(ORDER_ATTRIBUTE);
		if ((orderValue == null) || (orderValue.length() == 0)) {
			return 0;
		}
		try {
			return Integer.parseInt(orderValue);
		} catch (final NumberFormatException e) {
			return 0;
		}
	}

	public void loadContainers() {
		String parentViewId = null;
		if (getRootContainerInput() instanceof AFSRootContainerInput) {
			parentViewId = ((AFSRootContainerInput) getRootContainerInput()).getParentViewId();
		}
		if (parentViewId != null) {
			mainContainers = new ArrayList<>();
			final IExtensionRegistry registry = Platform.getExtensionRegistry();
			final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
			for (final IConfigurationElement element : elements) {
				try {
					if (isValid(element.getAttribute(VISIBLE_ATTRIBUTE))) {
						final String viewId = element.getAttribute(VIEWID_ATTRIBUTE);
						if (viewId.equalsIgnoreCase(parentViewId)) {
							final AbstractConnectedContainer container = (AbstractConnectedContainer) element.createExecutableExtension(CLASS_ATTRIBUTE);
							final Properties props = new Properties();
							final IConfigurationElement[] properties = element.getChildren("property"); //$NON-NLS-1$
							for (final IConfigurationElement property : properties) {
								final String key = property.getAttribute("key"); //$NON-NLS-1$
								final String value = property.getAttribute("value"); //$NON-NLS-1$
								props.put(key, value);
							}
							props.put("parentViewId", parentViewId); //$NON-NLS-1$
							if (props.containsKey("id")) { //$NON-NLS-1$
								container.setContainerId(props.getProperty("id")); //$NON-NLS-1$
							}
							container.setProps(props);
							container.setOrder(getOrder(element));
							container.setParent(this);
							mainContainers.add(container);
						}
						mainContainers.sort(null);
					}
				} catch (final CoreException e) {
					LogUITools.logWarning(Activator.getDefault().getBundle(), e);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T getAdapter(Class<T> adapter) {
		if (adapter == AbstractAFSProperties.class) {
			if (propertySource == null) {
				propertySource = new ServerPropertySource(server);
			}
			return (T) propertySource;
		}
		return super.getAdapter(adapter);
	}

	@Override
	public Object[] getFixedChildren() {
		return new Object[0];
	}

	@Override
	public void setServerConnection(ServerConnection connection) {
		super.setServerConnection(connection);
		// Force reloading children with new connection
		if ((mainContainers != null) && !mainContainers.isEmpty()) {
			mainContainers = null;
			loaded = false;
		}
	}

	/**
	 * Check Web Service validity
	 *
	 * @param url
	 * @return
	 */
	private boolean isValid(String serviceUrl) {
		if ((getServerConnection() == null) || (serviceUrl == null) || (serviceUrl.length() == 0)) {
			return true;
		}
		final DataAccessHelper helper = new DataAccessHelper(getServerConnection());
		return helper.getXml(serviceUrl) != null;
	}
}
