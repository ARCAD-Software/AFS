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
package com.arcadsoftware.afs.client.core.ui.containers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.aev.core.tools.StringTools;
import com.arcadsoftware.aev.core.ui.container.IContainer;
import com.arcadsoftware.afs.client.core.IACCMessages;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;

public class ConnectedContainerExtensionManager {

	public static final String EXTENSION_ID = "com.arcadsoftware.afs.client.core.connectedcontainer"; //$NON-NLS-1$
	public static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$
	public static final String VIEWID_ATTRIBUTE = "parentViewid"; //$NON-NLS-1$
	public static final String PARENTID_ATTRIBUTE = "parentId"; //$NON-NLS-1$
	public static final String CONTAINERID_ATTRIBUTE = "id"; //$NON-NLS-1$
	public static final String RIGHTID_ATTRIBUTE = "rightId"; //$NON-NLS-1$
	public static final String ORDER_ATTRIBUTE = "order"; //$NON-NLS-1$

	private static ConnectedContainerExtensionManager instance = new ConnectedContainerExtensionManager();

	private ConnectedContainerExtensionManager() {
		super();
	}

	public static ConnectedContainerExtensionManager getInstance() {
		return instance;
	}

	private boolean checkRight(ServerConnection connection, String rightId) {
		if (rightId != null) {
			if (StringTools.isNumeric(rightId)) {
				final ArrayList<Integer> rights = new ArrayList<>();
				rights.add(new Integer(rightId));
				return connection.isAllowed(rights);
			} else {
				final UserMessage um = UserMessageManager.getInstance()
						.getMessage(IACCMessages.ERR_CONTAINER_INVALID_RIGHT, rightId);
				LogUITools.logError(Activator.getDefault().getBundle(), um);
				return false;
			}
		}
		return true;
	}

	public void createChildContainerProviders(AbstractConnectedContainer parent,
			List<AbstractConnectedContainerProvider> mainContainerProviders, String parentViewId) {
		if (parentViewId != null) {
			final IExtensionRegistry registry = Platform.getExtensionRegistry();
			final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
			for (final IConfigurationElement element : elements) {
				try {
					final String viewId = element.getAttribute(VIEWID_ATTRIBUTE);
					final String rightId = element.getAttribute(RIGHTID_ATTRIBUTE);
					final boolean allowed = checkRight(parent.getServerConnection(), rightId);
					if (allowed) {
						boolean everyWhere = true;
						if ((viewId != null) && !viewId.equalsIgnoreCase(parentViewId)) {
							everyWhere = false;
						}
						if (everyWhere) {
							final String parentId = element.getAttribute(PARENTID_ATTRIBUTE);
							final String containerId = element.getAttribute(CONTAINERID_ATTRIBUTE);

							if (parentId.equalsIgnoreCase(parent.getContainerId())) {
								final Object o = element.createExecutableExtension(CLASS_ATTRIBUTE);
								if (o instanceof AbstractConnectedContainerProvider) {
									final AbstractConnectedContainerProvider container = (AbstractConnectedContainerProvider) o;
									container.setParent(parent);
									container.setContainerId(containerId);
									container.setOrder(getOrder(element));
									mainContainerProviders.add(container);
								}
							}
						}
					}
				} catch (final CoreException e) {
					LogUITools.logWarning(Activator.getDefault().getBundle(), e);
				}
			}

			Collections.sort(mainContainerProviders, getContainerComparator());
		}
	}

	public int getOrder(IConfigurationElement element) {
		final String orderValue = element.getAttribute(ORDER_ATTRIBUTE);
		if ((orderValue == null) || (orderValue.length() == 0)) {
			return 0;
		} else {
			try {
				final int value = Integer.parseInt(orderValue);
				return value;
			} catch (final NumberFormatException e) {
				return 0;
			}
		}
	}

	public void createChildContainers(AbstractConnectedContainer parent, List<IContainer> mainContainerProviders,
			String parentViewId) {
		if (parentViewId != null) {
			final IExtensionRegistry registry = Platform.getExtensionRegistry();
			final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
			for (final IConfigurationElement element : elements) {
				try {
					final String viewId = element.getAttribute(VIEWID_ATTRIBUTE);
					final String rightId = element.getAttribute(RIGHTID_ATTRIBUTE);
					final boolean allowed = checkRight(parent.getServerConnection(), rightId);
					if (allowed) {
						boolean everyWhere = true;
						if ((viewId != null) && !viewId.equalsIgnoreCase(parentViewId)) {
							everyWhere = false;
						}
						if (everyWhere) {
							final String parentId = element.getAttribute(PARENTID_ATTRIBUTE);
							final String containerId = element.getAttribute(CONTAINERID_ATTRIBUTE);

							if (parentId.equalsIgnoreCase(parent.getContainerId())) {
								final Object o = element.createExecutableExtension(CLASS_ATTRIBUTE);
								if (o instanceof AbstractConnectedContainerProvider) {
									final AbstractConnectedContainerProvider container = (AbstractConnectedContainerProvider) o;
									container.setParent(parent);
									container.setContainerId(containerId);
									container.setOrder(getOrder(element));
									mainContainerProviders.add(container);
								} else if (o instanceof AbstractConnectedContainer) {
									final AbstractConnectedContainer container = (AbstractConnectedContainer) o;
									container.setParent(parent);
									container.setContainerId(containerId);
									container.setOrder(getOrder(element));
									mainContainerProviders.add(container);
								}
							}
						}
					}
				} catch (final CoreException e) {
					LogUITools.logWarning(Activator.getDefault().getBundle(), e);
				}
			}

			Collections.sort(mainContainerProviders, getContainerComparator());
		}
	}

	public void createChildContainers(AbstractConnectedContainerProvider parent,
			List<AbstractConnectedContainer> mainContainers, String parentViewId) {
		createChildContainers(parent, mainContainers, parent.getContainerId(), parentViewId);
	}

	public void createChildContainers(AbstractConnectedContainerProvider parent,
			List<AbstractConnectedContainer> mainContainers, String expectedContainerId, String parentViewId) {
		if (parentViewId != null) {
			final IExtensionRegistry registry = Platform.getExtensionRegistry();
			final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
			for (final IConfigurationElement element : elements) {
				try {
					final String viewId = element.getAttribute(VIEWID_ATTRIBUTE);
					final String rightId = element.getAttribute(RIGHTID_ATTRIBUTE);
					final boolean allowed = checkRight(parent.getServerConnection(), rightId);
					if (allowed) {
						boolean everyWhere = true;
						if ((viewId != null) && !viewId.equalsIgnoreCase(parentViewId)) {
							everyWhere = false;
						}
						if (everyWhere) {
							final String parentId = element.getAttribute(PARENTID_ATTRIBUTE);
							final String containerId = element.getAttribute(CONTAINERID_ATTRIBUTE);
							if (parentId.equalsIgnoreCase(expectedContainerId)) {
								final AbstractConnectedContainer container = (AbstractConnectedContainer) element
										.createExecutableExtension(CLASS_ATTRIBUTE);
								container.setParent(parent);
								container.setContainerId(containerId);
								container.setOrder(getOrder(element));
								mainContainers.add(container);
							}
						}
					}
				} catch (final CoreException e) {
					LogUITools.logError(Activator.getDefault().getBundle(), e);
					final UserMessage um = UserMessageManager.getInstance()
							.getMessage(IACCMessages.ERR_CONTAINER_UNKNOWN_ERROR);
					LogUITools.logError(Activator.getDefault().getBundle(), um);
				}
			}

			Collections.sort(mainContainers, getContainerComparator());
		}
	}

	protected Comparator<IContainer> getContainerComparator() {
		return new Comparator<IContainer>() {
			@Override
			public int compare(IContainer o1, IContainer o2) {
				int order1 = 0;
				int order2 = 0;
				if (o1 instanceof AbstractConnectedContainerProvider) {
					final AbstractConnectedContainerProvider p1 = (AbstractConnectedContainerProvider) o1;
					order1 = p1.getOrder();
				}
				if (o1 instanceof AbstractConnectedContainer) {
					final AbstractConnectedContainer p1 = (AbstractConnectedContainer) o1;
					order1 = p1.getOrder();
				}
				if (o2 instanceof AbstractConnectedContainerProvider) {
					final AbstractConnectedContainerProvider p2 = (AbstractConnectedContainerProvider) o2;
					order2 = p2.getOrder();
				}
				if (o2 instanceof AbstractConnectedContainer) {
					final AbstractConnectedContainer p2 = (AbstractConnectedContainer) o2;
					order2 = p2.getOrder();
				}

				if (order1 > order2) {
					return 1;
				} else if (order1 < order2) {
					return -1;
				}
				return 0;

			}
		};
	}

}
