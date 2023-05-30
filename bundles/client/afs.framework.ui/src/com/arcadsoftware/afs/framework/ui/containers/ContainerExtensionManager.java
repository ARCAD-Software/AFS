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
package com.arcadsoftware.afs.framework.ui.containers;

import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.afs.framework.ui.images.ImageManager;

public class ContainerExtensionManager {
	
	public static final String EXTENSION_ID = "com.arcadsoftware.afs.framework.ui.container.manager"; //$NON-NLS-1$
	private static final ContainerExtensionManager instance = new ContainerExtensionManager();

	public static ContainerExtensionManager getInstance() {
		return instance;
	}
	
	private ContainerExtensionManager() {
		super();
	}

	private ContainerReference createContainer(IConfigurationElement element) {
		ContainerReference childContainer = new ContainerReference();
		childContainer.setLabel(element.getAttribute("label")); //$NON-NLS-1$
		childContainer.setViewid(element.getAttribute("viewid")); //$NON-NLS-1$
		String iconKey = element.getAttribute("icon");//$NON-NLS-1$
		if (iconKey != null) {
			childContainer.setImage(ImageManager.getInstance().getImage(iconKey));
		}
		childContainer.setUniqueKey(element.getAttribute("key")); //$NON-NLS-1$
		childContainer.setId(element.getAttribute("id")); //$NON-NLS-1$
		childContainer.setCategory(element.getAttribute("category")); //$NON-NLS-1$		
		return childContainer;
	}
	
	public void createExtensions(List<ContainerReference> list) {
		IConfigurationElement[] elements = Platform.getExtensionRegistry().getConfigurationElementsFor(EXTENSION_ID);
		for (IConfigurationElement element: elements) {
			ContainerReference childContainer = createContainer(element);
			list.add(childContainer);
		}
	}
	
	public void createChildExtensions(List<ContainerReference> list, String parentId) {
		for (IConfigurationElement element: Platform.getExtensionRegistry().getConfigurationElementsFor(EXTENSION_ID)) {
			String category = element.getAttribute("category");//$NON-NLS-1$
			boolean ok = (category != null);
			if ((category != null) && (parentId != null) && (parentId.length() > 0)) {
				ok = category.equalsIgnoreCase(parentId);
			}
			if (ok) {
				ContainerReference  childContainer = createContainer(element);
				list.add(childContainer);
			}
		}			
	}	
	

}
