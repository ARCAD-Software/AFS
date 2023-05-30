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

import java.util.ArrayList;

import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.aev.core.ui.container.Container;

public class ContainerEntry extends Container implements IDoubleClickContainer {

	private final ContainerReference reference;
	private final ArrayList<ContainerReference> childs = new ArrayList<ContainerReference>();
	
	public ContainerEntry(Container parent, ContainerReference ref) {
		super(parent);
		reference = ref; 
		ContainerExtensionManager.getInstance().createChildExtensions(childs, ref.getId());
	}

	public  ArrayList<ContainerReference> getChilds() {
		return childs;
	}
	
	public void doOnDoubleClick() {}

	public Object[] getChildren() {
		if (childs.size() > 0) {
			ContainerEntry[] result = new ContainerEntry[childs.size()];
			for(int i = 0; i < childs.size(); i++) {
				result[i] = new ContainerEntry(this, childs.get(i));				
			}
			return result;
		}
		return new Object[0];
	}

	public Image getImage() {
		return reference.getImage();
	}

	public String getLabel() {
		return reference.getLabel();
	}

	public String getUniqueKey() {
		return getParent().getUniqueKey().concat(reference.getUniqueKey());
	}

	public boolean hasChildren() {
		return childs.size() > 0;
	}

	public void refresh() {}
	
	@Override
	public int getIdentifier() {
		return reference.getIdentifier();
	}

}
