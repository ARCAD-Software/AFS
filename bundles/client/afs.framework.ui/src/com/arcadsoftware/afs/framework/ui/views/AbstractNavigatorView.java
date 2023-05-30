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
package com.arcadsoftware.afs.framework.ui.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.actions.ArcadActions;
import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.aev.core.ui.container.ContainerProvider;
import com.arcadsoftware.aev.core.ui.container.IContainer;
import com.arcadsoftware.aev.core.ui.container.RootContainerInput;
import com.arcadsoftware.afs.framework.ui.containers.ContainerEntry;
import com.arcadsoftware.afs.framework.ui.containers.ContainerExtensionManager;
import com.arcadsoftware.afs.framework.ui.containers.ContainerReference;
import com.arcadsoftware.afs.framework.ui.containers.viewer.ContainerTreeViewer;

public abstract class AbstractNavigatorView extends AbstractAFSView {
	
	protected ContainerTreeViewer viewer;
	protected RootContainerInput sc;
	private List<ContainerReference> containerList;
	
	public AbstractNavigatorView() {
		super();
		defineActions();
	}

	protected ContainerTreeViewer initializeContainerTreeViewer(Composite parent) {
		return new ContainerTreeViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				AbstractNavigatorView.this.doOnDoubleClick(selection);
			}
			@Override
			protected void doOnSelect(Object selected) {
				AbstractNavigatorView.this.doOnSelection();
			}
			@Override
			protected void fillContextMenu(IMenuManager newManager) {
				defineLocalContextMenu(newManager);
			}
			
		};
	}	
	
	@Override
	protected void defineLocalContextMenu(IMenuManager manager) {
		final IContainer c = viewer.getSelectedElement();
		if (c != null) {
			c.manageMenuAction(manager);
			if (c instanceof Container) {
				fillFixedContainerAction(manager);
			}
			if (c instanceof ContainerProvider) {
				fillFixedContainerProviderAction(manager);
			}
		}
		manager.add(new Separator("Additions")); //$NON-NLS-1$
	}	
	
	@Override
	public void createPartControl(Composite parent) {
		readContainers();
		viewer = initializeContainerTreeViewer(parent);
		super.createPartControl(parent);
		sc = defineInput();
		viewer.setInput(sc);
		viewerCreated(viewer);
	}
	
	protected void viewerCreated(ContainerTreeViewer viewer) {
		//Do nothing
	}
	
	private void readContainers() {
		containerList = new ArrayList<>();
		ContainerExtensionManager.getInstance().createExtensions(containerList);
		for (int i = 0; i < containerList.size(); i++) {
			containerList.get(i).setIdentifier(i);
		}
	}
	
	protected RootContainerInput defineInput() {
		return new AFSRootContainerInput(viewer.getViewer()) {

			public String getLabel() {
				return getRootName();
			}

			public Image getImage() {
				return getRootImage();
			}

			public Object[] getChildren() {
				return getRootChildren(this);
			}
			
			@Override
			public ArcadActions getRootContainerActions() {
				return getRootActions();
			}

			@Override
			public String getParentViewId() {
				return getViewId();
			}
		};
	}
	
	protected void doOnDoubleClick(IStructuredSelection selection) {
		Object o = selection.getFirstElement();
		if (o instanceof IContainer) {
			doubleClickOnContainer((IContainer) o);
		}
	}
	
	protected void doOnSelection() {
		IStructuredSelection sel = viewer.getSelection();
		Object o = sel.getFirstElement();
		if (o instanceof IContainer) {
			containerSelected((IContainer) o);
		}
	}	
	
	private ContainerEntry createContainer(Container parent,ContainerReference reference){
		ContainerEntry container = new ContainerEntry(parent,reference);
		for (ContainerReference r: container.getChilds()) {
			for (ContainerReference ref : containerList) {
				if (r.getId().equalsIgnoreCase(ref.getId())) {
					// We affect the identifier computed into the getContainerList() method()
					// This identifier is a integer that nedd to be passed to the viewer
					// to allow to display the container
					r.setIdentifier(ref.getIdentifier());
				}
			}
		}
		return container;		
	}		
		
	public List<ContainerReference> getContainerList() {
		return containerList;
	}

	public String getRootName() {
		return ""; //$NON-NLS-1$
	}
	
	public Image getRootImage() {
		return null;
	}
	
	public ArcadActions getRootActions() {
		return null;
	}
	
	public Object[] getRootChildren(Container root) {
		final List<Container> results = new ArrayList<>();
		for (ContainerReference ref : containerList) {
			// We add only container with no parent category
			if(ref.getCategory().length() == 0) {
				results.add(createContainer(root,ref));
			}
		}		
		return results.toArray();
	}
	
	protected void doubleClickOnContainer(IContainer o) {}
	
	protected void containerSelected(IContainer o) {}	

	public Container getInput() {
		return sc;
	}
	
	protected void fillFixedContainerAction(IMenuManager manager) {}
	
	protected void fillFixedContainerProviderAction(IMenuManager manager) {}	
	
	public abstract String getViewId();
}
