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
package com.arcadsoftware.afs.client.server.ui.views;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.IConnectionListener;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.server.admin.common.core.model.ConsoleConnector;
import com.arcadsoftware.afs.client.server.admin.common.core.model.category.CategoriesWrapper;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers.ActionManager;
import com.arcadsoftware.afs.client.server.admin.common.ui.settings.viewers.SectionViewer;
import com.arcadsoftware.afs.client.server.admin.common.ui.views.AbstractSSCView;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.rest.console.Category;
import com.arcadsoftware.rest.console.SectionId;

public class SectionView extends AbstractSSCView implements IConnectionListener{
	public static final String ID = "com.arcadsoftware.afs.client.server.ui.views.SectionView"; //$NON-NLS-1$
	
	
	List<Category> categories; 
	CategoriesWrapper categoryinput;	
	ServerConnection currentServerConnection;
	SectionViewer viewer;
	Action refreshAction;
	
	ConsoleConnector consoleConnector;
	
	public SectionView(){
		super();
		defineActions();
		ConnectionManager.getInstance().addConnectionListener(this);
	}
	
	
	@Override
	public void createPartControl(Composite parent) {
		GridLayout l = new GridLayout(1,false); 
		l.marginHeight = l.marginWidth = 0;
		l.marginLeft = l.marginRight = l.marginBottom = l.marginTop= 0;		
		parent.setLayout(l);				
		viewer = new SectionViewer(parent, SWT.NONE | SWT.FULL_SELECTION){
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {	
				if (getSelectedCategory().isSection()) {				
					editSection(getSelectedCategory().getSectionId());
				}
			}
		};
		viewer.getViewer().getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 3, 1));
		((Tree)viewer.getViewer().getControl()).setLinesVisible(false);
		super.createPartControl(parent);
		
		ConnectionManager.getInstance().getLastServerConnection().ifPresent(this::OnConnection);
	}
	
	public void editSection(SectionId sectionid){
		ActionManager actionManager = new ActionManager(consoleConnector,sectionid);
		actionManager.execute();
	}
	
	
	@Override
	protected void defineActions() {
		refreshAction = new Action(){
			@Override
			public void run() {				
				OnConnection(currentServerConnection);
			}			
		};
		refreshAction.setText(Activator.resString("server.action.refresh.text")); //$NON-NLS-1$
		refreshAction.setToolTipText(Activator.resString("server.action.refresh.tooltip")); //$NON-NLS-1$
		refreshAction.setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
	}
	
	
	@Override
	protected void fillLocalToolBar(IToolBarManager manager) {
		manager.add(refreshAction);
	}


	@Override
	public void OnConnection(ServerConnection connection) {
		currentServerConnection = connection;
		consoleConnector = new ConsoleConnector(connection);
		if (connection.isConnected()) {			
			categories = consoleConnector.getCategories();
			if(categories != null){
				categoryinput = new CategoriesWrapper(categories);
				viewer.setInput(categoryinput);
			}
		}
		
	}	
	
	@Override
	public void dispose() {
		ConnectionManager.getInstance().removeConnectionListener(this);
	}
}
