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
package com.arcadsoftware.afs.client.core.ui.views;

import java.util.List;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.AFSFormatTools;
import com.arcadsoftware.afs.client.core.ui.composites.BasicResultNotAllowedComposite;
import com.arcadsoftware.afs.client.core.ui.views.AbstractSecuredView;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;

public abstract class AbstractBasicConnectedView extends AbstractSecuredView {

	private Composite parentComposite;
	private Composite mainComposite;
	private IToolBarManager toolbarManager;
	
	public List<Integer> getExpectedRigths() {
		return null;
	}

	
	@Override
	protected void connectionChanged(ServerConnection connection) {
		super.connectionChanged(connection);
		createContent(parentComposite);
		parentComposite.layout();
		toolbarManager.removeAll();
		fillToolbar(toolbarManager);
		toolbarManager.update(true);
	}

	private void createContent(Composite parent){
		if (mainComposite!=null) {
			mainComposite.dispose();
		}
		mainComposite = AFSFormatTools.createComposite(parent,1,false);
		if (parent.getLayout() instanceof GridLayout) {
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			mainComposite.setLayoutData(gridData);
		}
		GridLayout gd = (GridLayout)mainComposite.getLayout();
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 1;
		gd.marginHeight = gd.marginWidth = 0;
		
		if (isAllowed()) {
			extended_HeaderControl(mainComposite);
			Composite parentResultComposite = createContentComposite(mainComposite);
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			parentResultComposite.setLayoutData(gridData);
			gd = new GridLayout(1,false);
			gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;
			gd.marginHeight = gd.marginWidth = 0;
			parentResultComposite.setLayout(gd);
			extended_FooterControl(mainComposite);
			super.createPartControl(parent);
		} else {
			new BasicResultNotAllowedComposite(mainComposite);
		}		
	}
	
	
	public void extended_HeaderControl(Composite parent) {
		
	}
	
	public void extended_FooterControl(Composite parent) {
		
	}
	
	public abstract Composite createContentComposite(Composite parent);
	
	
	
	@Override
	public void createPartControl(Composite parent) {
		parentComposite =parent;
		DynamicHelp.updateContextHelpId(getDynamicHelpId(), parentComposite);
	}

	@Override
	protected void defineLocalToolbar(IToolBarManager manager) {
		super.defineLocalToolbar(manager);
		this.toolbarManager = manager;
	}	

	protected void fillToolbar(IToolBarManager manager) {
		
	}

}
