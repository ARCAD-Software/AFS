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

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.AFSFormatTools;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractResultComposite;
import com.arcadsoftware.afs.client.core.ui.composites.BasicResultNotAllowedComposite;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractResultListView extends AbstractSecuredView{

	private Composite parentComposite;
	protected Composite mainComposite;
	private IToolBarManager toolbarManager;
	protected AbstractResultComposite listComposite;
	private MetaDataEntity entity;

	public AbstractResultListView() {
		super();
	}

	@Override
	protected void connectionChanged(ServerConnection connection) {
		super.connectionChanged(connection);
		entity= getHelper().getEntity(getType());
		createContent(parentComposite);
		parentComposite.layout();
		if (toolbarManager != null) { // It could be the case if not allowed to search
			toolbarManager.removeAll();
			fillToolbar(toolbarManager);
			toolbarManager.update(true);
		}
	}

	private void createContent(Composite parent) {
		if (mainComposite != null) {
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
			if (entity != null) {
				listComposite = createListComposite(mainComposite, getType(), getConnection());
				GridData gridData = new GridData(GridData.FILL_BOTH);
				gridData.grabExcessHorizontalSpace = true;
				gridData.grabExcessVerticalSpace = true;
				gridData.horizontalSpan = 3;
				listComposite.setLayoutData(gridData);
				super.createPartControl(parent);
			}
		} else {
			new BasicResultNotAllowedComposite(mainComposite);
		}		
	}
	
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

	public MetaDataEntity getStructure() {
		return entity;
	}

	protected void fillToolbar(IToolBarManager manager) {}
		
	/**
	 * Redefine this method in the inherited classes to return a string which represents the bean identifier you want to
	 * manipulate through this search view.
	 * 
	 * @return String : Bean Identifier.
	 */
	public abstract String getType();

	protected abstract void readStructureError();

	protected abstract AbstractResultComposite createListComposite(Composite parent, String entity, ServerConnection connection);
}
