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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.AFSFormatTools;
import com.arcadsoftware.afs.client.core.ui.composites.AbstractSearchComposite;
import com.arcadsoftware.afs.client.core.ui.composites.BasicResultComposite;
import com.arcadsoftware.afs.client.core.ui.composites.BasicResultNotAllowedComposite;
import com.arcadsoftware.afs.client.core.ui.listeners.ISearchListener;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractSearchView extends AbstractSecuredView {

	private Composite parentComposite;
	private Composite mainComposite;
	private IToolBarManager toolbarManager;
	protected AbstractSearchComposite searchComposite;
	protected ISearchListener resultComposite;
	protected MetaDataEntity entity;

	public AbstractSearchView() {
		super();
	}

	@Override
	protected void connectionChanged(ServerConnection connection) {
		entity = getHelper().getEntity(getType());
		createContent(parentComposite);
		parentComposite.layout();
		if (toolbarManager != null) {
			toolbarManager.removeAll();
			fillToolbar(toolbarManager);
			getViewSite().getActionBars().updateActionBars();
			toolbarManager.update(true);
		}
	}

	private void createContent(Composite parent){
		if (mainComposite != null) {
			mainComposite.dispose();
		}
		mainComposite = AFSFormatTools.createComposite(parent, 1, false);
		if (parent.getLayout() instanceof GridLayout) {
			GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			mainComposite.setLayoutData(gridData);
		}
		GridLayout gd = (GridLayout) mainComposite.getLayout();
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 1;
		gd.marginHeight = gd.marginWidth = 0;
		if (isAllowed()) {
			if (entity != null) {
				SashForm listAndEditor = new SashForm(mainComposite, SWT.HORIZONTAL);
				GridData gridData = new GridData(GridData.FILL_BOTH);
				gridData.grabExcessHorizontalSpace = true;
				gridData.grabExcessVerticalSpace = true;
				listAndEditor.setLayoutData(gridData);				
				searchComposite = createSearchComposite(getConnection(), listAndEditor, entity);
				if (listAndEditor.getLayout() instanceof GridLayout) {
					gridData = new GridData(GridData.FILL_BOTH);
					gridData.grabExcessHorizontalSpace = true;
					gridData.grabExcessVerticalSpace = true;
					searchComposite.setLayoutData(gridData);
				}
				Composite parentResultComposite = new Composite(listAndEditor, SWT.NONE);
				if (listAndEditor.getLayout() instanceof GridLayout) {
					gridData = new GridData(GridData.FILL_BOTH);
					gridData.grabExcessHorizontalSpace = true;
					gridData.grabExcessVerticalSpace = true;
					parentResultComposite.setLayoutData(gridData);
				}
				gd = new GridLayout(1, false);
				gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = gd.marginHeight = gd.marginWidth = 0;
				parentResultComposite.setLayout(gd);
				extended_resultHeaderControl(parentResultComposite);
				resultComposite = createResultComposite(parentResultComposite, getDisplayedSelectClause(), searchComposite
						.getQueryManager(), getConnection());
				gridData = new GridData(GridData.FILL_BOTH);
				gridData.grabExcessHorizontalSpace = true;
				gridData.grabExcessVerticalSpace = true;
				resultComposite.getParentComposite().setLayoutData(gridData);
				extended_resultFooterControl(parentResultComposite);
				listAndEditor.setWeights(getWeights());
				searchComposite.getQueryManager().setResultListener(resultComposite);
				super.createPartControl(parent);
			}
		} else {
			new BasicResultNotAllowedComposite(mainComposite);
		}		
	}
	
	public void extended_resultHeaderControl(Composite parent) {}
	
	public void extended_resultFooterControl(Composite parent) {}
	
	@Override
	public void createPartControl(Composite parent) {
		parentComposite =parent;
		DynamicHelp.updateContextHelpId(getDynamicHelpId(), parentComposite);
	}
	
	protected int[] getWeights() {
		return new int[] {28, 100};
	}
	
	@Override
	protected void defineLocalToolbar(IToolBarManager manager) {
		this.toolbarManager = manager;
	}	
	
	protected ISearchListener createResultComposite(Composite parent, String selectClause,
			QueryManager queryManager,ServerConnection connection) {
		return new BasicResultComposite(parent, entity, selectClause, queryManager,connection);
	}

	public MetaDataEntity getStructure() {
		return entity;
	}

	protected void fillToolbar(IToolBarManager manager) {
		if (resultComposite.isSearchManagementActivated()) {
			Action autosearchAction = new Action("", IAction.AS_CHECK_BOX) {
				@Override
				public void run() {					
					resultComposite.setAutosearch(this.isChecked());					
				}
			};
			autosearchAction.setText(Activator.resString("search.autosearch.action.text"));
			autosearchAction.setToolTipText(Activator.resString("search.autosearch.action.tooltip"));
			autosearchAction.setImageDescriptor(AFSIcon.SEARCH_EXTENDED.imageDescriptor());
			autosearchAction.setChecked(resultComposite.isAutosearch());
			manager.add(autosearchAction);
			manager.add(new Separator());
		}
	}
	
	/**
	 * Redefine this method in the inherited classes to create your own "searchComposite".
	 * 
	 * @param parent
	 *            : Composite : The parent composite of the search Editor
	 * @return AbstractSearchComposite : A composite inherited from AbstractSearchComposite
	 */
	public abstract AbstractSearchComposite createSearchComposite(ServerConnection connection, Composite parent, MetaDataEntity newStructure);

	/**
	 * Redefine this method in the inherited classes to return a string which represents the bean identifier you want to
	 * manipulate through this search view.
	 * 
	 * @return String : Bean Identifier.
	 */
	public abstract String getType();

	protected String getDisplayedSelectClause() {
		return searchComposite.createSelectClause();
	}
	
	protected abstract void readStructureError();
	
	public void autosearch() {
		if ((resultComposite != null) && resultComposite.isSearchManagementActivated()) {
			if (resultComposite.isAutosearch()) {
				searchComposite.search();
			}
		}
	}
	
	@Override
	public void activate() {
		super.activate();
		autosearch();
	}
}
