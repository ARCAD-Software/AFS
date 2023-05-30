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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.IACCMessages;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.AFSFormatTools;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractSearchAndResultComposite extends Composite implements ISecuredAction {

	protected ServerConnection connection;
	protected DataAccessHelper helper;
	
	
	private Composite mainComposite;
	
	
	protected AbstractSearchComposite searchComposite;
	protected AbstractResultComposite resultComposite;
	private MetaDataEntity entity = null;


	public AbstractSearchAndResultComposite(Composite parent, int style) {
		super(parent,style);
		GridLayout gd = new GridLayout(1,false);
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;
		gd.marginHeight = gd.marginWidth = 0;
		this.setLayout(gd);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		this.setLayoutData(gridData);		
		this.setBackground(parent.getBackground());
		if (connection!=null) {
			createContent(this);
		}
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public void setConnection(ServerConnection connexion) {
		this.connection = connexion;
		helper = new DataAccessHelper(connexion);
		connectionChanged(connexion);
	}

	public boolean isAllowed() {
		if (connection!=null)
			return connection.isAllowed(getExpectedRigths());
		else {
			LogUITools.logError(Activator.getDefault().getBundle(), 
					UserMessageManager.getInstance().getMessage(IACCMessages.ERR_SRH_CONNECTIONMISSING));
			return false;
		}
	}
	
	
	
	protected void connectionChanged(ServerConnection connection) {
		entity= helper.getEntity(getType());
		createContent(this);
		this.layout();
	}

	private void createContent(Composite parent){
		if (mainComposite!=null) {
			mainComposite.dispose();
		}
		mainComposite = AFSFormatTools.createComposite(parent,1,false);
		mainComposite.setBackground(parent.getBackground());
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		mainComposite.setLayoutData(gridData);
		GridLayout gd = (GridLayout)mainComposite.getLayout();
		gd.marginLeft = gd.marginTop = gd.marginRight = gd.marginBottom = 0;
		gd.marginHeight = gd.marginWidth = 0;
		
		if (isAllowed()) {
			if (entity != null) {
				if (shashed()) {				
					SashForm listAndEditor = new SashForm(mainComposite, getSashOrientation());
					listAndEditor.setBackground(parent.getBackground());
					gridData = new GridData(GridData.FILL_BOTH);
					gridData.grabExcessHorizontalSpace = true;
					gridData.grabExcessVerticalSpace = true;
					listAndEditor.setLayoutData(gridData);
					
					searchComposite = createSearchComposite(connection,listAndEditor, entity);	
					searchComposite.setBackground(parent.getBackground());
					formatSearchComposite(searchComposite);
	
					resultComposite = createResultComposite(listAndEditor, getDisplayedSelectClause(), searchComposite
							.getQueryManager(),connection);
					resultComposite.setBackground(parent.getBackground());
					formatResultComposite(resultComposite);
					listAndEditor.setWeights(getWeights());				
				} else {
					if (searchCompositeFirst()) {					
						searchComposite = createSearchComposite(connection,mainComposite, entity);									
						resultComposite = createResultComposite(mainComposite, getDisplayedSelectClause(), searchComposite
								.getQueryManager(),connection);
					} else {
						resultComposite = createResultComposite(mainComposite, getDisplayedSelectClause(),null ,connection);
						searchComposite = createSearchComposite(connection,mainComposite, entity);
						resultComposite.setQueryManager(searchComposite
								.getQueryManager());
						
					}
					formatSearchComposite(searchComposite);
					formatResultComposite(resultComposite);
					resultComposite.setBackground(parent.getBackground());
					searchComposite.setBackground(parent.getBackground());

				}

				searchComposite.getQueryManager().setResultListener(resultComposite);

			}
		} else {
			new BasicResultNotAllowedComposite(mainComposite);
		}		
	}
	
	protected void formatSearchComposite(Composite c){
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		c.setLayoutData(gridData);		
	}
	
	protected void formatResultComposite(Composite c){
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		c.setLayoutData(gridData);
	}	
	
	public Composite getMainComposite(){
		return mainComposite;
	}
	
	
	protected int[] getWeights(){
		return new int[] { 28, 100 };
	}
	

	
	protected AbstractResultComposite createResultComposite(Composite parent, String selectClause,
			QueryManager queryManager,ServerConnection connection) {
		return new BasicResultComposite(parent, entity, selectClause, queryManager,connection);
	}

	public MetaDataEntity getStructure() {
		return entity;
	}

	/**
	 * Redefine this method in the inherited classes to create your own "searchComposite".
	 * 
	 * @param parent
	 *            : Composite : The parent composite of the search Editor
	 * @return AbstractSearchComposite : A composite inherited from AbstractSearchComposite
	 */
	public abstract AbstractSearchComposite createSearchComposite(ServerConnection connection,
			Composite parent, MetaDataEntity newStructure);

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
	
	/**
	 * Re-create Result Composite , and re-fill it with current data
	 */
	public void resetResultComposite() {
		// Recreate result composite
		
		// Get parent to be cleaned of current resultComposite	
		Composite listAndEditor = resultComposite.getParent();
		// store current input
		Object input = resultComposite.getInput();		
		Object gridData = resultComposite.getLayoutData();

		// Dispose old result composite
		resultComposite.dispose();
		
		// Recreate the new one
		resultComposite = createResultComposite(listAndEditor, getDisplayedSelectClause(), searchComposite.getQueryManager(),connection);
		resultComposite.contentChanged((BeanMapList) input);
		
		resultComposite.setBackground(listAndEditor.getBackground());
		
		resultComposite.setLayoutData(gridData);
		if (listAndEditor instanceof SashForm){
		 ((SashForm)listAndEditor).setWeights(getWeights());
		}

		// Reset query manager listener
		searchComposite.getQueryManager().setResultListener(resultComposite);
		
		// Force layout of parent
		listAndEditor.layout(true);	
	}

	/**
	 * Reset actions, or change their enability
	 */
	protected void resetActions() {
	}
	
	/**
	 * Provide specific actions to container
	 * 
	 * @return
	 */
	public List<Action> getActions() {
		return new ArrayList<Action>();
	}
	
	
	protected int getSashOrientation() {
		return SWT.HORIZONTAL;		
	}
	
	protected boolean shashed(){
		return true;
	}
	
	protected boolean searchCompositeFirst(){
		return true;
	}	
	
}
