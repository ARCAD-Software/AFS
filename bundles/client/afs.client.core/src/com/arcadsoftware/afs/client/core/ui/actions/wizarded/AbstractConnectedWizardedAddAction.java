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
package com.arcadsoftware.afs.client.core.ui.actions.wizarded;

import org.osgi.framework.Bundle;

import com.arcadsoftware.aev.core.ui.actions.AbstractSimpleItemWithWizardAction;
import com.arcadsoftware.aev.core.ui.wizards.AbstractSimpleItemWizard;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.IBeanMapActionListener;
import com.arcadsoftware.afs.client.core.ui.wizards.AbstractConnectedWizardPage;
import com.arcadsoftware.afs.client.core.ui.wizards.ConnectedWizard;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMap;

/**
 * Define an "Add" action, that will create a new data (BeanMap) through the several step of a wizard.
 * 
 * <p>
 * Implementation of this action need to define :
 * 
 * <ul>
 * <li> The type of the manipulated data.
 * <li> The pages of the associated wizard.
 * <li> A general error user message if the creation fail.
 * </ul>
 * 
 * @author ARCAD Software
 */
public abstract class AbstractConnectedWizardedAddAction extends
		AbstractConnectedWizardedAction {


	public AbstractConnectedWizardedAddAction(ServerConnection connexion) {
		super(connexion);
		setActionsToRunAfterWizard(false);
	}

	@Override
	public AbstractSimpleItemWizard createWizard(AbstractSimpleItemWithWizardAction action, String title) {
		return new ConnectedWizard(action, title);
	}

	protected BeanMap createBeanMap() {
		return new BeanMap(getType());
	}

	@Override
	protected void doInitialize() {
		currentBeanMap = createBeanMap();
		initBeanMap(currentBeanMap);
	}

	@Override
	public boolean runActions() {
		boolean result = false;
		if (wizardToBeanmap(currentBeanMap) && doBeforeSaving(currentBeanMap)) {
			if (saveBeanMap(currentBeanMap)){
				doAfterSaving(currentBeanMap);
				result = true;
			} else {
				handleError();
			}
		}
		return result;
	}
	
	public boolean saveBeanMap(BeanMap beanMap){
		return helper.create(beanMap);
	}
	
	
	public boolean wizardToBeanmap(BeanMap newCurrentBeanMap){
		for (AbstractConnectedWizardPage page:pageList)  {
			if (page.isLinkedToCurrentBeanMap()){
				page.screenToBeanMap(newCurrentBeanMap);
			}
		}
		return true;
	}
	
	@Override
	protected int getActionType(){
		return IBeanMapActionListener.ACTION_ADD;
	}
	/**
	 * This method is called if an error occurred during the BeanMap Creation;
	 */
	public void handleError() {
		LogUITools.logError(getBundle(), getErrorMessage()); 
	}
		
	/**
	 * Define the type of the created BeanMap
	 * @return the type of the BeanMp to create
	 */
	public abstract String getType();
	
	
	/**
	 * @return the Bundle from where the action is executed 
	 */
	public abstract Bundle getBundle();
	
	
	/**
	 * @return the error message to display when the BeanMap creation failed.
	 */
	public abstract UserMessage getErrorMessage();
	



}
