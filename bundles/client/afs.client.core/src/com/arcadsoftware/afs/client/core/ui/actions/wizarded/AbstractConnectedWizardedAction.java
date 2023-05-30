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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.aev.core.ui.actions.AbstractSimpleItemWithWizardAction;
import com.arcadsoftware.aev.core.ui.wizards.AbstractSimpleItemWizardPage;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.actions.IActionActivationManager;
import com.arcadsoftware.afs.client.core.ui.actions.IBeanMapActionListener;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.core.ui.wizards.AbstractConnectedWizardPage;
import com.arcadsoftware.afs.framework.services.IDynamicHelpIdProvider;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.beanmap.BeanMap;

/**
 * This class define an action that performs an operation onto a data (BeanMap).
 * 
 * <p>
 * This operation is proceeded into a wizard that will be defined by the implementation of this class.
 * This may be a creation or edition operation generally associated to "Add" or any kind of "Edit" actions.
 *    
 * @author ARCAD Software
 */
public abstract class AbstractConnectedWizardedAction extends AbstractSimpleItemWithWizardAction 
implements ISecuredAction, IActionActivationManager, IDynamicHelpIdProvider {

	protected BeanMap currentBeanMap;
	protected ServerConnection connection;
	protected DataAccessHelper helper;
	protected ArrayList<AbstractConnectedWizardPage> pageList;
	protected List<IBeanMapActionListener> listeners = null;

	public AbstractConnectedWizardedAction(ServerConnection connection) {
		this.connection = connection;
		helper = new DataAccessHelper(connection);
	}

	/**
	 * Called before the record operation.
	 * <p>
	 * If <code>false</code> is returned then the record operation is cancelled and the
	 * <code>doAfterSaving()</code> method is not called.
	 * 
	 * @param beanmap
	 * @return
	 */
	public boolean doBeforeSaving(BeanMap beanmap) {
		return true;
	}

	/**
	 * Called after the record operation.
	 * 
	 * <p>
	 * Into standard Wizards the returned value is not used.
	 * 
	 * @param beanmap
	 * @return 
	 */
	public boolean doAfterSaving(BeanMap beanmap) {
		return true;
	}
	
	@Override
	protected void doAfterRun() {
		if (isRunOk()){
			if (listeners != null && currentBeanMap != null && getActionType() != IBeanMapActionListener.ACTION_NONE){
				for (IBeanMapActionListener listener : listeners) {
					listener.actionDone(getActionType(), currentBeanMap);
				}
			}
		}
	}

	@Override
	public ArcadEntity getItem() {
		return null;
	}

	@Override
	protected void doInitialize() {
		super.doInitialize();
		
	}
	
	@Override
	public AbstractSimpleItemWizardPage[] getPages() {
		pageList = new ArrayList<AbstractConnectedWizardPage>();
		addConnectedWizardPages(connection);
		AbstractSimpleItemWizardPage[] result = new AbstractSimpleItemWizardPage[pageList.size()];
		int i = 0;
		for (AbstractConnectedWizardPage page:pageList)  {
			result[i++]=page;
		}
		return result;
	}	
	
	/**
	 * Use this method to add a wizard page.
	 * 
	 * <p>
	 * The <code>initBeanMap</code> of the page is called as soon as the 
	 * page is added to the wizard.
	 * 
	 * @param page
	 */
	public void addConnectedWizardPage(AbstractConnectedWizardPage page) {
		page.initBeanMap(currentBeanMap);
		pageList.add(page);
	}
	
	/**
	 * @return the currently data associated to this action execution.
	 */
	public BeanMap getCurrentBeanMap(){
		return currentBeanMap;
	}
	
	@Override
	protected boolean canExecute() {
		boolean result =  super.canExecute();
		if (result) {
			result = isAllowed();
			if (! result){
//				LogUITools.logError(Activator.getDefault().getBundle(), 
//						UserMessageManager.getInstance().getMessage(IACCMessages.ERR_ACTION_NO_RIGHT));
//				MessageDialog.openError(Activator.getDefault().getPluginShell(), 
//						getText(),
//						Activator.resString("msg.error.right.missing")); //$NON-NLS-1$
				Activator.getDefault().missingRight(getExpectedRigths());
			}
		}
		return result;
	}
	
	public boolean isAllowed() {
		return connection.isAllowed(getExpectedRigths());
	}
	
	protected int getActionType(){
		return IBeanMapActionListener.ACTION_NONE;
	}
	
	public void addActionListener(IBeanMapActionListener listener){
		if (listeners == null){
			listeners = new ArrayList<IBeanMapActionListener>();
		}
		if (!listeners.contains(listener)){
			listeners.add(listener);
		}
	}
	
	public boolean allowMultiSelection() {
		return true;
	}
	
	public boolean isAvailable() {
		return canExecute();
	}

	/**
	 * Get Dynamic Help Id
	 * @return
	 */
	public String getDynamicHelpId() {
		return null;
	}
	
	
	protected void doAfterWizardCreation(Dialog dialog){
		if (getDynamicHelpId() != null){
			DynamicHelp.updateContextHelpId(getDynamicHelpId(), dialog.getShell());
		}
	}
	
	/**
	 * This method is used to initialize the beanMap
	 * before display the wizard
	 * @param beanmap
	 */
	public abstract void initBeanMap(BeanMap beanmap);
	
	/**
	 * Override this method to add the pages to the wizard.
	 * 
	 * <p>
	 * To do so use the <code>addConnectedWizardPagew</code> method.
	 * 
	 * <p>
	 * You can use the <code>getCurrentBeanMap()</code> when adding pages.
	 * 
	 * @param connexion The current server connexion.
	 * @see #addConnectedWizardPage(AbstractConnectedWizardPage)
	 * @see #getCurrentBeanMap()
	 */
	public abstract void addConnectedWizardPages(ServerConnection connexion);
	
}
