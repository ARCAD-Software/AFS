/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
import com.arcadsoftware.afs.client.core.ui.wizards.ConnectedWizard;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public abstract class AbstractConnectedWizardedMultiAddAction extends AbstractConnectedWizardedAction {
	protected BeanMapList currentBeanMaps;

	public AbstractConnectedWizardedMultiAddAction(ServerConnection connexion) {
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
	}

	@Override
	public boolean runActions() {
		boolean result = true;
		currentBeanMaps = wizardToBeanmaps();
		if ((currentBeanMaps != null) && (currentBeanMaps.size() > 0)) {
			for (final BeanMap beanMap : currentBeanMaps) {
				if (doBeforeSaving(beanMap)) {
					if (saveBeanMap(beanMap)) {
						doAfterSaving(beanMap);
					} else {
						result = false;
					}
				}
			}

			if (!result) {
				handleError();
			}
		} else {
			result = false;
		}
		return result;
	}

	public boolean saveBeanMap(BeanMap beanMap) {
		helper.create(beanMap);
		return true;
	}

	@Override
	protected void doAfterRun() {
		if (isRunOk()) {
			if ((listeners != null) && (currentBeanMaps != null)
					&& (getActionType() != IBeanMapActionListener.ACTION_NONE)) {
				for (final IBeanMapActionListener listener : listeners) {
					listener.actionDone(getActionType(), currentBeanMaps);
				}
			}
		}
	}

	public abstract BeanMapList wizardToBeanmaps();

	@Override
	protected int getActionType() {
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
	 *
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
