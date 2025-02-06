/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractConnectedWizardedWithParentAddAction extends
		AbstractConnectedWizardedAddAction {

	public AbstractConnectedWizardedWithParentAddAction(
			ServerConnection connexion) {
		super(connexion);
	}

	@Override
	public void initBeanMap(BeanMap beanmap) {
		final BeanMap parent = getParent();
		if (parent != null) {
			beanmap.put(getParentAttribute(), getParentValue(parent));
		}
	}

	@Override
	protected boolean canExecute() {
		final boolean result = super.canExecute();
		if (result) {
			final BeanMap parent = getParent();
			return matchingCondition(parent);
		}
		return result;
	}

	public boolean matchingCondition(BeanMap parent) {
		if (parent != null) {
			return (parent.getId() > 0);
		} else {
			return false;
		}
	}

	public Object getParentValue(BeanMap parent) {
		return parent.getId();
	}

	public abstract String getParentAttribute();

	public abstract BeanMap getParent();

}
