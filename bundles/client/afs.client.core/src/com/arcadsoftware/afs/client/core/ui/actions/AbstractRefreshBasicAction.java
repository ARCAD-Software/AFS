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
package com.arcadsoftware.afs.client.core.ui.actions;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.views.AbstractConnectedView;

public abstract class AbstractRefreshBasicAction extends ArcadAction {

	public AbstractRefreshBasicAction() {
		super();

	}

	public AbstractRefreshBasicAction(String id) {
		super();
		setId(id);
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("common.action.refresh.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("common.action.refresh.tooltip")); //$NON-NLS-1$
		setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
	}

	public abstract AbstractConnectedView getView();

}
