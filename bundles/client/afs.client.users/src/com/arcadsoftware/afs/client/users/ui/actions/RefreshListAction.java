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
package com.arcadsoftware.afs.client.users.ui.actions;

import org.eclipse.jface.viewers.TableViewer;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBeanMapAction;
import com.arcadsoftware.afs.client.users.internal.Activator;

public abstract class RefreshListAction extends AbstractConnectedBeanMapAction {

	public RefreshListAction(ServerConnection connection){
		super(connection);
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("common.action.refresh.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("common.action.refresh.tooltip")); //$NON-NLS-1$
		setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
	}
	@Override
	protected boolean canExecute() {
		return true;
	}

	@Override
	protected abstract boolean execute();
	
	protected TableViewer getViewer(){
		return null;
	}

	protected String getFilter() {
		return null;
	}
}
