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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.client.core.IACCMessages;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.metadata.MetaDataEntity;

public class ListCompositeAdapter extends AbstractSearchListComposite {

	public ListCompositeAdapter(Composite parent,
			MetaDataEntity entityStructure, ServerConnection connection, boolean displayInformationPanel) {
		super(parent, entityStructure, connection, displayInformationPanel);
	}

	@Override
	public List<Action> getActions() {
		return null;
	}

	@Override
	protected UserMessage getSearchErrorMessage() {
		return UserMessageManager.getInstance().getMessage(IACCMessages.ERR_SRH_UNKNOWNERROR);
	}

	@Override
	protected Bundle getParentBundle() {
		return Activator.getDefault().getBundle();
	}

	@Override
	protected String createSelectClause() {
		return null;
	}

	@Override
	protected String createSearchClause() {
		return null;
	}

	@Override
	protected String createOrderClause() {
		return ""; //$NON-NLS-1$
	}

	@Override
	protected Image getElementIcon(Object element) {
		return null;
	}

}
