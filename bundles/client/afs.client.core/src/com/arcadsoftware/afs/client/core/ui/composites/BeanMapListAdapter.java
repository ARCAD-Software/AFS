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

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.messages.UserMessage;

public class BeanMapListAdapter extends AbstractBeanMapListComposite {

	public BeanMapListAdapter(Composite parent, String entityType,
			ServerConnection connection) {
		super(parent, entityType, connection);
	}

	@Override
	public List<Action> getActions() {
		return null;
	}

	@Override
	protected UserMessage getSearchErrorMessage() {
		return null;
	}

	@Override
	protected Bundle getParentBundle() {
		return null;
	}

	@Override
	protected String createSelectClause() {
		return null;
	}

	@Override
	protected Image getElementIcon(Object element) {
		return null;
	}

}
