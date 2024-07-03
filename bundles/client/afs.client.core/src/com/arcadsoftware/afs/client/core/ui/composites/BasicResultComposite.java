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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.managers.QueryManager;
import com.arcadsoftware.metadata.MetaDataEntity;

public class BasicResultComposite extends AbstractResultComposite {

	public BasicResultComposite(Composite parent, MetaDataEntity entityStructure, QueryManager queryManager,
			ServerConnection connection) {
		super(parent, entityStructure, queryManager, connection);
	}

	public BasicResultComposite(Composite parent, MetaDataEntity entityStructure,
			String selectClause, QueryManager queryManager, ServerConnection connection) {
		super(parent, entityStructure, selectClause, queryManager, connection);
	}

	@Override
	public List<Action> getActions() {
		return null;
	}

}
