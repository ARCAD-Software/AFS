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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;

public abstract class AbstractEnumerationSelectorDialog extends AbstractBeanMapListSelectorDialog {

	public AbstractEnumerationSelectorDialog(Shell parentShell,
			ServerConnection connection, boolean multiselection) {
		super(parentShell, connection, multiselection);
	}

	@Override
	public BeanMapList getInput() {
		final ISearchCriteria searchCriteria = getSearchCriteria();
		if (searchCriteria == null) {
			return helper.getList(getType(), getAttributeList());
		} else {
			return helper.getList(getType(), getAttributeList(), searchCriteria);
		}
	}

	@Override
	public Point getSize() {
		return new Point(600, 500);
	}

	protected ISearchCriteria getSearchCriteria() {
		return null;
	}

}
