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
package com.arcadsoftware.afs.client.core.ui.widgets;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.client.editors.swtwidgets.widgets.AbstractSearchSWTProvider;
import com.arcadsoftware.client.editors.swtwidgets.widgets.ISearchBeanMap;
import com.arcadsoftware.editor.swt.ISWTDataLoader;

public abstract class AbstractConnectedSearchSWTProvider extends
		AbstractSearchSWTProvider {

	protected ServerConnection connection;

	@Override
	public ISearchBeanMap getSelector() {
		final ISWTDataLoader loader = renderer.getDataLoader();
		if (loader instanceof CoreContentLoader) {
			connection = ((CoreContentLoader) loader).getConnection();
		}
		return getConnectedSelector(connection);
	}

	public abstract ISearchBeanMap getConnectedSelector(ServerConnection connection);

}
