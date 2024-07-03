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

import org.eclipse.jface.window.Window;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.dialogs.AbstractSimpleSearchBeanMapListDialog;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.widgets.AbstractSearchSWTProvider;
import com.arcadsoftware.client.editors.swtwidgets.widgets.ISearchBeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * Subclass this class to create a {@link AbstractSearchSWTProvider} where the benMap selection is based on a
 * dialog.<br/>
 * You just need to provide the dialog used to make the selection by implementing the {@link #getDialog()} method
 *
 * @author ARCAD Software
 */
public abstract class AbstractBeanMapSearchWithDialogSWTProvider extends AbstractSearchSWTProvider {

	protected ServerConnection connection = null;

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters,
			Element element, MetaDataEntity structure) {
		super.create(renderer, parameters, element, structure);
		final ISWTDataLoader loader = renderer.getDataLoader();
		if (loader instanceof CoreContentLoader) {
			connection = ((CoreContentLoader) loader).getConnection();
		}
	}

	private class BeanMapSelector implements ISearchBeanMap {
		@Override
		public BeanMap search(MetaDataEntity structure) {
			final AbstractSimpleSearchBeanMapListDialog dialog = getDialog(connection);
			if (dialog.open() == Window.OK) {
				return dialog.getSelected();
			}
			return null;
		}
	}

	@Override
	public ISearchBeanMap getSelector() {
		return new BeanMapSelector();
	}

	/**
	 * Returns the dialog used to make the BeanMap selection
	 *
	 * @return An object that subclasses {@link AbstractSimpleSearchBeanMapListDialog}
	 */
	public abstract AbstractSimpleSearchBeanMapListDialog getDialog(ServerConnection connection);

}
