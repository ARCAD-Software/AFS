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

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.core.ui.editors.ConnectedDynamicEditor;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.inputs.TableWithButtonBarSWTProvider;
import com.arcadsoftware.editor.swt.DynamicEditorPart;
import com.arcadsoftware.editor.swt.IBeanMapErrorOnSaveListener;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.metadata.MetaDataLink;

public abstract class AbstractConnectedTableWithButtonBarSWTProvider extends TableWithButtonBarSWTProvider
		implements IBeanMapErrorOnSaveListener, ISecuredAction {

	private DataAccessHelper helper = null;

	public DataAccessHelper getHelper() {
		if (helper == null) {
			if (getConnection() != null) {
				helper = new DataAccessHelper(getConnection());
			}
		}
		return helper;
	}

	public ServerConnection getConnection() {
		final ISWTDataLoader dataLoader = renderer.getDataLoader();
		if (dataLoader instanceof CoreContentLoader) {
			final CoreContentLoader loader = (CoreContentLoader) dataLoader;
			final ServerConnection connection = loader.getConnection();
			return connection;
		}
		return null;
	}

	@Override
	protected boolean editBeanMap(BeanMap beanMap) {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			if (editionAllowed()) {
				final String layoutName = getEditionLayoutName();
				DynamicEditorPart editor;
				if (layoutName.length() == 0) {
					editor = (DynamicEditorPart) ConnectedDynamicEditor.openConnectedEditor(connection, beanMap);
				} else {
					editor = (DynamicEditorPart) ConnectedDynamicEditor.openConnectedEditor(connection, beanMap,
							layoutName);
				}

				// listen to save error
				editor.addErrorOnSaveListener(this);

				if (addAdditionalValues(beanMap)) {
					editor.refreshEditorContent();
				}
				return true;
			} else {
				missingRight(getExpectedEditRight());
			}
		}
		return false;
	}

	@Override
	protected void createBeanMap(MetaDataLink link, boolean withOpenEditor) {
		if (creationAllowed()) {
			super.createBeanMap(link, withOpenEditor);
		} else {
			missingRight(getExpectedEditRight());
		}
	}

	@Override
	protected void removeBeanMap(MetaDataLink link) {
		if (deletionAllowed()) {
			super.removeBeanMap(link);
		} else {
			missingRight(getExpectedEditRight());
		}
	}

	protected boolean addAdditionalValues(BeanMap beanMap) {
		return false;
	}

	@Override
	public void onErrorOnSave(BeanMap beanmap, String errorMessage) {
		MessageDialog.openError(Activator.getDefault().getPluginShell(),
				"", errorMessage);
	}

	@Override
	public void onErrorOnSave(BeanMap beanmap, UserMessage errorUserMessage) {
	}

	@Override
	public boolean isAllowed() {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			return connection.isAllowed(getExpectedRigths());
		}
		return false;
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return getExpectedEditRight();
	}

	public boolean editionAllowed() {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			final boolean result = connection.isAllowed(getExpectedEditRight());
			if (!result) {
				Activator.getDefault().missingRight(getExpectedEditRight());
			}
			return result;
		}
		return false;
	}

	public boolean creationAllowed() {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			return connection.isAllowed(getExpectedAddRight());
		}
		return false;
	}

	public boolean deletionAllowed() {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			return connection.isAllowed(getExpectedDeleteRight());
		}
		return false;
	}

	public abstract List<Integer> getExpectedEditRight();

	public abstract List<Integer> getExpectedAddRight();

	public abstract List<Integer> getExpectedDeleteRight();

	public abstract void missingRight(List<Integer> expected);

}
