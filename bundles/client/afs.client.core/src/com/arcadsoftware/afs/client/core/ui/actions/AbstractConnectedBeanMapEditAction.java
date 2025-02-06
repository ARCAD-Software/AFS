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

import java.util.Map;

import org.eclipse.ui.IEditorPart;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.editors.ConnectedDynamicEditor;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapModifier;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.swt.IBeanMapErrorOnSaveListener;
import com.arcadsoftware.editor.swt.IBeanMapSavedListener;

public abstract class AbstractConnectedBeanMapEditAction extends AbstractConnectedBeanMapAction implements IBeanMapSavedListener, IBeanMapErrorOnSaveListener, IBeanMapModifier {

	private ConnectedDynamicEditor editor;

	public AbstractConnectedBeanMapEditAction(ServerConnection connection) {
		super(connection);
	}

	public AbstractConnectedBeanMapEditAction() {
		super();
	}

	protected boolean isReadOnly() {
		return false;
	}

	protected boolean isReadOnly(BeanMap bean) {
		return false;
	}

	protected void beanEdit(BeanMap beanMap) {
		if (beanMap != null) {
			doBeforeEditing(beanMap);
			IEditorPart e = ConnectedDynamicEditor.openConnectedEditor( //
					connection, //
					beanMap, //
					getLayoutName(beanMap), //
					getVirtualValues(beanMap), //
					this, //
					isReadOnly() || isReadOnly(beanMap));
			if (e instanceof ConnectedDynamicEditor) {
				editor = (ConnectedDynamicEditor) e;
				editor.addBeanMapSavedListener(this);
				editor.addErrorOnSaveListener(this);
				editor.setAllowExternalRefresh(isExternalRefreshAllowed());
			}
		}
	}

	public Map<String , Object> getVirtualValues(BeanMap beanMap) {
		return null;
	}
	
	protected boolean isExternalRefreshAllowed() {
		return true;
	}

	@Override
	protected boolean execute() {
		BeanMap b = getBeanMapToManage();
		if (b == null) {			
			final BeanMapList list = getBeanMapListToManage();
			if (list != null) {
				for (final BeanMap bean: list) {
					beanEdit(bean);
				}
			}
		} else {
			beanEdit(b);
		}
		return true;
	}

	protected String getLayoutName(BeanMap beanmap) {
		return getLayoutName();
	}

	protected String getLayoutName() {
		return null;
	}

	@Override
	public void beanMapSaved(BeanMap beanmap) {
		if (beanmap != null) {
			saved(beanmap);
		}
	}

	protected void saved(BeanMap beanMap) {
		doAfterSaving(beanMap);
	}
	
	protected final ConnectedDynamicEditor getConnectedDynamicEditor() {
		return editor;
	}
	
	protected void doBeforeEditing(BeanMap beanMap) {}

	protected void doAfterSaving(BeanMap beanMap) {}

	public void onErrorOnSave(BeanMap beanmap, String errorMessage) {}

	public void onErrorOnSave(BeanMap beanmap, UserMessage errorUserMessage) {}

	public void modify(BeanMap beanmap) {}
	
}
