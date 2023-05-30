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
package com.arcadsoftware.afs.framework.ui.editors;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IPersistableElement;

import com.arcadsoftware.afs.framework.services.IDynamicHelpIdProvider;

public abstract class AbstractAFSEditorInput implements IEditorInput, IDynamicHelpIdProvider {

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Object getAdapter(Class adapter) {
		return null;
	}

	public boolean exists() {
		return false;
	}

	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	public IPersistableElement getPersistable() {
		return null;
	}

	public String getToolTipText() {
		return null;
	}
	
	public String getDynamicHelpId() {
		return null;
	}

}
