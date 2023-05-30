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

import org.eclipse.core.runtime.IProgressMonitor;

public abstract class AbstractAFSNoSaveEditorPart extends AbstractAFSEditorPart {

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public boolean checkData() {
		return true;
	}

	@Override
	public void doSave(IProgressMonitor monitor) {}

}
