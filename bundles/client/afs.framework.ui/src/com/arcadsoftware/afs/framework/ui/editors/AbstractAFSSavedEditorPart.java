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
package com.arcadsoftware.afs.framework.ui.editors;

import org.eclipse.core.runtime.IProgressMonitor;

import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSEditorComposite;

public abstract class AbstractAFSSavedEditorPart extends AbstractAFSEditorPart {

	@Override
	public boolean isDirty() {
		for (final AbstractAFSEditorComposite child : childComposites) {
			if (child.isDirty()) {
				return true;
			}
		}
		return super.isDirty();
	}

	@Override
	public boolean checkData() {
		for (final AbstractAFSEditorComposite child : childComposites) {
			if (!child.checkData()) {
				return false;
			}
		}
		return true;
	}

	@Override
	public void doSave(IProgressMonitor monitor) {
		for (final AbstractAFSEditorComposite child : childComposites) {
			child.fromScreen();
		}
		saveContent(monitor);
	}

	public abstract void saveContent(IProgressMonitor monitor);

}
