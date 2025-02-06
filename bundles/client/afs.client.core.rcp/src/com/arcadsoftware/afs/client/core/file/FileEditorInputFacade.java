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
package com.arcadsoftware.afs.client.core.file;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class FileEditorInputFacade extends FileEditorInput {

	private IEditorInput facade;
	
	public FileEditorInputFacade(IFile file, IEditorInput input) {
		super(file);
		this.facade = input;
	}

	@Override
	public String getName() {
		if (facade!=null)
			return facade.getName();
		return super.getName();
	}
	
}
