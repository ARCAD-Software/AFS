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
package com.arcadsoftware.afs.client.core.file;

import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.ui.ide.FileStoreEditorInput;

public class FixedTitleFileStoreEditorInput extends FileStoreEditorInput {

	private String title;
	
	public FixedTitleFileStoreEditorInput(IFileStore fileStore, String title) {
		super(fileStore);
		this.title = title;
	}
	
	@Override
	public String getName() {
		return title;
	}

}
