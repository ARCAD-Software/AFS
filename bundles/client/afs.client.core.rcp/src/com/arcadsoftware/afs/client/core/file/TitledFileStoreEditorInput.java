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
package com.arcadsoftware.afs.client.core.file;

import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.ui.ide.FileStoreEditorInput;

public class TitledFileStoreEditorInput extends FileStoreEditorInput {
	private String title;
	public TitledFileStoreEditorInput(IFileStore fileStore, String title) {
		super(fileStore);
		this.title = title;
	}
	
	@Override
	public String getName() {
		return title;
	}

	@Override
	public boolean equals(Object o) {
		// check if same file store
		if (o instanceof TitledFileStoreEditorInput){
			TitledFileStoreEditorInput obj = (TitledFileStoreEditorInput)o;
			String name = getName();
			if (name != null && name.equals(obj.getName())){
				return true;
			}
		}
		return super.equals(o);
	}
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
