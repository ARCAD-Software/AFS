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
package com.arcadsoftware.cleanup.operation;

import java.io.File;
import java.io.FilenameFilter;

/**
 * Basic directory cleaning operation. No conditions, the directorie's content always deleted.
 *
 * @author ARCAD Software
 */
public class BasicCleanDirectory extends AbstractCleanDirectory {

	public static BasicCleanDirectory newFilenameExtensionCleanDirectory(String id, File directory, final String extension) {
		return new BasicCleanDirectory(id, directory, new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return name.endsWith(extension);
			}
		});
	}

	public BasicCleanDirectory(String id, File directory) {
		super(id, directory);
	}

	public BasicCleanDirectory(String id, File directory, FilenameFilter filter) {
		super(id, directory, filter);
	}

	@Override
	protected boolean isDeletable(File file) {
		return true;
	}
}
