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
package com.arcadsoftware.osgi;

import java.io.File;

/**
 * Theses methods are called when file System change occurs.
 * 
 * @see FileSystemTracker
 */
public interface IFileSystemTrackerCallBack {

	/**
	 * Called when a new file is added.
	 * 
	 * @param tracker The FileSystemTracker.
	 * @param name File name relative to the base directory.
	 * @param file The external file to be added.
	 * @return False if the operation must be delayed.
	 */
	public boolean addFile(FileSystemTracker tracker, String name, File file);
	
	/**
	 * Called when a new file is changed.
	 * 
	 * @param tracker The FileSystemTracker.
	 * @param name File name relative to the base directory.
	 * @param file The external file to be modified.
	 */
	public void updateFile(FileSystemTracker tracker, String name, File file);
	
	/**
	 * Called when a new file is removed.
	 * 
	 * @param tracker The FileSystemTracker.
	 * @param name File name relative to the base directory.
	 * @param file The external file to be removed.
	 */
	public void removeFile(FileSystemTracker tracker, String name, File file);
	
}
