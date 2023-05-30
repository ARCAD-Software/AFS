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
package com.arcadsoftware.cleanup.operation;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Files;

public abstract class AbstractCleanDirectory extends AbstractCleanOperation {

	protected File directory;
	protected FilenameFilter filenameFilter;
	protected boolean ignoreConditions;

	public AbstractCleanDirectory(String id, File directory) {
		this(id, directory, null);
	}

	public AbstractCleanDirectory(String id, File directory, FilenameFilter filter) {
		super(id);
		this.directory = directory;
		if (filter != null) {
			filenameFilter = filter;
		}
	}

	@Override
	public boolean executeCleanOperation(boolean ignoreConditions) {
		this.ignoreConditions = ignoreConditions;
		return ((directory != null) && cleanDirectory(directory));
	}

	protected boolean cleanDirectory(File directory) {
		File fixedDirectory = null;
		cleanupManager.writeCleanupLog(String.format("Cleaning directory %1$s%n", directory));
		try {
			// resolve symlinks
			fixedDirectory = directory.getCanonicalFile();
			if (fixedDirectory.exists() && fixedDirectory.isDirectory()) {
				for (final File oFile : fixedDirectory.listFiles(filenameFilter)) {
					// resolve symlink
					final File aFile = oFile.getCanonicalFile();
					if (canDelete(aFile)) {
						final boolean fileIsSymLink = Files.isSymbolicLink(oFile.toPath());
						if (aFile.isDirectory() && (!fileIsSymLink || (fileIsSymLink && cleanupManager.mustFollowSymlinks()))) {
							// Clean files in directory
							// does not check return values ???
							cleanDirectory(aFile);
							// Delete the directory if its empty
							if (aFile.listFiles().length == 0) {
								if (aFile.delete()) {
									cleanupManager.writeCleanupLog(String.format("Deleting directory %1$s%n", aFile));
								} else {
									cleanupManager.writeCleanupLog(String.format("/!\\ Could not delete directory %1$s (%2$s)%n", aFile, oFile));
								}
								if (fileIsSymLink) {
									if (oFile.delete()) {
										cleanupManager.writeCleanupLog(String.format("Deleting symlink %1$s%n", oFile));
									} else {
										cleanupManager.writeCleanupLog(String.format("/!\\ Could not delete symlink %1$s (%2$s)%n", oFile, aFile));
									}
								}
							}
						} else {
							// be careful here if you have a symlink.
							if (oFile.delete()) {
								cleanupManager.writeCleanupLog(String.format("Deleting file %1$s%n", oFile));
							} else {
								cleanupManager.writeCleanupLog(String.format("/!\\ Could not delete file %1$s%n", oFile));
							}
						}
					}
				}
			} else {
				cleanupManager.writeCleanupLog(String.format("%1$s does not exist or is not a directory.%n", directory));
				return false;
			}
		} catch (final IOException e1) {
			cleanupManager.logException(e1);
			cleanupManager.writeCleanupLog(String.format("cleanDirectory problem: %1$s%n", e1.getMessage()));
			return false;
		}
		return true;
	}

	protected boolean canDelete(File file) {
		return (file.isDirectory() && isDeletable(file)) || ((file.lastModified() < cleanupManager.getReferenceDate().getTime()) && (ignoreConditions || isDeletable(file)));
	}

	protected abstract boolean isDeletable(File file);

	@Override
	public String toString() {
		try {
			return super.toString() + String.format(" [%1$s]", directory.getCanonicalPath()) + (filenameFilter != null ? " [filtered]" : "");
		} catch (final IOException e) {
			return super.toString();
		}
	}
}
