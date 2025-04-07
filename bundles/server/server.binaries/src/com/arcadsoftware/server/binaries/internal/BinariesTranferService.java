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
package com.arcadsoftware.server.binaries.internal;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.osgi.IBinariesTranferService;

/**
 * Implement the binaries file transfer processor.
 * 
 * This implementation is a local implementation.
 * 
 */
public class BinariesTranferService implements IBinariesTranferService {

	private final Activator activator;

	public BinariesTranferService(Activator activator) {
		super();
		this.activator = activator;
	}

	/**
	 * File copying with java NIO !
	 * 
	 * @param source
	 * @param target
	 * @return
	 */
	private boolean copy(File source, File target) {
		try {
			FileInputStream fis = new FileInputStream(source);
			try {
				FileChannel inc = fis.getChannel();
				try {
					FileOutputStream fos = new FileOutputStream(target);
					try {
						FileChannel outc = fos.getChannel();
						try {
							long size = source.length();
							long count = 0;
							while (size > 0) {
								long result = inc.transferTo(count, size, outc);
								if (result == 0) {
									break;
								}
								count += result;
								size -= result;
							}
							if (size > 0) {
								Activator.getInstance().error("File copy failed, incomplete file transfert (" + size + "b not copied).");
								return false;
							}
						} finally {
							outc.close();
						}
					} finally {
						fos.close();
					}
				} finally {
					inc.close();
				}
			} finally {
				fis.close();
			}
			return true;
		} catch (Exception e) {
			Activator.getInstance().error(Messages.BinariesTranferService_Error_copy, e);
			return false;
		}
	}

	/**
	 * Create the new file...
	 * 
	 * @param category
	 * @param id
	 * @param file
	 * @return
	 */
	private File getNewFileName(String category, int id, File file) {
		String name = file.getName();
		int i = name.indexOf('_') + 1;
		if ((i > 0) && (i < (name.length() - 1))) {
			name = name.substring(i);
		} else {
			name = ""; //$NON-NLS-1$
		}
		File result = new File(activator.getSubDir(category, id), Integer.toString(id) + '_' + name); //$NON-NLS-1$
		// Test a directory path traversal attack...
		try {
			if (!result.getCanonicalPath().startsWith(activator.getPath().getCanonicalPath())) {
				activator.error("Invalid Path name : '" + result.getAbsolutePath() + "' does apears to be contained in: " + activator.getPath().getAbsolutePath());
				return null;
			}
		} catch (IOException e) {
			activator.error("Unable to get canonical path of: '" + result.getAbsolutePath() + "' does apears to be contained in: " + activator.getPath().getAbsolutePath(), e);
			return null;
		}
		File parent = result.getParentFile();
		if (parent != null) {
			parent.mkdirs();
		}
		return result;
	}

	@Override
	public String generateKey(String category, int id, boolean readOnly) {
		return activator.store(category, id, readOnly);
	}

	@Override
	public List<Integer> listCategory(String category) {
		File dir = activator.getDir(category);
		ArrayList<Integer> result = new ArrayList<Integer>();
		if (dir.isDirectory()) {
			for (File sdir : dir.listFiles()) {
				if (sdir.isDirectory()) {
					for (File file : dir.listFiles()) {
						if (file.isFile()) {
							String[] s = file.getName().split("_"); //$NON-NLS-1$
							if ((s.length > 0) && (s[0].length() > 0)) {
								try {
									Integer i = Integer.parseInt(file.getName());
									result.add(i);
								} catch (NumberFormatException e) {
									activator.debug(e.getLocalizedMessage());
								}
							}
						}
					}
				}
			}
		}
		return result;
	}

	@Override
	public int newFileId(String category) {
		return activator.getNewFileId(category);
	}

	@Override
	public int moveFileId(String newCategory, String oldCategory, int id) {
		File source = activator.getFile(oldCategory, id);
		if ((source != null) && source.isFile()) {
			int nid = activator.getNewFileId(newCategory);
			if (nid >= 0) {
				File target = getNewFileName(newCategory, nid, source);
				if ((target != null) && source.renameTo(target)) {
					activator.fileEventDel(oldCategory, id, source);
					activator.fileEventNew(newCategory, nid, target);
					return nid;
				}
			}
		}
		return 0;
	}

	@Override
	public int newFileId(String newCategory, String oldCategory, int id) {
		File source = activator.getFile(oldCategory, id);
		if ((source != null) && source.isFile()) {
			int nid = activator.getNewFileId(newCategory);
			if (nid >= 0) {
				File target = getNewFileName(newCategory, nid, source);
				if ((target != null) && copy(source, target)) {
					activator.fileEventNew(newCategory, nid, target);
					return nid;
				}
			}
		}
		return 0;
	}

	@Override
	public boolean removeFile(String category, int id) {
		File file = activator.getFile(category, id);
		if ((file != null) && file.isFile()) {
			if (file.delete()) {
				activator.fileEventDel(category, id, file);
				return true;
			}
		}
		return false;
	}

	@Override
	public File generateTempFile(String category, int id) {
		// We should copy the file into a temporary directory.
		// Distant repository need to access to the web-service.
		File result = activator.getFile(category, id);
		if ((result != null) && result.isFile()) {
			return result;
		}
		return null;
	}

	@Override
	public boolean newFile(String category, int id, File file) {
		if ((category != null) && (file != null) && file.isFile()) {
			if (activator.removeFiles(category, id)) {
				activator.fileEventDel(category, id, null);
			}
			File target = new File(activator.getSubDir(category, id), Integer.toString(id) + '_' + file.getName());
			// Test a directory path trasversal attack...
			try {
				if (!target.getCanonicalPath().startsWith(activator.getPath().getCanonicalPath())) {
					activator.error("Invalid Path name : '{}' does apears to be contained in: {}.", target.getAbsolutePath(), activator.getPath().getAbsolutePath());
					return false;
				}
			} catch (IOException e) {
				activator.error("Unable to get canonical path of: '{}' does apears to be contained in: {}.", target.getAbsolutePath(), activator.getPath().getAbsolutePath(), e);
				return false;
			}
			// Ensure that parent exists
			File parent = target.getParentFile();
			if ((parent != null) && !parent.exists()) {
				parent.mkdirs();
				if (!parent.exists()) {
					Activator.getInstance().error("The Binary file '{}' could not be created.", parent.getAbsolutePath());
				}
			}
			if (copy(file, target)) {
				activator.fileEventNew(category, id, target);
				return true;
			} else {
				activator.error("Unable to copy file: '{}'.", target.getAbsolutePath());
			}
		}
		return false;
	}

	@Override
	public boolean newFile(String category, int id, InputStream stream, String filename) {
		if ((category != null) && (stream != null) && (filename != null)) {
			if (activator.removeFiles(category, id)) {
				activator.fileEventDel(category, id, null);
			}
			try {
				File target = new File(activator.getSubDir(category, id), Integer.toString(id) + '_' + filename);
				// Test a directory path trasversal attack...
				try {
					if (!target.getCanonicalPath().startsWith(activator.getPath().getCanonicalPath())) {
						activator.error("Invalid Path name : '" + target.getAbsolutePath() + "' does apears to be contained in: " + activator.getPath().getAbsolutePath());
						return false;
					}
				} catch (IOException e) {
					activator.error("Unable to get canonical path of: '" + target.getAbsolutePath() + "' does apears to be contained in: " + activator.getPath().getAbsolutePath(), e);
					return false;
				}
				if (!target.getParentFile().exists()) {
					target.getParentFile().mkdirs();
				}
				FileOutputStream out = new FileOutputStream(target);
				try {
					BufferedInputStream bin = new BufferedInputStream(stream);
					BufferedOutputStream bout = new BufferedOutputStream(out);
					try {
						while (true) {
							int datum = bin.read();
							if (datum == -1)
								break;
							bout.write(datum);
						}
						bout.flush();
					} finally {
						bout.close();
					}
				} finally {
					out.close();
				}
				activator.fileEventNew(category, id, target);
				return true;
			} catch (Throwable e) {
				Activator.getInstance().error(Messages.BinariesTranferService_Error_copy, e);
			}
		}
		return false;
	}
	
	@Override
	public String getRootPath() {
		return activator.getPath().getAbsolutePath();
	}

}
