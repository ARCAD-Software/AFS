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
package com.arcadsoftware.server.binaries.internal;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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

	private Activator activator;

	/**
	 * @param activator
	 */
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
		activator.test(category, id);
		String name = file.getName();
		int i = name.indexOf('_') + 1;
		if ((i > 0) && (i < (name.length() - 1))) {
			name = name.substring(i);
		} else {
			name = ""; //$NON-NLS-1$
		}
		File result = new File(activator.getFileNamePrefix(category, id) + name);
		File parent = result.getParentFile();
		if (parent != null) {
			parent.mkdirs();
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#generateKey (java.lang.String, int, boolean)
	 */
	public String generateKey(String category, int id, boolean readOnly) {
		activator.test(category, id);
		return activator.store(category, id, readOnly);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#listCategory (java.lang.String)
	 */
	public List<Integer> listCategory(String category) {
		File dir = new File(activator.getDirName(category));
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#newFileId(java .lang.String)
	 */
	public int newFileId(String category) {
		return activator.getNewFileId(category);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#moveFileId( java.lang.String, java.lang.String,
	 * int)
	 */
	public int moveFileId(String newCategory, String oldCategory, int id) {
		File source = activator.getFile(oldCategory, id);
		if ((source != null) && source.isFile()) {
			int nid = activator.getNewFileId(newCategory);
			File target = getNewFileName(newCategory, nid, source);
			if (source.renameTo(target)) {
				activator.fileEventDel(oldCategory, id, source);
				activator.fileEventNew(newCategory, nid, target);
				return nid;
			}
		}
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#newFileId(java .lang.String, java.lang.String,
	 * int)
	 */
	public int newFileId(String newCategory, String oldCategory, int id) {
		File source = activator.getFile(oldCategory, id);
		if ((source != null) && source.isFile()) {
			int nid = activator.getNewFileId(newCategory);
			File target = getNewFileName(newCategory, nid, source);
			if (copy(source, target)) {
				activator.fileEventNew(newCategory, nid, target);
				return nid;
			}
		}
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#removeFile( java.lang.String, int)
	 */
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#generateTempFile(java.lang.String, int)
	 */
	public File generateTempFile(String category, int id) {
		// We should copy the file into a temporary directory.
		// Distant repository need to access to the web-service.
		File result = activator.getFile(category, id);
		if ((result != null) && result.isFile()) {
			return result;
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#newFile(java.lang.String, int, java.io.File)
	 */
	public boolean newFile(String category, int id, File file) {
		if ((category != null) && (file != null) && file.isFile()) {
			if (activator.removeFiles(category, id)) {
				activator.fileEventDel(category, id, null);
			}
			activator.test(category, id);
			if (file.isFile()) {
				File target = new File(activator.getFileNamePrefix(category, id) + file.getName());
				// Ensure that parent exists
				File parent = target.getParentFile();
				if ((parent != null) && !parent.exists()) {
					parent.mkdirs();
					if (!parent.exists()) {
						Activator.getInstance().log(Activator.LOG_ERROR, parent.getAbsolutePath() + " could not be created");
					}
				}
				if (copy(file, target)) {
					activator.fileEventNew(category, id, target);
					return true;
				}
			}
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#newFile(java.lang.String, int,
	 * java.io.InputStream, java.lang.String)
	 */
	public boolean newFile(String category, int id, InputStream stream, String filename) {
		if ((category != null) && (stream != null) && (filename != null)) {
			if (activator.removeFiles(category, id)) {
				activator.fileEventDel(category, id, null);
			}
			activator.test(category, id);
			try {
				File target = new File(activator.getFileNamePrefix(category, id) + filename);
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
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.server.binaries.IBinariesTranferService#ngetRootPath()
	 */
	
	@Override
	public String getRootPath() {
		return activator.getPath();
	}

}
