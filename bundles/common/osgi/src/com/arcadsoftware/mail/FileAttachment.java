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
package com.arcadsoftware.mail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import com.arcadsoftware.osgi.internal.Activator;

/**
 *
 */
public class FileAttachment implements Attachment {

	private File file;
	private String filename;

	public FileAttachment() {
		super();
	}
	
	public FileAttachment(File file, String filename) {
		super();
		this.file = file;
		this.filename = filename;
	}
	
	public File getFile() {
		return file;
	}
	
	public void setFile(File file) {
		this.file = file;
	}
	
	public String getFileName() {
		return filename;
	}
	
	public void setFileName(String filename) {
		this.filename = filename;
	}

	public boolean isValid() {
		return hasContent() && hasFileName();
	}
	
	public boolean hasContent() {
		return (file != null) && file.isFile();
	}
	
	public boolean hasFileName() {
		return (filename != null) && (filename.length() > 0);
	}

	public byte[] getContent() {
		InputStream is = getContentStream();
		if (is == null) {
			return new byte[0];
		}
		try {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			try {
				int b = is.read();
				while (b >= 0) {
					bos.write(b);
					b = is.read();
				}
				return bos.toByteArray();
			} catch (IOException e) {
				Activator.getInstance().error(e.getLocalizedMessage(), e);
				return new byte[0];
			} finally {
				try {
					bos.close();
				} catch (IOException e) {
					Activator.getInstance().debug(e.getLocalizedMessage(), e);
				}
			}
		} finally {
			try {
				is.close();
			} catch (IOException e) {
				Activator.getInstance().debug(e.getLocalizedMessage(), e);
			}
		}
	}

	public InputStream getContentStream() {
		try {
			return new FileInputStream(file);
		} catch (FileNotFoundException e) {
			Activator.getInstance().error(e.getLocalizedMessage(), e);
			return null;
		}
	}

	public String getMimeType() {
		String ext = file.getName();
		if (ext.indexOf('.') >= 0) {
			ext = ext.substring(ext.lastIndexOf('.'));
		}
		return MIME2FileExtension.getFileMimeTypeFromExtension(ext);
	}

	public String getName() {
		return getFileName();
	}

	public long getSize() {
		if ((file == null) || !file.isFile()) {
			return 0;
		}
		return file.length();
	}
}
