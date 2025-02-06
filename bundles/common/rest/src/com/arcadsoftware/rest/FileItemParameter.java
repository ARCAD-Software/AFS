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
package com.arcadsoftware.rest;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.InvalidFileNameException;
import org.restlet.data.Parameter;

/**
 * This class is used to manage the File passed through a <code>getRequestForm()</code>.
 * <p>
 * This class is a Facade of the <b>org.apache.commons.fileupload.FileItem</b> class.
 * <p>
 * To avoid an undesired load of the file content in memory all method accessing to the "value" of this parameter will
 * return an empty string. use getFileString(...) method to get it. Please not that loading the whole file content in
 * memory is a security thread.
 * 
 * @author ARCAD Software
 * @see BaseResource#getRequestForm()
 */
public class FileItemParameter extends Parameter {

	private final FileItem item;

	public FileItemParameter(FileItem item) {
		super(item.getFieldName(), ""); //$NON-NLS-1$
		this.item = item;
	}

	/**
	 * Returns an {@link java.io.InputStream InputStream} that can be used to retrieve the contents of the file.
	 *
	 * @return An {@link java.io.InputStream InputStream} that can be used to retrieve the contents of the file.
	 * @throws IOException
	 *             if an error occurs.
	 */
	public InputStream getInputStream() throws IOException {
		return item.getInputStream();
	}

	/**
	 * Returns the content type passed by the browser or <code>null</code> if not defined.
	 *
	 * @return The content type passed by the browser or <code>null</code> if not defined.
	 */
	public String getFileContentType() {
		return item.getContentType();
	}

	/**
	 * Returns the original filename in the client's filesystem, as provided by the browser (or other client software).
	 * In most cases, this will be the base file name, without path information. However, some clients, such as the
	 * Opera browser, do include path information.
	 *
	 * @return The original filename in the client's filesystem.
	 * @throws InvalidFileNameException
	 *             The file name contains a NUL character, which might be an indicator of a security attack. If you
	 *             intend to use the file name anyways, catch the exception and use InvalidFileNameException#getName().
	 */
	public String getFileName() {
		return item.getName();
	}

	/**
	 * Provides a hint as to whether or not the file contents will be read from memory.
	 *
	 * @return <code>true</code> if the file contents will be read from memory; <code>false</code> otherwise.
	 */
	public boolean isFileInMemory() {
		return item.isInMemory();
	}

	/**
	 * Returns the size of the file item.
	 *
	 * @return The size of the file item, in bytes.
	 */
	public long getFileSize() {
		return item.getSize();
	}

	/**
	 * Returns the contents of the file item as an array of bytes.
	 * <p>
	 * Do not load a file in memory without checking it size.
	 * 
	 * @return The contents of the file item as an array of bytes.
	 */
	public byte[] getFileContent() {
		return item.get();
	}

	/**
	 * Returns the contents of the file item as a String, using the specified encoding. This method uses {@link #get()}
	 * to retrieve the contents of the item.
	 *
	 * @param encoding
	 *            The character encoding to use.
	 * @return The contents of the item, as a string.
	 * @throws UnsupportedEncodingException
	 *             if the requested character encoding is not available.
	 */
	public String getFileString(String encoding) throws UnsupportedEncodingException {
		return item.getString(encoding);
	}

	/**
	 * Returns the contents of the file item as a String, using the UTF-8 character encoding. This method uses
	 * {@link #get()} to retrieve the contents of the item.
	 *
	 * @return The contents of the item, as a string.
	 */
	public String getFileString() {
		try {
			return item.getString("UTF8"); //$NON-NLS-1$
		} catch (UnsupportedEncodingException e) {
			return item.getString();
		}
	}

	/**
	 * A convenience method to write an uploaded item to disk. The client code is not concerned with whether or not the
	 * item is stored in memory, or on disk in a temporary location. They just want to write the uploaded item to a
	 * file.
	 * <p>
	 * This method is not guaranteed to succeed if called more than once for the same item. This allows a particular
	 * implementation to use, for example, file renaming, where possible, rather than copying all of the underlying
	 * data, thus gaining a significant performance benefit.
	 *
	 * @param file
	 *            The <code>File</code> into which the uploaded item should be stored.
	 * @throws Exception
	 *             if an error occurs.
	 */
	public void writeFile(File file) throws Exception {
		item.write(file);
	}

	/**
	 * Deletes the underlying storage for a file item, including deleting any associated temporary disk file. Although
	 * this storage will be deleted automatically when the <code>FileItem</code> instance is garbage collected, this
	 * method can be used to ensure that this is done at an earlier time, thus preserving system resources.
	 */
	public void deleteTempStorage() {
		item.delete();
	}

	/**
	 * Returns an {@link java.io.OutputStream OutputStream} that can be used for storing the contents of the file.
	 *
	 * @return An {@link java.io.OutputStream OutputStream} that can be used for storing the contensts of the file.
	 * @throws IOException
	 *             if an error occurs.
	 */
	public OutputStream getFileOutputStream() throws IOException {
		return item.getOutputStream();
	}
}
