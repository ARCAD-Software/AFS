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
package com.arcadsoftware.rest;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.GregorianCalendar;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.ext.fileupload.RestletFileUpload;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.osgi.AbstractFileRepositoryActivator;
import com.arcadsoftware.rest.internal.Messages;

/**
 * Resource associated to files from a "File repository".
 * 
 * @see AbstractFileRepositoryActivator
 */
public abstract class FileRepositoryResource extends UserLinkedResource {

	private File file;
	private String filename;

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (filename == null) {
			filename = getFilename();
		}
		if ((filename == null) || (filename.length() == 0)) {
			setExisting(false);
		} else {
			setVariants(FileMediaType.guess(filename));
			GregorianCalendar fileLastModification = new GregorianCalendar();
			file = getActivator().getFile(filename, fileLastModification);
			if (file == null) {
				setExisting(false);
			} else if (file.isFile()) {
				setLastModification(fileLastModification.getTime());
				getAllowedMethods().add(Method.GET);
			}
		}
		setName(Messages.AbstractFileResource_Name);
		setDescription(Messages.AbstractFileResource_Description);
		addVariants(MediaType.APPLICATION_OCTET_STREAM);
		addVariants(MediaType.MULTIPART_FORM_DATA);
	}

	/**
	 * @return The associated File.
	 */
	protected File getFile() {
		return file;
	}

	/**
	 * The file name is relative to the Root path within the bundle (default is a folder "files").
	 * 
	 * <p>
	 * You can override this method to force the file name to any String. Default implementation
	 * use the remaining part of the URL to get the file name.
	 * 
	 * @return The file name to download. Null is none.
	 * @see #rootPath()
	 */
	protected String getFilename() {
		String result = getRequest().getResourceRef().getRemainingPart(true,false);
		if ((result != null) && (result.length() > 0) && (result.charAt(0) != '/')) {
			result = '/' + result;
		}
		return result;
	}

	/**
	 * Return the relative bundle Activator class.
	 * 
	 * <p>
	 * Can <b>NOT</b> return a null value. This activator must extends the AbstractFileRepositoryActivator class.
	 * @return
	 */
	protected abstract AbstractFileRepositoryActivator getActivator();
	
	@Override
	public Representation get(Variant variant) throws ResourceException {
		return new FileRepresentation(file, variant.getMediaType());
	}
	
	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		if (filename == null) {
			filename = getFilename();
			if (filename == null) {
				return null;
			}
		}
		file = getActivator().getExternalFile(filename);
		if ((file != null) && file.isFile()) {
			if (!file.delete()) {
				getActivator().info("Unable to delete file: " + file.getAbsolutePath());
				setStatus(Status.SERVER_ERROR_INTERNAL);
				return null;
			}
		}
		setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}

	@Override
	protected Representation post(Representation entity, Variant variant)
			throws ResourceException {
		saveFile(entity, variant);
		return null;
	}

	@Override
	protected Representation put(Representation representation, Variant variant)
			throws ResourceException {
		saveFile(representation, variant);
		return null;
	}

	private void saveFile(Representation entity, Variant variant) {
		if (filename == null) {
			filename = getFilename();
			if (filename == null) {
				return;
			}
		}
		file = getActivator().getExternalFile(filename);
		if (MediaType.APPLICATION_OCTET_STREAM.equals(variant.getMediaType(), true) ||
			MediaType.APPLICATION_XML.equals(variant.getMediaType(), true)) {
			if (file.exists()) {
				if (!file.delete()) {
					getActivator().info("Unable to delete file: " + file.getAbsolutePath());
				}
			} else {
				file.getParentFile().mkdirs();
			}
			try {
				if (!file.createNewFile()) {
					getActivator().debug("File already exists: " + file.getAbsolutePath());
				}
			} catch (IOException e) {
				getActivator().error(e.getLocalizedMessage(), e);
			}
			try {
				long size = entity.getSize();
				final ReadableByteChannel in = entity.getChannel();
				try {
					final FileOutputStream fout = new FileOutputStream(file);
					try {
						final FileChannel out = fout.getChannel();
						try {
							if (size == -1) {
								final ByteBuffer buffer = ByteBuffer.allocateDirect(16 * 1024);
								while (in.read(buffer) != -1) {
									// prepare the buffer to be drained
									buffer.flip();
									// write to the channel, may block
									out.write(buffer);
									// If partial transfer, shift remainder down
									// If buffer is empty, same as doing clear()
									buffer.compact();
								}
								// EOF will leave buffer in fill state
								buffer.flip();
								// make sure the buffer is fully drained.
								while (buffer.hasRemaining()) {
									out.write(buffer);
								}
							} else {
								out.transferFrom(in, 0, size);
							}
						} finally {
							out.close();
						}
					} finally {
						fout.close();
					}
				} finally {
					in.close();
				}
				setStatus(Status.SUCCESS_CREATED);
			} catch (IOException e) {
				getActivator().error(e.getLocalizedMessage(), e);
				setStatus(Status.SERVER_ERROR_INTERNAL);
			}
		} else if (MediaType.MULTIPART_FORM_DATA.equals(variant.getMediaType(), true)) {

			// The Apache FileUpload project parses HTTP requests which
			// conform to RFC 1867, "Form-based File Upload in HTML". That
			// is, if an HTTP request is submitted using the POST method,
			// and with a content type of "multipart/form-data", then
			// FileUpload can parse that request, and get all uploaded files
			// as FileItem.

			// 1/ Create a factory for disk-based file items
			DiskFileItemFactory factory = new DiskFileItemFactory();
			factory.setSizeThreshold(1000240);
			factory.setRepository(new File("_tempdir")); //$NON-NLS-1$

			// 2/ Create a new file upload handler based on the Restlet
			// FileUpload extension that will parse Restlet requests and
			// generates FileItems.
			RestletFileUpload upload = new RestletFileUpload(factory);
			try {
				// 3/ Request is parsed by the handler which generates a
				// list of FileItems
				FileItem fileItem = null;
				for (FileItem fitem : upload.parseRequest(getRequest())) {
					if (fitem.getFieldName().equals("file")) { //$NON-NLS-1$
						fileItem = fitem;
					}
				}
				// Set the status of the response.
				if (fileItem != null) {
					// Delete any existing file.
					if (file.exists()) {
						if (!file.delete()) {
							getActivator().info("Unable to delete file: " + file.getAbsolutePath());
						}
					} else {
						file.getParentFile().mkdirs();
					}
					fileItem.write(file);
					setStatus(Status.SUCCESS_CREATED);
				} else {
					setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				}
			} catch (Exception e) {
				setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				getActivator().error(e.getLocalizedMessage(), e);
			}
		} else {
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
		}
	}
}
