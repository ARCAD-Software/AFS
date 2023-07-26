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
package com.arcadsoftware.server.binaries.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.Date;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.ext.fileupload.RestletFileUpload;
import org.restlet.representation.EmptyRepresentation;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.resource.ResourceException;
import org.restlet.representation.Variant;

import com.arcadsoftware.rest.FileMediaType;
import com.arcadsoftware.rest.OSGiResource;

public class BinResource extends OSGiResource {

	private int id;
	private String category;
	private File file;

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		setExisting(false);
		if (Activator.getInstance() != null) {
			Object o = getRequest().getAttributes().get("key"); //$NON-NLS-1$
			if (o instanceof String) {
				BinariesKey key = Activator.getInstance().find((String) o);
				if (key != null) {
					id = key.getId();
					category = key.getCategory();
					setExisting(true);
					getAllowedMethods().clear();
					getAllowedMethods().add(Method.OPTIONS);
					getAllowedMethods().add(Method.HEAD);
					getAllowedMethods().add(Method.GET);
					setVariants(MediaType.APPLICATION_OCTET_STREAM, MediaType.MULTIPART_FORM_DATA, MediaType.ALL);
					if (!key.isReadOnly()) {
						getAllowedMethods().add(Method.PUT);
						getAllowedMethods().add(Method.POST);
						getAllowedMethods().add(Method.DELETE);
					}
					file = Activator.getInstance().getFile(category, id);
					if ((file != null) && file.isFile()) {
						setLastModification(new Date(file.lastModified()));
					}
				}
			}
		}
	}

	/**
	 * Accepts and processes a representation posted to the resource.
	 */
	@Override
	public Representation post(Representation entity, Variant variant) {
		if (!getAllowedMethods().contains(Method.POST)) {
			setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
			return null;
		}
		Activator.getInstance().test(category, id);
		if ((entity == null) || (entity instanceof EmptyRepresentation)) {
			// POST request with no entity.
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST, "The server has retreived an empty body. Creating null binaries files is not allowed.");
			return null;
		}
		if (MediaType.APPLICATION_OCTET_STREAM.equals(entity.getMediaType(), true)) {
			// Delete any existing file.
			Activator.getInstance().removeFiles(category, id);
			String name = null;
			if (entity.getDisposition() != null) {
				name = entity.getDisposition().getFilename();
			}
			if (name == null) {
				name = getRequest().getResourceRef().getRemainingPart(true);
				if (name == null) {
					name = ""; //$NON-NLS-1$
				}
			}
			if ((name.indexOf('/') > -1) || (name.indexOf('\\') > -1)) {
				name = new File(name).getName();
			}
			file = new File(Activator.getInstance().getFileNamePrefix(category, id) + name);
			try {
				file.getParentFile().mkdirs();
				if (!file.createNewFile()) {
					Activator.getInstance().debug("File already exists: " + file.getName());
				}
			} catch (Exception e) {
				Activator.getInstance().error(Messages.BinResource_Error_file_creation, e);
				setStatus(Status.SERVER_ERROR_INTERNAL);
				return null;
			}
			try {
				long size = entity.getSize();
				ReadableByteChannel in = entity.getChannel();
				if (in == null) {
					setStatus(Status.CLIENT_ERROR_UNPROCESSABLE_ENTITY, "The Request body is empty or can not reach the /bin resource.");
				} else {
					try {
						FileOutputStream fos = new FileOutputStream(file);
						try {
							FileChannel out = fos.getChannel();
							try {
								if (size == -1) {
									ByteBuffer buffer = ByteBuffer.allocateDirect(16 * 1024);
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
							fos.close();
						}
					} finally {
						in.close();
					}
					setStatus(Status.SUCCESS_CREATED);
				}
			} catch (IOException e) {
				Activator.getInstance().error(Messages.BinResource_Error_file_upload, e);
				setStatus(Status.SERVER_ERROR_INTERNAL);
			}
		} else if (MediaType.MULTIPART_FORM_DATA.equals(entity.getMediaType(), true)) {
			// The Apache FileUpload project parses HTTP requests which
			// conform to RFC 1867, "Form-based File Upload in HTML". That
			// is, if an HTTP request is submitted using the POST method,
			// and with a content type of "multipart/form-data", then
			// FileUpload can parse that request, and get all uploaded files
			// as FileItem.
			// 1/ Create a factory for disk-based file items
			DiskFileItemFactory factory = new DiskFileItemFactory();
			//factory.setSizeThreshold(1000240);
			int maxFileSize = Activator.getInstance().getMaxFileSize();
			factory.setSizeThreshold(maxFileSize);
			factory.setRepository(new File(Activator.getInstance().getDirName("_tempdir"))); //$NON-NLS-1$
			// 2/ Create a new file upload handler based on the Restlet
			// FileUpload extension that will parse Restlet requests and
			// generates FileItems.
			RestletFileUpload upload = new RestletFileUpload(factory);
			try {
				// 3/ Request is parsed by the handler which generates a
				// list of FileItems
				FileItem fileItem = null;
				for (FileItem fitem : upload.parseRequest(getRequest())) {
					if ("file".equals(fitem.getFieldName())) { //$NON-NLS-1$
						fileItem = fitem;
						break;
					}
					if (fileItem == null) {
						fileItem = fitem;
					}
				}
				// Set the status of the response.
				if (fileItem != null) {
					// Delete any existing file.
					Activator.getInstance().removeFiles(category, id);
					// Create new one.
					String name = fileItem.getName();
					if (name == null) {
						name = ""; //$NON-NLS-1$
					} else if ((name.indexOf('/') > -1) || (name.indexOf('\\') > -1)) {
						name = new File(name).getName();
					}
					file = new File(Activator.getInstance().getFileNamePrefix(category, id) + name);
					try {
						file.getParentFile().mkdirs();
						if (!file.createNewFile()) {
							Activator.getInstance().debug("File already exists: " + file.getName());
						}
					} catch (Throwable e) {
						Activator.getInstance().error(Messages.BinResource_Error_file_creation, e);
						setStatus(Status.SERVER_ERROR_INTERNAL);
						return null;
					}
					fileItem.write(file);
					setStatus(Status.SUCCESS_CREATED);
				} else {
					setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				}
			} catch (Exception e) {
				setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				Activator.getInstance().error(Messages.BinResource_Error, e);
			}
		}
		return null;
	}

	@Override
	public Representation delete(Variant variant) throws ResourceException {
		if (!getAllowedMethods().contains(Method.DELETE)) {
			setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
			return null;
		}
		Activator.getInstance().removeFiles(category, id);
		setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}
	
	@Override
	public Representation get(Variant variant) throws ResourceException {
		if ((file == null) || !file.exists()) {
			throw new ResourceException(Status.CLIENT_ERROR_NOT_FOUND);
		}
		String name = file.getName();
		int i = name.indexOf('_');
		if ((i > 0) && (i < (name.length() - 2))) {
			name = name.substring(i + 1);
		}
		FileRepresentation fileRep = new FileRepresentation(file, FileMediaType.guess(name));
		// Replace the real file name with the non indexed file name, if any. 
		fileRep.getDisposition().setFilename(name);
		fileRep.getDisposition().setSize(file.length());
		fileRep.getDisposition().setModificationDate(new Date(file.lastModified()));
		return fileRep;
	}

	@Override
	public Representation put(Representation entity, Variant variant) throws ResourceException {
		return post(entity, variant);
	}
}
