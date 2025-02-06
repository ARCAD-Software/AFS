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
package com.arcadsoftware.server.properties.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.ext.fileupload.RestletFileUpload;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.resource.ResourceException;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;

import com.arcadsoftware.rest.UserLinkedResource;

public class AdminPropertiesResource extends UserLinkedResource {

	private File file;
	private String filename;

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (hasRight(251)) {
			getAllowedMethods().add(Method.GET);
			getAllowedMethods().add(Method.PUT);
			getAllowedMethods().add(Method.POST);
			getAllowedMethods().add(Method.DELETE);
			addVariants(MediaType.TEXT_PLAIN);
			addVariants(MediaType.MULTIPART_FORM_DATA);
			Object o = getRequest().getAttributes().get("filename"); //$NON-NLS-1$
			if (o == null) {
				setExisting(false);
			} else {
				Calendar lm = new GregorianCalendar();
				lm.setTime(new Date(0));
				filename = '/' + o.toString();
				file = Activator.getInstance().getFile(filename, lm); //$NON-NLS-1$
				setLastModification(lm.getTime());
			}
		} else {
			getAllowedMethods().clear();
		}
	}

	@Override
	public Representation post(Representation entity, Variant variant) throws ResourceException {
		file = Activator.getInstance().getExternalFile(filename);
		file.getParentFile().mkdirs();
		if (MediaType.MULTIPART_FORM_DATA.equals(entity.getMediaType(), true)) {
			// The Apache FileUpload project parses HTTP requests which
			// conform to RFC 1867, "Form-based File Upload in HTML". That
			// is, if an HTTP request is submitted using the POST method,
			// and with a content type of "multipart/form-data", then
			// FileUpload can parse that request, and get all uploaded files
			// as FileItem.

			// 1/ Create a factory for disk-based file items
			DiskFileItemFactory factory = new DiskFileItemFactory();
			factory.setSizeThreshold(1000240);
			factory.setRepository(Activator.getInstance().getExternalFile("_tempdir")); //$NON-NLS-1$

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
					if ((file != null) && (file.exists())) {
						if (!file.delete()) {
							Activator.getInstance().info("Unable to remove old file: " + file.getAbsolutePath());
						}
					}
					// Create new one.
					fileItem.write(file);
					setStatus(Status.SUCCESS_CREATED);
				} else {
					setStatus(Status.CLIENT_ERROR_BAD_REQUEST,
							Messages.AdminPropertiesResource_EmptyFileError);
				}
			} catch (Exception e) {
				setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Messages.AdminPropertiesResource_UploadError);
				Activator.getInstance().error(Messages.AdminPropertiesResource_UploadError, e);
			}
		} else {
			// on accepte n'importe quoi...
			if (file.exists()) {
				if (!file.delete()) {
					Activator.getInstance().info("Unable to remove old file: " + file.getAbsolutePath());
				}
			}
			try {
				if (!file.createNewFile()) {
					Activator.getInstance().debug("File already exists: " + file.getAbsolutePath());
				}
			} catch (IOException e) {
				Activator.getInstance().error(Messages.AdminPropertiesResource_UploadError, e);
			}
			try {
				long size = entity.getSize();
				ReadableByteChannel in = entity.getChannel();
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
				purgeCache();
				setStatus(Status.SUCCESS_CREATED);
			} catch (IOException e) {
				Activator.getInstance().error(Messages.AdminPropertiesResource_UploadError, e);
				setStatus(Status.SERVER_ERROR_INTERNAL, Messages.AdminPropertiesResource_UploadError);
			}
		}
		return null;
	}

	@Override
	public Representation delete(Variant variant) throws ResourceException {
		file = Activator.getInstance().getExternalFile(filename);
		if ((file != null) && file.exists()) {
			if (file.delete()) {
				purgeCache();
				setStatus(Status.SUCCESS_NO_CONTENT);
			}
			throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED,
					Messages.AdminPropertiesResource_NoDeletionError);
		}
		return null;
	}

	private void purgeCache() {
		Activator.getInstance().fileChanged(filename);
	}

	@Override
	public Representation get(Variant variant) throws ResourceException {
		if ((file != null) && file.exists()) {
			return new FileRepresentation(file, MediaType.TEXT_PLAIN);
		}
		StringRepresentation sr = new StringRepresentation("\n", MediaType.TEXT_PLAIN); //$NON-NLS-1$
		sr.setModificationDate(getLastModification());
		return sr;
	}

	@Override
	public Representation put(Representation entity, Variant variant) throws ResourceException {
		return post(entity,variant);
	}
}
