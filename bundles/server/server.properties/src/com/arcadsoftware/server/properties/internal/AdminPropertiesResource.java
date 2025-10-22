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
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.eclipse.jetty.http.MultiPartConfig;
import org.eclipse.jetty.http.MultiPart.Part;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.ext.jetty.MultiPartRepresentation;
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
			try {
				boolean found = false;
				MultiPartRepresentation mprep = new MultiPartRepresentation(entity, new MultiPartConfig.Builder().build());
				for (Part part : mprep.getParts()) {
					if (part.getName().equals("file")) { //$NON-NLS-1$
						// Delete any existing file.
						if (file.exists()) {
							if (!file.delete()) {
								getOSGiApplication().getActivator().info("Unable to delete file: " + file.getAbsolutePath());
							}
						} else {
							file.getParentFile().mkdirs();
						}
						part.writeTo(file.toPath());
						found = true;
						break;
					}
				}
				// Set the status of the response.
				if (found) {
					setStatus(Status.SUCCESS_CREATED);
				} else {
					setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				}
			} catch (Exception e) {
				setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				getOSGiApplication().getActivator().error(e.getLocalizedMessage(), e);
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
				ReadableByteChannel in = Channels.newChannel(entity.getStream());
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
