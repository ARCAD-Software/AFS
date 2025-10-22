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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.Date;

import org.eclipse.jetty.http.MultiPartConfig;
import org.eclipse.jetty.http.MultiPart.Part;
import org.restlet.data.CharacterSet;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.ext.jetty.MultiPartRepresentation;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

/**
 * This Resource can be used to manage access to an XML File. 
 * 
 * <p>
 * To allow modification of this file (i.e. uploading) you just have to 
 * add the following line to the <code>defineFile</code> method:
 * 
 * <p><code>getAllowedMethods().add(Method.PUT);</code>
 * 
 * <p>
 * You can also allow POST method or DELETE method. In both case
 * the <code>defineFile</code> method must return a File object
 * that point to the modified or new file to upload.
 * 
 * @see FileBundleResource
 */
public abstract class XmlFileResource extends OSGiResource {

	private static final String ENCODING = "encoding=\""; //$NON-NLS-1$

	/**
	 * Retreive the XML File charset from the XML tag declaration.
	 * @param file
	 * @return
	 */
	public static CharacterSet getCharacterSet(File file) {
		// Récupération de l'encodage du fichier xml...
		FileInputStream fip;
		try {
			fip = new FileInputStream(file);
		} catch (FileNotFoundException e) {
			// Default encoding of non existent files
			return CharacterSet.UTF_8;
		}
		try {
			byte[] buffer = new byte[512]; // more than needed
			int x = fip.read(buffer);
			String s = new String(buffer, 0, x);
			x = s.indexOf(ENCODING);
			if (x != -1) {
				x += ENCODING.length();
				s = s.substring(x, s.indexOf('"', x));
				return CharacterSet.valueOf(s);
			}
		} catch (Throwable e) {
		} finally {
			try {
				fip.close();
			} catch (IOException e) {
			}
		}
		// Default encoding of XML files
		return CharacterSet.UTF_8;
	}

	private File file;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		file = defineFile();
		if (file == null) {
			setExisting(false);
		} else if (file.isFile()) {
			getAllowedMethods().add(Method.GET);
			if (getLastModification() == null) {
				setLastModification(new Date(file.lastModified()));
			}
		}
		//getVariants().clear();
		setVariants(MediaType.TEXT_XML,MediaType.APPLICATION_XML,MediaType.APPLICATION_OCTET_STREAM, MediaType.MULTIPART_FORM_DATA);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (file.isFile()) {
			FileRepresentation fileRepresentation = new FileRepresentation(file, variant.getMediaType());
			fileRepresentation.setCharacterSet(getCharacterSet(file));
			fileRepresentation.setModificationDate(getLastModification());
			fileRepresentation.setTransient(true);
			return fileRepresentation;
		}
		return null;
	}

	@Override
	protected Representation put(Representation representation, Variant variant)
			throws ResourceException {
		return post(representation, variant);
	}
	
	@Override
	protected Representation post(Representation entity, Variant variant)
			throws ResourceException {
		if (MediaType.APPLICATION_OCTET_STREAM.equals(variant.getMediaType(), true) ||
			MediaType.APPLICATION_XML.equals(variant.getMediaType(), true)) {
			if (file.exists()) {
				if (!file.delete()) {
					logError("Unable to delete file: " + file.getAbsolutePath());
				}
			} else {
				file.getParentFile().mkdirs();
			}
			try {
				if (!file.createNewFile()) {
					logError("");
				}
			} catch (IOException e) {
				logError(e);
			}
			try {
				long size = entity.getSize();
				final ReadableByteChannel in = Channels.newChannel(entity.getStream());
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
				logError(e);
				setStatus(Status.SERVER_ERROR_INTERNAL);
			}
		} else if (MediaType.MULTIPART_FORM_DATA.equals(variant.getMediaType(), true)) {
			try {
				boolean found = false;
				MultiPartRepresentation mprep = new MultiPartRepresentation(entity, new MultiPartConfig.Builder().build());
				for (Part part : mprep.getParts()) {
					if (part.getName().equals("file")) { //$NON-NLS-1$
						// Delete any existing file.
						if (file.exists()) {
							if (!file.delete()) {
								logError("Unable to delete file: " + file.getAbsolutePath());
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
				logError(e);
			}
		} else {
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
		}
		return null;
	}

	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		if ((file != null) && file.isFile()) {
			if (!file.delete()) {
				logError("Unable to delete file: " + file.getAbsolutePath());
				setStatus(Status.SERVER_ERROR_INTERNAL);
				return null;
			}
		}
		setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}

	/**
	 * Obtain the requested file name from three options :
	 * 
	 * <ul>
	 * <li>An uploaded Octet Stream of XML file.
	 * <li>An "file" field into a multi-part Form.
	 * <li>The remaining part of the URL.  
	 * </ul>
	 * 
	 * @return File name from Request entity.
	 */
	protected String getFileName() {
		String name = null;
		Representation entity = getRequest().getEntity();
		if (MediaType.APPLICATION_OCTET_STREAM.equals(entity.getMediaType(), true) || //
				MediaType.APPLICATION_XML.equals(entity.getMediaType(), true) ||
				MediaType.TEXT_XML.equals(entity.getMediaType(), true)) {
			if (entity.getDisposition() != null) {
				name = entity.getDisposition().getFilename();
			}
		} else if (MediaType.MULTIPART_FORM_DATA.equals(entity.getMediaType(), true)) {
			try {
				for (Part part : new MultiPartRepresentation(entity, new MultiPartConfig.Builder().build()).getParts()) {
					if (part.getName().equals("file")) { //$NON-NLS-1$
						name = part.getFileName();
						break;
					}
				}
			} catch (Exception e) {
				logError(e);
			}
		}
		if ((name == null) || (name.trim().length() == 0)) {
			name = getRequest().getResourceRef().getRemainingPart(true, false);
		}
		return name;
	}
	
	/**
	 * Log error during File Upload.
	 * 
	 * @param e
	 */
	protected void logError(Exception e) {
		if (getLoggedPlugin() != null) {
			getLoggedPlugin().error(e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Log error during File Upload.
	 * 
	 * @param message
	 */
	protected void logError(String message) {
		if (getLoggedPlugin() != null) {
			getLoggedPlugin().warn(message);
		}
	}

	/**
	 * This method determine the resource associated XML File.
	 * 
	 * <p>
	 * When possible the <code>lastModicficationDate</code> should be set into this method.
	 * 
	 * <p>
	 * The file should <b>not</b> be loaded here.
	 * 
	 * @return The associated XML File.
	 */
	protected abstract File defineFile();

	/**
	 * @return The previously associated file.
	 */
	protected File getFile() {
		return file;
	}
	
}
