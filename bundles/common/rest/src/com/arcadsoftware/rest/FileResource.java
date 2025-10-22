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
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.Date;

import org.eclipse.jetty.http.MultiPartConfig;
import org.eclipse.jetty.http.MultiPart.Part;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.ext.jetty.MultiPartRepresentation;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.internal.Messages;

/**
 * This resource allow to upload and download files from the server by passing the file path.
 * 
 * <p>
 * Basically this resource use a request parameter <b>path</b> to matched to the file path, or as a default, 
 * use the remaining part of the URL. 
 * 
 * <p>
 * According to the file name extension the resource support the corresponding MediaType. By debault the
 * "application/octet-stream" MadiaType is supported.
 * 
 * <p>
 * If the file exists onto the File system the the GET method is allowed and the "last modification date" of this
 * resource match the system last modification date of the file.
 * 
 * <p>
 * You can extends this behavior in different ways :
 * 
 * <ul>
 * <li>override <code>getPath()</code> to return any (hard coded or any) local file path. 
 * <li>override <code>getFile(String)</code> if the Path is a relative one or if the file must be 
 * converted before to be accessible by the Resource.
 * <li>override <code>doInit()</code> to allow the POST, PUT and DELETE methods, theses methods interact
 * with the File.
 * </ul>
 * 
 * Instead of override <code>getPath()</code> to set a fixed file path name, you can override <code>doInit()</code> 
 * and call <code>setPath(String)</code> before to call the <code>super.doInit()</code>, and after this call
 * can set the allowed methods. Note that in that case de GET method is allowed if the File exists. If you
 * want to forbid the read access to this file you will have to manually remove the allowed method.   
 */
public class FileResource extends UserLinkedResource {

	private File file;
	private String path;
	private MediaType gmt;

	/**
	 * Override this method to specify the access type to this resource.
	 * 
	 * <p>
	 * By default GET access is allowed only if the file exist onto the file system.
	 * Other supporter access type may be:
	 * 
	 * <pre>
	 * getAllowedMethods().add(Method.POST);
	 * getAllowedMethods().add(Method.PUT);
	 * getAllowedMethods().add(Method.DELETE);
	 * </pre>
	 */
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (path == null) {
			path = getPath();
		}
		if ((path == null) || (path.length() == 0)) {
			setExisting(false);
		} else {
			gmt = FileMediaType.guess(path);
			if (gmt != null) {
				setVariants(gmt);
				if (!MediaType.APPLICATION_OCTET_STREAM.equals(gmt)) {
					addVariants(MediaType.APPLICATION_OCTET_STREAM);
				}
			}
			// Ici on initialise l'état de la resource en fonction de 
			// l'état du fichier associé.
			file = getFile(path);
			if (file == null) {
				setExisting(false);
			} else if (file.isFile()) {
				setLastModification(new Date(file.lastModified()));
				getAllowedMethods().add(Method.GET);
			}
		}
		setName(Messages.AbstractFileResource_Name);
		setDescription(Messages.AbstractFileResource_Description);
		addVariants(MediaType.MULTIPART_FORM_DATA);
	}

	/**
	 * Override this method to log the errors...
	 * 
	 * @param e an error.
	 */
	protected void logError(Throwable e) {
		if (getLoggedPlugin() != null) {
			getLoggedPlugin().error(e.getLocalizedMessage(), e);
		}
	}
	
	/**
	 * Set the path value (used to define a fixed file path).
	 * 
	 * <p>
	 * If used before to call the <code>super.doInit()</code> method this setter disable the call to <code>getPath()</code>. 
	 * 
	 * @param path as valid file system path.
	 */
	protected void setPath(String path) {
		this.path = path;
	}
	
	/**
	 * This method is called during the initialization of the resource to get an handle to the destination
	 * file associated to this Resource.
	 * 
	 * <p>
	 * The returned file may not currently exist onto the file system.
	 * 
	 * <p>
	 * Sub-class may override this method if the path is not a real File System path, or to make any accessibility 
	 * test onto the returned File.
	 * 
	 * @param path A path, may be a invalid file path, or a pseudo file path.
	 * @return null if the access to the target file is forbidden.
	 */
	protected File getFile(String path) throws ResourceException {
		try {
			return new File(path);
		} catch (Exception e) {
			logError(e);
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST,e);
		}
	}
	
	/**
	 * Get the file targeted by this resource.
	 * 
	 * <p>
	 * This method is not intended to be override.
	 * 
	 * @return null if this call has been cancelled.
	 */
	protected final File getFile() {
		return file;
	}

	/**
	 * The path name a fully qualified path targeting a file. This file may not exist on the file system,
	 * especially with POST request. 
	 * <p>
	 * This method just have to return a valid file system path from any request element,
	 * any fixed value or whatever. It can be used to apply a pattern to the path, but do not test the validity
	 * of the file reference, this will be done into the <code>doInit()</code> method.
	 * 
	 * <p>
	 * You can override this method to force the File path name to any String. Default implementation
	 * use a request parameter "path" value, or if none, return the remaining part of the resource URL.
	 * 
	 * @return The file name to download. Null if none, the call will be cancelled.
	 */
	protected String getPath() {
		String p = getRequest().getResourceRef().getQueryAsForm().getFirstValue("path"); //$NON-NLS-1$
		if (p == null) {
			p = getRequest().getResourceRef().getRemainingPart();
		}
		if ((p == null) || (p.length() == 0) || ("/".equals(p))) { //$NON-NLS-1$
			return null;
		}
		return p;
	}

	/**
	 * Override this method to set that the files sent to the client (through GET HTTP method should be deleted after communication.
	 *  
	 * @return Default implementation return false.
	 */
	public boolean isAutoDeleting() {
		return false;
	}
	
	@Override
	public Representation get(Variant variant) throws ResourceException {
		if ((file != null) && file.isFile()) {
			FileRepresentation frep = new FileRepresentation(file, variant.getMediaType());
			if (isAutoDeleting()) {
				frep.setAutoDeleting(true);
				frep.setExpirationDate(new Date(System.currentTimeMillis() + 3600000)); // one hour !
			}
			return frep;
		}
		return null;
	}
	
	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		if (file == null) {
			return null;
		}
		// On interdit ici la supperssion de dossiers...
		if (file.isFile()) {
			if (!file.delete()) {
				logError(new IOException("Unable to delete file: " + file.getAbsolutePath()));
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

	/**
	 * This method record the file onto the file system.
	 * 
	 * <p>
	 * Default implementation support a direct Octet stream, XML and JSON format, along with Multipart Form data.
	 * 
	 * <p>
	 * If an error occurs the response status is set to a value diferent from 201 (Status.SUCCESS_CREATED), if this
	 * method is overrided this is the responsability of the overrider to check this status before to process
	 * to post-operation.
	 * 
	 * @param entity
	 * @param variant
	 */
	protected void saveFile(Representation entity, Variant variant) {
		if (file == null) {
			return;
		}
		if (entity == null) {
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
			return;
		}
		if (MediaType.APPLICATION_OCTET_STREAM.equals(variant.getMediaType(), true) ||
				MediaType.APPLICATION_XML.equals(variant.getMediaType(), true) ||
				MediaType.APPLICATION_JSON.equals(variant.getMediaType(), true) ||
			((gmt != null) && gmt.equals(variant.getMediaType(), true))) {
			try {
				// Delete any existing file.
				if (file.exists()) {
					if (!file.delete()) {
						logError(new IOException("Unable to delete file: " + file.getAbsolutePath()));
					}
				} else {
					file.getParentFile().mkdirs();
				}
				// Create an empty new file.
				file.createNewFile();
			} catch (Throwable e) {
				logError(e);
				return;
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
			} catch (Throwable e) {
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
			} catch (IOException e) {
				setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
				getOSGiApplication().getActivator().error(e.getLocalizedMessage(), e);
			}
		} else {
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
		}
	}
	
	/**
	 * <b>WARNING</b> call this method if and only if <code>getPath()</code> return <b>null</b>.
	 * Otherwise you should override the method <code>getFile(String)</code>. 
	 * If you call <code>setFile(String)</code> you will have to set the allowed method, 
	 * any specific MediaType and set the last modification date.
	 * 
	 * <p>
	 * Overriding <code>getPath()</code> and <code>getFile(String)</code> allow to the FileResource
	 * to initialize its state according to the File state (Read access, type and modification date). 
	 * 
	 * <p>
	 * Use this method carefully, if <code>getPath()</code> and <code>getFile(String)</code> return 
	 * a viable file (as does the default implementation and that <code>setPath(String)</code> is not 
	 * called into the <code>doInit()</code> method, then the Resource initialization may be a mess.
	 * 
	 * @param file the new file to point to.
	 * @see #getFile(String)
	 * @see #getPath()
	 */
	protected final void setFile(File file) {
		this.file = file;
	}
}
