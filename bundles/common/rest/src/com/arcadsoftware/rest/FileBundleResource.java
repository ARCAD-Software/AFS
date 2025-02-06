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
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Date;

import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.restlet.data.MediaType;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.resource.ResourceException;
import org.restlet.representation.Variant;

import com.arcadsoftware.rest.internal.Messages;

/**
 * This resource can be associate to a directory or a file stored into an OSgi Bundle.
 * 
 * <p>
 * It give read access to these files. The default implementation of this
 * resource use the bundle folder <b>/file</b> and the remaining part of
 * the resource URL to look for files to serve. You have to implement
 * the abstract {@link #getBundleContext()} method to return the bundle Context
 * of your Activator.
 * 
 * <p>
 * Then you have to override {@link #doInit()} to allow (or not) the read access:
 * 
 * <pre>
 * protected void doInit() throws ResourceException {
 *   super.doInit();
 *   getAllowedMethods().add(Method.GET);
 * }
 * </pre>
 * 
 * <p>
 * You can override {@link #rootPath()} and/or {@link #getFilename()} to change
 * the default implementation.
 * 
 * @see AbstractFileRepositoryActivator
 * @see XmlFileResource
 */
public abstract class FileBundleResource extends UserLinkedResource {

	private File file;
	private String filename;

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		filename = getFilename();
		if ((filename == null) || (filename.length() == 0)) {
			setExisting(false);
		} else {
			if (filename.charAt(0) != '/') {
				filename = '/' + filename; 
			}
			file = loadFile(filename);
			if (file == null) {
				setExisting(false);
			} else {
				getVariants().add(new Variant(MediaType.ALL));
			}
		}
		setName(Messages.AbstractFileResource_Name);
		setDescription(Messages.AbstractFileResource_Description);
	}

	/**
	 * This method must load the file from the file system.
	 * 
	 * <p>
	 * This method must set the "lastModification" property too.
	 * @param filename
	 * @return
	 */
	protected final File loadFile(String filename) {
		try {
			// Extract the resource from the bundle jar:
			Bundle bundle = getFileBundleContext().getBundle();
			URL url = bundle.getEntry(rootPath() + filename);
			if (url != null) {
				url = FileLocator.toFileURL(url); //$NON-NLS-1$
				File file;
				try {
					// [ML] FileLocator ne fourni pas un URL standart (les espaces ne sont pas remplacé par %20).
					file = new File(new URI(url.getProtocol(), url.getPath(), null));
				} catch (URISyntaxException e) {
					file = new File(url.getPath());
				}
				if ((file != null) && file.exists() && file.isFile()) {
					// Try to guess that we are in a development environment.
					if (bundle.getEntry("/.classpath") != null) { //$NON-NLS-1$
						setLastModification(new Date(file.lastModified()));
					} else {
						setLastModification(new Date(bundle.getLastModified()));
					}
				} else {
					setExisting(false);
				}
				return file;
			}
		} catch (IOException e) {}
		return null;
	}

	/**
	 * Default Root path is "/files".
	 * 
	 * <p>
	 * You can override this method to give access to another bundle folder.
	 * 
	 * @return Root directory from within the Bundle JAr where to find files.
	 */
	protected String rootPath() {
		return "/files"; //$NON-NLS-1$
	}
	
	/**
	 * @return The Bundle Context from the bundle that actually content the files to download.
	 */
	protected abstract BundleContext getFileBundleContext();

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
	 * Get the file reference computed during initialization.
	 * 
	 * @return A bundle File, may be null.
	 * @see #doInit()
	 */
	protected final File getFile() {
		return file;
	}
	
	@Override
	public Representation get(Variant variant) throws ResourceException {
		return new FileRepresentation(file,FileMediaType.guess(filename));
	}
	
}
