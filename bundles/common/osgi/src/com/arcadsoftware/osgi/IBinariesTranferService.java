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
package com.arcadsoftware.osgi;

import java.io.File;
import java.io.InputStream;
import java.util.List;

/**
 * This is the interface with OSGi bundle for Binary file transfer implementation services.
 * This interface is coupled with a REST interface for client upload and download.
 */
public interface IBinariesTranferService {

	/**
	 * OSGi service name.
	 */
	public static final String clazz = IBinariesTranferService.class.getName(); 

	/**
	 * What the client is going to do with the uri returned by
	 * {@link IBinariesTranferService#generateKey(String, int, BinaryAccess)}.
	 *
	 * <p>A store may need the operation itself and not only the access right: an S3
	 * pre-signed url is bound to exactly one HTTP verb, whereas a local filesystem
	 * key only carries a read-only flag.
	 */
	public enum BinaryAccess {

		/** Read the resource: GET, HEAD. */
		READ(true),
		/** Create or replace the resource: PUT, POST. */
		WRITE(false),
		/** Remove the resource: DELETE. */
		DELETE(false),
		/**
		 * Legacy: a single key usable with any verb.
		 *
		 * <p>This is the meaning of {@code readOnly == false} in
		 * {@link IBinariesTranferService#generateKey(String, int, boolean)}. Only a
		 * local filesystem store can honor it; a pre-signed url store has to fall
		 * back to {@link #WRITE}.
		 */
		READ_WRITE(false);

		private final boolean readOnly;

		private BinaryAccess(boolean readOnly) {
			this.readOnly = readOnly;
		}

		/**
		 * The minimal access right carried by the generated key.
		 *
		 * @return true if the key must not allow any modification.
		 */
		public boolean isReadOnly() {
			return readOnly;
		}
	}

	/**
	 * Generate a temporary access to the given resource.
	 *
	 * @param category
	 * @param id
	 * @param readOnly
	 * @return the http uri usable to access to this resource.
	 * @deprecated use {@link #generateKey(String, int, BinaryAccess)}. A
	 *             {@code readOnly} value of false used to mean "a key valid for any
	 *             verb", which a pre-signed url store can not produce.
	 */
	@Deprecated
	public String generateKey(String category, int id, boolean readOnly);

	/**
	 * Generate a temporary access to the given resource, for one given operation.
	 *
	 * @param category
	 * @param id
	 * @param access the operation the returned uri is intended for.
	 * @return the http uri usable to access to this resource.
	 */
	public default String generateKey(String category, int id, BinaryAccess access) {
		// A filesystem store only needs the access right carried by the key.
		return generateKey(category, id, access.isReadOnly());
	}

	/**
	 * Generate a temporary File representation of the given resource.
	 * 
	 * <p>This file can be acceded in read-only mode. 
	 * 
	 * @param category
	 * @param id
	 * @return the file or null if it does not exists.
	 */
	public File generateTempFile(String category, int id);
	
	/**
	 * Create a new file entry into the file repository.
	 * 
	 * <p>
	 * This method may return a negative number if the BinariesTranfertService is not correctly configured.
	 * 
	 * @param category
	 * @return the new created ID.
	 */
	public int newFileId(String category);

	/**
	 * Copy a file from a category to another.
	 * 
	 * @param newCategory
	 * @param oldCategory
	 * @param id
	 * @return
	 */
	public int newFileId(String newCategory, String oldCategory, int id);
	
	/**
	 * Put the given file into the repository.
	 * 
	 * @param category
	 * @param id
	 * @param file
	 * @return true if the file is successfully copied into the repository.
	 */
	public boolean newFile(String category, int id, File file);

	/**
	 * Put the given Stream Content into the Repository.
	 * 
	 * @param category
	 * @param id
	 * @param stream
	 * @param filename
	 * @return
	 */
	public boolean newFile(String category, int id, InputStream stream, String filename);
	
	/**
	 * Move a file id from a category to another.
	 * 
	 * @param newCategory
	 * @param oldCategory
	 * @param id
	 * @return zero if the operation can not be completed.
	 */
	public int moveFileId(String newCategory, String oldCategory, int id);
	
	/**
	 * Definitively remove a file from the repository.
	 *  
	 * @param category
	 * @param id
	 * @return true if the operation is successful.
	 */
	public boolean removeFile(String category, int id);
	
	/**
	 * Get all the id of files accessible into a category.
	 * 
	 * @param category
	 * @return can return an empty list, never return null.
	 */
	public List<Integer> listCategory(String category);
	
	
	/**
	 * Get the root path of the repository.
	 * @return the root path of the repository
	 */
	public String getRootPath();
	
	
}
