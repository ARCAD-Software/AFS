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

import org.restlet.data.Reference;
import org.restlet.resource.ResourceException;

/**
 * The identified resource is a resource find with this kind of URI :
 * 
 * /resource/{id}
 * 
 * where id is an identification code of this resource. The default implementation
 * assume that the id is a number, but overriding <code>findEntityFromCode</code> subclasses
 * may look for other kind of identifier.
 * 
 * This resource does not test if the entity associated to this identification really exist
 * in the storage base. It simply verify the correct syntax of this identifiation.  
 * 
 * An id equal to zero represent an non identified resource.
 *  
 */
public abstract class IdentifiedResource extends OSGiResource {
	
	/**
	 * Id key parameter in paths.
	 * 
	 * <p>
	 * This id can be a string or an integer. If it is a number then
	 * it will be proceeded and returned by <code>getId()</code> method.  
	 */
	public final static String KEY_ID = "id"; //$NON-NLS-1$

	private int id = 0;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		// Test the id attribute (must be an integer).
		if (id == 0) {
			id = getIntAttribute(KEY_ID);
		}
		if (isExisting()) {
			setExisting(id > 0);
		}
	}

	/**
	 * Facility method that parse an Integer key parameter.
	 *
	 * <p>
	 * If the key is not an integer the the method <code>findEntityFromCode</code>
	 * is called to get a numeric id from a string code.
	 *
	 * @param key the key to parse.
	 * @return zero if the key does not exist or is not an integer.
	 * @see #getAttribute(String, int)
	 */
	private final int getIntAttribute(String key) {
		Object o = getRequest().getAttributes().get(key);
		if (o != null) {
			if (o instanceof Integer) {
				return (Integer)o;
			} else {
				String value = o.toString();
				if (value.length() == 0) {
					return 0;
				}
				try {
					return Integer.parseInt(value);
				} catch (NumberFormatException e) {
					return findEntityFromCode(key,Reference.decode(value));
				}
			}
		}
		return 0;
	}
	
	/**
	 * Try to found the corresponding Entity ID from a String, assuming an Entity Code.
	 * 
	 * <p>
	 * Default implementation return zero. You have to override this method this
	 * correct process that associate a Code to an ID (e.g. with a MetaData Entity).
	 * 
	 * @param key The attribute key.
	 * @param string a String passed to the resource through the URI. 
	 * @return default implementation return 0.
	 */
	protected int findEntityFromCode(String key,String value) {
		return 0;
	}

	/**
	 * @return the internal identification number of this resource.
	 * @see IdentifiedResource#setId(int)
	 */
	public int getId() {
		return id;
	}
	
	/**
	 * Force the ID of this resource.
	 * 
	 * <p>
	 * Into default implementation the DI is deduced from the {id} part of the route to this resource.
	 * 
	 * @param id
	 */
	public final void setId(int id) {
		this.id = id;
	}

}
