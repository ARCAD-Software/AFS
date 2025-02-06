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

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.restlet.data.Parameter;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionUserRepository;

/**
 * An Restlet Resource that is attached to an "Secured" branch and can obtain a reference to the
 * connected user. This reference is used to test the access rights to the accepted HTTP Methods.
 * 
 * <p>
 * <b>NB</b>: This code is duplicated with the class {@link UserLinkedResource}.
 * 
 * @see IConnectionUserBean
 */
public abstract class UserLinkedIdentifiedResource extends IdentifiedResource {

	/**
	 * The value of this request query parameter indicate that the current connected user try to act as another one.
	 * 
	 * <p>
	 * To do so the connected user must have the Corresponding Access right (#3).
	 * 
	 * <p>
	 * The getUser() method return the substituted user.
	 */
	public static final String QUERY_SUBTITUTETOUSER = UserLinkedResource.QUERY_SUBTITUTETOUSER;

	private IConnectionUserBean user;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		// Get the current user connection information.
		IConnectionUserBean u = (IConnectionUserBean) getRequest().getAttributes().get(IConnectionUserBean.CONNECTED_USER);
		if (u != null) {
			// The user possess the substitude Right.
			if (u.getProfile().hasRight(3)) {
				Parameter uid = getRequest().getResourceRef().getQueryAsForm().getFirst(QUERY_SUBTITUTETOUSER);
				if (uid != null) {
					IConnectionUserBean uu = getConnectionUser(uid.getValue());
					if (uu != null) {
						u = uu;
					}
				}
			}
			setUser(u);
		}
	}

	/**
	 * Load an ICOnnectionUserBean associated to the given reference.
	 * 
	 * @param ref must a valid login or a key code [usertype]/[userid].
	 * @return null if this reference do not correspond to any user.
	 */
	protected IConnectionUserBean getConnectionUser(String ref) {
		if (ref == null) {
			return null;
		}
		String type = null;
		int id = 0;
		int i = ref.indexOf('/');
		if (i > 0) {
			type = ref.substring(0, i);
			try {
				id = Integer.parseInt(ref.substring(i+1));
			} catch (NumberFormatException e) {}
		} else {
			try {
				id = Integer.parseInt(ref);
				// Default "user" type...
				type = "user"; //$NON-NLS-1$
			} catch (NumberFormatException e) {}
		}
		BundleContext context = getBundleContext();
		if (context == null) {
			return null;
		}
		@SuppressWarnings("rawtypes")
		ServiceReference sr = context.getServiceReference(IConnectionUserRepository.clazz);
		if (sr == null) {
			return null;
		}
		@SuppressWarnings("unchecked")
		Object service = context.getService(sr);
		if (service instanceof IConnectionUserRepository) {
			if ((id > 0) && (type != null)) {
				IConnectionUserBean user = ((IConnectionUserRepository) service).getUser(type, id);
				if (user != null) {
					return user;
				}
			}
			return ((IConnectionUserRepository) service).getUser(ref);
		}
		return null;
	}
	
	/**
	 * @param user the user to set
	 */
	private void setUser(IConnectionUserBean user) {
		this.user = user;
	}

	/**
	 * @return the user
	 */
	protected final IConnectionUserBean getUser() {
		return user;
	}

	/**
	 * Test a general right (helper method).
	 * 
	 * @param key
	 * @return
	 */
	protected final boolean hasRight(int key) {
		return (getUser() != null) && getUser().getProfile().hasRight(key);
	}
		
	/**
	 * Test a parameterized right (helper method).
	 * 
	 * @param key
	 * @param param
	 * @return
	 */
	protected final boolean hasRight(int key, int param) {
		return getUser().getProfile().hasRight(key, param);
	}

}
