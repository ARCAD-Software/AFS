/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.rest.connection;

/**
 * This interface define the object that is associated to a Connection. It provide informations
 * and validity test of the connection. 
 * 
 * <p>The "Credential" is created by the IAuthentificationService and is cached by the 
 * Connection manager during a short period of time.
 * 
 * @see IUpdatableCredential
 * @see IPatchUserCredential
 */
public interface IConnectionCredential {

	/**
	 * Used to store the currently used credential.
	 */
	public static final String CONNECTED_CREDENTIAL = "connected.credential"; //$NON-NLS-1$
	
	/**
	 * Return a textual representation of the user identification.
	 * 
	 * @return
	 */
	public String getText();

	/**
	 * @return true if the collected identification information are validated.
	 */
	public boolean authenticate(IConnectionUserBean user, String login, char[] secret);
	
	/**
	 * Return an unique ID identifying this identification. This method
	 * should be perform quickly, it is used each time a request is performed. 
	 * 
	 * <p>Because the connection cache can contain user with identification 
	 * from different sources the unique ID should be prefixed with a source ID.
	 * 
	 * <p>The unique ID can be construct like this : "[SourceID]:[LocalUserID]" 
	 * 
	 * @return
	 */
	public String getUniqueId();

	/**
	 * Determine if the validity of this authorization token has expired.
	 * 
	 * <p>This method will allow a restricted access to the .../currentuser urls.
	 * 
	 * @return True if it is out of date.
	 */
	public boolean isOutOfDate();
	
	/**
	 * <p>This method is invoked if the credential is not in the cache or if 
	 * it is invalidated and need to be reloaded.
	 * 
	 * <p>The returned ID will be equal to the user connection bean that will own this credential.
	 * 
	 * @return the USER (entity) internal Identifier (basically the user ID).
	 */
	public int loadUserId();
	
	/**
	 * <p>This method is invoqued just after the <code>loadUserId</code>. 
	 * 
	 * <p>The given user can be a fresh object or an already used one. It need to be patched with
	 * correct credential information (if applicable).
	 * @param user
	 */
	public void update(IConnectionUserBean user);

	/**
	 * Give the user type associated this this king of connection. This is used to load the user informations, from the {@link IConnectionInfoService}.
	 * 
	 * @return null if default user type is used.
	 * @see IConnectionInfoService
	 */
	public String getUserType();

	/**
	 * Test that the given secret phrase is the same as any previous one.
	 * 
	 * @param secret
	 * @return
	 */
	public boolean checkSecret(char[] secret);
}
