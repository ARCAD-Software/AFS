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
package com.arcadsoftware.metadata.internal;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.Profile;

/**
 * ConnectionUserBean Fake.
 */
public class FakeConnectionUserBean implements IConnectionUserBean {

	private int id;
	private boolean canChangePWD;
	private boolean locked;
	private boolean changePWD;
	private String userType = "user"; //$NON-NLS-1$
	
	public FakeConnectionUserBean(int id) {
		super();
		this.id = id;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getCredential(java.lang.Class)
	 */
	public List<IConnectionCredential> getCredential(Class<? extends IConnectionCredential> clazz) {
		return new ArrayList<IConnectionCredential>();
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getCredential(java.lang.String)
	 */
	public IConnectionCredential getCredential(String uniqueID) {
		return null;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getCredentials()
	 */
	public List<IConnectionCredential> getCredentials() {
		return new ArrayList<IConnectionCredential>();
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getFullname()
	 */
	public String getFullname() {
		return "Unknown"; //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getId()
	 */
	public int getId() {
		return id;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getPassword()
	 */
	public String getPassword() {
		return ""; //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getPrincipal()
	 */
	public int getPrincipal() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#getProfile()
	 */
	public Profile getProfile() {
		return new Profile();
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#isCanChangePWD()
	 */
	public boolean isCanChangePWD() {
		return canChangePWD;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#isChangePWD()
	 */
	public boolean isChangePWD() {
		return changePWD;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#isLocked()
	 */
	public boolean isLocked() {
		return locked;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#setCanChangePWD(boolean)
	 */
	public void setCanChangePWD(boolean canChangePWD) {
		this.canChangePWD = canChangePWD;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#setChangePWD(boolean)
	 */
	public void setChangePWD(boolean changePWD) {
		this.changePWD = changePWD;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.connection.server.IConnectionUserBean#setLocked(boolean)
	 */
	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.rest.connection.IConnectionUserBean#getLogin()
	 */
	public String getLogin() {
		return ""; //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.rest.connection.IConnectionUserBean#getUserType()
	 */
	public String getUserType() {
		return userType;
	}
	
	public void setUserType(String userType) {
		this.userType = userType;
	}
}
