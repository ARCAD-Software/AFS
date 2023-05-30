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
package com.arcadsoftware.ssh.model;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;

public class SSHKeyUpload implements IIdentifiedBean {

	public static final String ENTITY = "sshUpload";

	public static final String NAME = SSHKey.NAME;
	public static final String PASSPHRASE = SSHKey.PASSPHRASE;	
	public static final String PRIVATE_KEY = "privatekey";
	
	public static final String SUCCESSFUL = "successful";
	public static final String MESSAGE = "message";

	private final BeanMap beanmap;

	public SSHKeyUpload() {
		this(new BeanMap());
	}

	public SSHKeyUpload(final BeanMap beanmap) {
		this.beanmap = beanmap;
	}

	public BeanMap getBeanmap() {
		return beanmap;
	}

	@Override
	public int getId() {
		return beanmap.getId();
	}

	public String getMessage() {
		return beanmap.getString(MESSAGE);
	}

	public boolean isSuccessful() {
		return beanmap.getBoolean(SUCCESSFUL);
	}

	public void setMessage(final String message) {
		beanmap.put(MESSAGE, message);
	}

	public void setSuccessful(final boolean successful) {
		beanmap.put(SUCCESSFUL, successful);
	}
	
	public String getName() {
		return beanmap.getString(NAME);
	}
	
	public void setName(final String name) {
		beanmap.put(NAME, name);
	}
	
	public String getPassphrase() {
		return beanmap.getString(PASSPHRASE);
	}
	
	public void setPassphrase(final String passphrase) {
		beanmap.put(PASSPHRASE, passphrase);
	}
	
	public String getPrivateKey() {
		return beanmap.getString(PRIVATE_KEY);
	}
	
	public void setPrivateKey(final String passphrase) {
		beanmap.put(PRIVATE_KEY, passphrase);
	}
}
