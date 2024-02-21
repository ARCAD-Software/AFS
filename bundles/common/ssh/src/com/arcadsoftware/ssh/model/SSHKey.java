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
package com.arcadsoftware.ssh.model;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;

public class SSHKey implements IIdentifiedBean {

	public static final String ENTITY = "sshKey"; //$NON-NLS-1$
	public static final String NAME = "name"; //$NON-NLS-1$
	public static final String FINGERPRINT = "fingerprint"; //$NON-NLS-1$
	public static final String TYPE = "keytype"; //$NON-NLS-1$
	public static final String LENGTH = "keylength"; //$NON-NLS-1$
	public static final String COMMENT = "keycomment"; //$NON-NLS-1$
	public static final String PASSPHRASE = "passphrase"; //$NON-NLS-1$

	private final BeanMap beanmap;

	public SSHKey() {
		this(new BeanMap(ENTITY));
	}

	public SSHKey(String type) {
		this(new BeanMap(type));
	}

	public SSHKey(final BeanMap beanmap) {
		this.beanmap = beanmap;
	}

	public BeanMap getBeanMap() {
		return beanmap;
	}

	public String getComment() {
		return beanmap.getString(COMMENT);
	}

	public String getFingerprint() {
		return beanmap.getString(FINGERPRINT);
	}

	@Override
	public int getId() {
		return beanmap.getId();
	}

	public int getLength() {
		return beanmap.getInt(LENGTH);
	}

	public String getName() {
		return beanmap.getString(NAME);
	}

	public String getPassphrase() {
		return beanmap.getString(PASSPHRASE);
	}

	public String getAlgorithm() {
		return beanmap.getString(TYPE);
	}

	public SSHKeyType getType() {
		try {
			return SSHKeyType.fromAlgorithm(getAlgorithm());
		} catch (final IllegalArgumentException e) {
			return SSHKeyType.UNKNOWN;
		}
	}

	public boolean isEncrypted() {
		return (getPassphrase() != null) && !getPassphrase().isEmpty();
	}

	public void setComment(final String comment) {
		beanmap.put(COMMENT, comment);
	}

	public void setFingerprint(final String fingerprint) {
		beanmap.put(FINGERPRINT, fingerprint);
	}

	public void setLength(final int length) {
		beanmap.put(LENGTH, length);
	}

	public void setName(final String name) {
		beanmap.put(NAME, name);
	}

	public void setPassphrase(final String passphrase) {
		beanmap.put(PASSPHRASE, passphrase);
	}

	public void setType(final SSHKeyType type) {
		beanmap.put(TYPE, type.getAlgorithm());
	}
}
