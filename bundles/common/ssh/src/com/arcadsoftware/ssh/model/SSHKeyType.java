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

public enum SSHKeyType {
	
	UNKNOWN("", 0, "", ""), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	RSA("RSA", 4096, "id_rsa", "id_rsa.pub"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	EDDSA("EdDSA", 0, "id_ed25519", "id_ed25519.pub"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	public static SSHKeyType fromAlgorithm(final String algo) {
		if ((algo != null) && ! algo.isEmpty()) {
			for (SSHKeyType skt : SSHKeyType.values()) {
				if (skt.getAlgorithm().equalsIgnoreCase(algo)) {
					return skt;
				}
			}
		}
		return UNKNOWN;
	}
	
	private final String algorithm;
	private final int length;
	private final String privateKeyName;
	private final String publicKeyName;

	SSHKeyType(final String algorithm, final int length, final String privateKeyName, final String publicKeyName) {
		this.algorithm = algorithm;
		this.length = length;
		this.privateKeyName = privateKeyName;
		this.publicKeyName = publicKeyName;
	}

	public String getAlgorithm() {
		return algorithm;
	}

	public int getLength() {
		return length;
	}

	public String getPrivateKeyName() {
		return privateKeyName;
	}

	public String getPublicKeyName() {
		return publicKeyName;
	}
}
