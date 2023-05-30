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

import java.util.stream.Stream;

public enum SSHKeyType {
	UNKNOWN("", 0, "", ""),
	RSA("RSA", 4096, "id_rsa", "id_rsa.pub"),
	EDDSA("EdDSA", 0, "id_ed25519", "id_ed25519.pub");

	public static SSHKeyType fromAlgorithm(final String algo) {
		return Stream.of(SSHKeyType.values()).filter(t -> t.getAlgorithm().equalsIgnoreCase(algo)).findFirst().orElse(UNKNOWN);
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
