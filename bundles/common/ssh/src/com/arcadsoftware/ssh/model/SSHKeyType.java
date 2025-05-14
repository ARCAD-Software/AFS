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
	
	UNKNOWN("", "", 0, "", "", ""),
	RSA("RSA", "RSA", 4096, "id_rsa", "id_rsa.pub", "BC"),
	EDDSA("EdDSA", "Ed25519", 255, "id_ed25519", "id_ed25519.pub", "BC");

	public static SSHKeyType fromAlgorithm(final String algo) {
		if (algo != null && ! algo.isEmpty()) {
			for (SSHKeyType skt : SSHKeyType.values()) {
				if (skt.getAlgorithm().equalsIgnoreCase(algo)) {
					return skt;
				}
			}
		}
		return UNKNOWN;
	}
	
	public static SSHKeyType fromName(final String name) {
		if (name != null && !name.isEmpty()) {
			for (SSHKeyType skt : SSHKeyType.values()) {
				if (skt.getName().equalsIgnoreCase(name)) {
					return skt;
				}
			}
		}
		return UNKNOWN;
	}
	
	private final String name;
	private final String algorithm;
	private final int length;
	private final String privateKeyName;
	private final String publicKeyName;
	private final String provider;

	SSHKeyType(final String name, final String algorithm, final int length, final String privateKeyName, final String publicKeyName, final String provider) {
		this.name = name;
		this.algorithm = algorithm;
		this.length = length;
		this.privateKeyName = privateKeyName;
		this.publicKeyName = publicKeyName;
		this.provider = provider;
	}

	public String getName() {
		return name;
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
	
	public String getProvider() {
		return provider;
	}
}
