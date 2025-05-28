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
package com.arcadsoftware.ssh.model;

/**
 * Enumeration representing supported SSH key types.
 * <p>
 * Each enum constant defines the properties of an SSH key type,
 * including its name, algorithm, key length, key file names, and cryptographic provider.
 * </p>
 * 
 * @author ARCAD Software
 */
public enum SSHKeyType {

    /**
     * Unknown or unsupported SSH key type.
     */
    UNKNOWN("", "", 0, "", "", ""),

    /**
     * RSA SSH key type.
     */
    RSA("RSA", "RSA", 4096, "id_rsa", "id_rsa.pub", "BC"),

    /**
     * EdDSA SSH key type (Ed25519 variant).
     */
    EDDSA("EdDSA", "Ed25519", 255, "id_ed25519", "id_ed25519.pub", "BC");

    /**
     * Returns the {@link SSHKeyType} corresponding to the given algorithm name.
     * <p>
     * The comparison is case-insensitive.
     * </p>
     *
     * @param algo the algorithm name to look up
     * @return the matching {@code SSHKeyType}, or {@link #UNKNOWN} if no match is found
     */
    public static SSHKeyType fromAlgorithm(final String algo) {
        if ((algo != null) && !algo.isBlank()) {
            for (SSHKeyType skt : SSHKeyType.values()) {
                if (skt.getAlgorithm().equalsIgnoreCase(algo)) {
                    return skt;
                }
            }
        }
        return UNKNOWN;
    }

    /**
     * Returns the {@link SSHKeyType} corresponding to the given type name.
     * <p>
     * The comparison is case-insensitive.
     * </p>
     *
     * @param name the type name to look up
     * @return the matching {@code SSHKeyType}, or {@link #UNKNOWN} if no match is found
     */
    public static SSHKeyType fromName(final String name) {
        if ((name != null) && !name.isBlank()) {
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

    /**
     * Constructs an SSHKeyType enum instance.
     *
     * @param name the display name of the key type
     * @param algorithm the algorithm name
     * @param length the default key length (in bits)
     * @param privateKeyName the default private key file name
     * @param publicKeyName the default public key file name
     * @param provider the cryptographic provider name
     */
    SSHKeyType(final String name, final String algorithm, final int length, final String privateKeyName, final String publicKeyName, final String provider) {
        this.name = name;
        this.algorithm = algorithm;
        this.length = length;
        this.privateKeyName = privateKeyName;
        this.publicKeyName = publicKeyName;
        this.provider = provider;
    }

    /**
     * Returns the display name of this SSH key type.
     *
     * @return the key type name
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the algorithm name associated with this SSH key type.
     *
     * @return the algorithm name
     */
    public String getAlgorithm() {
        return algorithm;
    }

    /**
     * Returns the default key length (in bits) for this SSH key type.
     *
     * @return the key length
     */
    public int getLength() {
        return length;
    }

    /**
     * Returns the default private key file name for this SSH key type.
     *
     * @return the private key file name
     */
    public String getPrivateKeyName() {
        return privateKeyName;
    }

    /**
     * Returns the default public key file name for this SSH key type.
     *
     * @return the public key file name
     */
    public String getPublicKeyName() {
        return publicKeyName;
    }

    /**
     * Returns the cryptographic provider name associated with this SSH key type.
     *
     * @return the provider name
     */
    public String getProvider() {
        return provider;
    }
}
