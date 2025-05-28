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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;

/**
 * Represents an SSH key with its associated properties.
 * <p>
 * This class implements the {@link IIdentifiedBean} interface and uses a {@link BeanMap}
 * to store and manage the key's attributes such as name, type, length, fingerprint, and passphrase.
 * </p>
 * 
 * @author ARCAD Software
 */
public class SSHKey implements IIdentifiedBean {

    /**
     * The entity name used in the underlying {@link BeanMap}.
     */
    public static final String ENTITY = "sshKey"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's name.
     */
    public static final String NAME = "name"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's fingerprint.
     */
    public static final String FINGERPRINT = "fingerprint"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's type.
     */
    public static final String TYPE = "keytype"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's length.
     */
    public static final String LENGTH = "keylength"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's comment.
     */
    public static final String COMMENT = "keycomment"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's passphrase.
     */
    public static final String PASSPHRASE = "passphrase"; //$NON-NLS-1$

    /**
     * The underlying map storing SSH key properties.
     */
    private final BeanMap beanmap;

    /**
     * Constructs a new {@code SSHKey} with a default empty {@link BeanMap}.
     */
    public SSHKey() {
        this(new BeanMap(ENTITY));
    }

    /**
     * Constructs a new {@code SSHKey} initialized with a {@link BeanMap} identified by the given type string.
     *
     * @param type the type name to initialize the {@link BeanMap}
     */
    public SSHKey(String type) {
        this(new BeanMap(type));
    }

    /**
     * Constructs a new {@code SSHKey} using the specified {@link BeanMap}.
     *
     * @param beanmap the bean map holding SSH key properties, must not be null
     * @throws NullPointerException if the {@code beanmap} is null
     */
    public SSHKey(final BeanMap beanmap) {
        this.beanmap = beanmap;
        if (beanmap == null) {
            throw new NullPointerException("A BeanMap is required when creating a SSHKey !");
        }
    }

    /**
     * Returns the underlying {@link BeanMap} containing SSH key properties.
     *
     * @return the bean map of this SSH key
     */
    public BeanMap getBeanMap() {
        return beanmap;
    }

    /**
     * Returns the comment associated with the SSH key.
     *
     * @return the key comment, or null if not set
     */
    public String getComment() {
        return beanmap.getString(COMMENT);
    }

    /**
     * Returns the fingerprint of the SSH key.
     *
     * @return the fingerprint string, or null if not set
     */
    public String getFingerprint() {
        return beanmap.getString(FINGERPRINT);
    }

    /**
     * Returns the unique identifier of this SSH key.
     *
     * @return the ID of the SSH key
     */
    @Override
    public int getId() {
        return beanmap.getId();
    }

    /**
     * Returns the length (in bits) of the SSH key.
     *
     * @return the key length
     */
    public int getLength() {
        return beanmap.getInt(LENGTH);
    }

    /**
     * Returns the name of the SSH key.
     *
     * @return the key name, or null if not set
     */
    public String getName() {
        return beanmap.getString(NAME);
    }

    /**
     * Returns the passphrase protecting the SSH key, if any.
     *
     * @return the passphrase, or null if not set
     */
    public String getPassphrase() {
        return beanmap.getString(PASSPHRASE);
    }

    /**
     * Returns the type of the SSH key.
     *
     * @return the {@link SSHKeyType} of the key, or null if type is not set or invalid
     */
    public SSHKeyType getType() {
        return SSHKeyType.fromName(beanmap.getString(TYPE));
    }

    /**
     * Indicates whether the SSH key is encrypted with a passphrase.
     *
     * @return {@code true} if the key has a non-empty passphrase; {@code false} otherwise
     */
    public boolean isEncrypted() {
        return (getPassphrase() != null) && !getPassphrase().isEmpty();
    }

    /**
     * Sets the comment for this SSH key.
     *
     * @param comment the comment string to set
     */
    public void setComment(final String comment) {
        beanmap.put(COMMENT, comment);
    }

    /**
     * Sets the fingerprint for this SSH key.
     *
     * @param fingerprint the fingerprint string to set
     */
    public void setFingerprint(final String fingerprint) {
        beanmap.put(FINGERPRINT, fingerprint);
    }

    /**
     * Sets the length (in bits) for this SSH key.
     *
     * @param length the key length to set
     */
    public void setLength(final int length) {
        beanmap.put(LENGTH, length);
    }

    /**
     * Sets the name for this SSH key.
     *
     * @param name the name string to set
     */
    public void setName(final String name) {
        beanmap.put(NAME, name);
    }

    /**
     * Sets the passphrase for this SSH key.
     *
     * @param passphrase the passphrase string to set
     */
    public void setPassphrase(final String passphrase) {
        beanmap.put(PASSPHRASE, passphrase);
    }

    /**
     * Sets the type of this SSH key.
     *
     * @param type the {@link SSHKeyType} to set
     */
    public void setType(final SSHKeyType type) {
        beanmap.put(TYPE, type.getName());
    }
}
