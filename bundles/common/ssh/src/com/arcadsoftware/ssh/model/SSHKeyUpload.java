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
 * This class is just a tricky facade to a BeanMap container.
 * 
 * @author ARCAD Software
 */
/**
 * Represents an SSH key upload operation, including the key data and upload status.
 * <p>
 * This class implements the {@link IIdentifiedBean} interface and uses a {@link BeanMap}
 * to store properties such as the SSH key name, passphrase, private key content,
 * upload success status, and an optional message.
 * </p>
 * 
 * @author ARCAD Software
 */
public class SSHKeyUpload implements IIdentifiedBean {

    /**
     * The entity name used in the underlying {@link BeanMap}.
     */
    public static final String ENTITY = "sshUpload"; //$NON-NLS-1$

    /**
     * The property key for the SSH key's name (reused from {@link SSHKey#NAME}).
     */
    public static final String NAME = SSHKey.NAME;

    /**
     * The property key for the SSH key's passphrase (reused from {@link SSHKey#PASSPHRASE}).
     */
    public static final String PASSPHRASE = SSHKey.PASSPHRASE;

    /**
     * The property key for the private key content.
     */
    public static final String PRIVATE_KEY = "privatekey"; //$NON-NLS-1$

    /**
     * The property key indicating if the upload was successful.
     */
    public static final String SUCCESSFUL = "successful"; //$NON-NLS-1$

    /**
     * The property key for an optional message related to the upload.
     */
    public static final String MESSAGE = "message"; //$NON-NLS-1$

    /**
     * The underlying map storing upload properties.
     */
    private final BeanMap beanmap;

    /**
     * Constructs a new {@code SSHKeyUpload} with a default empty {@link BeanMap}.
     */
    public SSHKeyUpload() {
        this(new BeanMap(ENTITY));
    }

    /**
     * Constructs a new {@code SSHKeyUpload} initialized with a {@link BeanMap} identified by the given type string.
     *
     * @param type the type name to initialize the {@link BeanMap}
     */
    public SSHKeyUpload(final String type) {
        this(new BeanMap(type));
    }

    /**
     * Constructs a new {@code SSHKeyUpload} using the specified {@link BeanMap}.
     *
     * @param beanmap the bean map holding upload properties, must not be null
     * @throws NullPointerException if the {@code beanmap} is null
     */
    public SSHKeyUpload(final BeanMap beanmap) {
        this.beanmap = beanmap;
        if (beanmap == null) {
            throw new NullPointerException("A BeanMap is required when creating a SSHKeyUpload !");
        }
    }

    /**
     * Returns the underlying {@link BeanMap} containing upload properties.
     *
     * @return the bean map of this SSH key upload
     */
    public BeanMap getBeanmap() {
        return beanmap;
    }

    /**
     * Returns the unique identifier of this SSH key upload.
     *
     * @return the ID of the upload
     */
    @Override
    public int getId() {
        return beanmap.getId();
    }

    /**
     * Returns the message associated with the upload operation.
     * <p>
     * This may contain informational or error messages.
     * </p>
     *
     * @return the upload message, or null if none is set
     */
    public String getMessage() {
        return beanmap.getString(MESSAGE);
    }

    /**
     * Indicates whether the SSH key upload was successful.
     *
     * @return {@code true} if the upload succeeded; {@code false} otherwise
     */
    public boolean isSuccessful() {
        return beanmap.getBoolean(SUCCESSFUL);
    }

    /**
     * Sets the message related to the upload operation.
     *
     * @param message the message string to set
     */
    public void setMessage(final String message) {
        beanmap.put(MESSAGE, message);
    }

    /**
     * Sets the success status of the SSH key upload.
     *
     * @param successful {@code true} if upload succeeded; {@code false} otherwise
     */
    public void setSuccessful(final boolean successful) {
        beanmap.put(SUCCESSFUL, successful);
    }

    /**
     * Returns the name of the SSH key being uploaded.
     *
     * @return the SSH key name, or null if not set
     */
    public String getName() {
        return beanmap.getString(NAME);
    }

    /**
     * Sets the name of the SSH key being uploaded.
     *
     * @param name the SSH key name to set
     */
    public void setName(final String name) {
        beanmap.put(NAME, name);
    }

    /**
     * Returns the passphrase protecting the SSH key being uploaded.
     *
     * @return the passphrase, or null if none is set
     */
    public String getPassphrase() {
        return beanmap.getString(PASSPHRASE);
    }

    /**
     * Sets the passphrase protecting the SSH key being uploaded.
     *
     * @param passphrase the passphrase string to set
     */
    public void setPassphrase(final String passphrase) {
        beanmap.put(PASSPHRASE, passphrase);
    }

    /**
     * Returns the private key content being uploaded.
     *
     * @return the private key string, or null if not set
     */
    public String getPrivateKey() {
        return beanmap.getString(PRIVATE_KEY);
    }

    /**
     * Sets the private key content being uploaded.
     *
     * @param privateKey the private key string to set
     */
    public void setPrivateKey(final String privateKey) {
        beanmap.put(PRIVATE_KEY, privateKey);
    }
}
