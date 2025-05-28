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
 * Defines the REST API endpoints related to SSH key management.
 * <p>
 * This interface contains constants representing the URL paths
 * for SSH key operations such as generating, importing, and retrieving public keys.
 * </p>
 * 
 * @author ARCAD Software
 */
public interface SSHRoutes {

    /**
     * The endpoint to generate a new SSH key.
     * <p>HTTP Method: PUT, POST</p>
     * <p>Example usage: <code>/sshgeneratekey</code></p>
     */
    public static final String GENERATE_KEY = "/sshgeneratekey"; //$NON-NLS-1$

    /**
     * The endpoint to import an existing SSH key.
     * <p>HTTP Method: PUT, POST</p>
     * <p>Example usage: <code>/importsshkey</code></p>
     */
    public static final String IMPORT_KEY = "/importsshkey"; //$NON-NLS-1$

    /**
     * The endpoint to retrieve the public part of a stored SSH key by its ID.
     * <p>HTTP Method: GET</p>
     * <p>Path parameter: <code>{id}</code> – the identifier of the SSH key</p>
     * <p>Example usage: <code>/sshpublickey/123</code></p>
     */
    public static final String PUBLIC_KEY = "/sshpublickey/{id}"; //$NON-NLS-1$
}
