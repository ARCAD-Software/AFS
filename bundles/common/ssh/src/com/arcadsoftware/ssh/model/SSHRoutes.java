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

public class SSHRoutes {
	
	public static final String GENERATE_KEY = "/sshgeneratekey"; //$NON-NLS-1$
	public static final String IMPORT_KEY = "/importsshkey"; //$NON-NLS-1$
	public static final String PUBLIC_KEY = "/sshpublickey/{id}"; //$NON-NLS-1$

	private SSHRoutes() {}
}
