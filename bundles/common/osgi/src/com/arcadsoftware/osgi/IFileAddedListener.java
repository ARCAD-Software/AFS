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
package com.arcadsoftware.osgi;

import java.io.File;

import org.osgi.framework.Bundle;

/**
 * Creation Date: 25 mai 2012
 * @see FileBundleTracker
 */
public interface IFileAddedListener {

	public boolean addFile(Bundle bundle, File file);
	
}
