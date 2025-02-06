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
package com.arcadsoftware.cleanup.operation;

import com.arcadsoftware.cleanup.CleanupManager;

public abstract class AbstractCleanOperation {
	
	protected CleanupManager cleanupManager;
	protected String id;

	public AbstractCleanOperation(String id) {
		this.id = id;
	}

	public String getId() {
		return id;
	}

	public void setParentCleanupManager(CleanupManager parent) {
		cleanupManager = parent;
	}

	public abstract boolean executeCleanOperation(boolean ignoreConditions);

	@Override
	public String toString() {
		String className = this.getClass().getSimpleName();
		if ((className == null) || className.isEmpty()) {
			className = "Customized CleanOperation";
		}
		return String.format("[%1$s] %2$s", className, id); //$NON-NLS-1$
	}
}
