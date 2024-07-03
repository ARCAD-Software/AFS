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
package com.arcadsoftware.afs.framework.ui.views;

import org.eclipse.jface.viewers.StructuredViewer;

import com.arcadsoftware.aev.core.ui.container.RootContainerInput;

public abstract class AFSRootContainerInput extends RootContainerInput {

	public AFSRootContainerInput(StructuredViewer viewer) {
		super(viewer);
	}

	public abstract String getParentViewId();
}
