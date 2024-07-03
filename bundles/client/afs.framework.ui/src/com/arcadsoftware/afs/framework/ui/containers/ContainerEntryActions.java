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
package com.arcadsoftware.afs.framework.ui.containers;

import com.arcadsoftware.aev.core.ui.actions.ArcadActions;
import com.arcadsoftware.aev.core.ui.container.Container;

public abstract class ContainerEntryActions extends ArcadActions {

	public ContainerEntryActions() {
		this(null, true);
	}

	public ContainerEntryActions(boolean initAction) {
		this(null, initAction);
	}

	public ContainerEntryActions(Container container, boolean initAction) {
		super(container);
		if (initAction) {
			makeAction();
		}
	}

}
