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
package com.arcadsoftware.afs.client.ssh.internal;

import java.util.List;

import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.IRightManagerExtension;
import com.arcadsoftware.afs.client.core.singletons.SingletonManager;

public class RightManager implements IRightManagerExtension {

	public static RightManager getInstance() {
		return SingletonManager.get(RightManager.class);
	}

	private final IRightManagerExtension delegatedManager;

	private RightManager() {
		delegatedManager = AFSRightManager.getRightManager();
	}

	@Override
	public List<Integer> getExpectedRights(final String action) {
		if (delegatedManager != null) {
			return delegatedManager.getExpectedRights(action);
		}
		return null;
	}

	@Override
	public List<Integer> getExpectedRights(final String action, final boolean ignoreIfUndefined) {
		if (delegatedManager != null) {
			return delegatedManager.getExpectedRights(action, ignoreIfUndefined);
		}
		return null;
	}

	@Override
	public void missingRight(final List<Integer> expected) {
		if (delegatedManager != null) {
			delegatedManager.missingRight(expected);
		}
	}

}
