/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.afs.client.core.ui.views;

import com.arcadsoftware.afs.client.core.IACCMessages;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;

public abstract class AbstractSecuredView extends AbstractConnectedView implements ISecuredAction {

	private boolean allowedToSearch;
	
	public boolean isAllowed() {
		if (getConnection() != null) {
			allowedToSearch = getConnection().isAllowed(getExpectedRigths()); 
		} else {
			allowedToSearch = false;
			LogUITools.logError(Activator.getDefault().getBundle(), UserMessageManager.getInstance().getMessage(IACCMessages.ERR_SRH_CONNECTIONMISSING));
		}
		return allowedToSearch;
	}

	/**
	 * FIXME this value is undefined if isAllowed() is not called before !!!
	 * 
	 * @return
	 * @deprecated use {@link #isAllowed()}
	 */
	public boolean isAllowedToSearch() {
		return allowedToSearch;
	}
	
	
}
