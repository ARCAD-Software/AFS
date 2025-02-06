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
package com.arcadsoftware.afs.client.core.ui.propertypage;

import java.util.List;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.framework.ui.propertypage.AbstractAFSPropertyPage;
import com.arcadsoftware.metadata.client.DataAccess;
import com.arcadsoftware.rest.connection.ConnectionUserBean;

public abstract class AbstractConnectedPropertypage extends AbstractAFSPropertyPage {

	protected ServerConnection getServerConnection() {
		final IAdaptable element = getElement();
		if (element != null) {
			final Object o = element.getAdapter(ServerConnection.class);
			if (o instanceof ServerConnection) {
				return (ServerConnection) o;
			}
		}
		return null;
	}

	protected ConnectionUserBean getUser() {
		final ServerConnection cnx = getServerConnection();
		if (cnx != null) {
			return cnx.getUser();
		}
		return null;
	}

	protected DataAccess getDataAccess() {
		final ServerConnection cnx = getServerConnection();
		if (cnx != null) {
			return cnx.getDataAccess();
		}
		return null;
	}

	public boolean isAllowed() {
		final ServerConnection connection = getServerConnection();
		return (connection != null) && connection.isAllowed(getExpectedRigths());
	}

	@Override
	protected Control createContents(Composite parent) {
		// Check rights, if any defined
		if (!isAllowed()) {
			final Label notAllowed = new Label(parent, SWT.NONE);
			notAllowed.setText(Activator.resString("search.label.notAllowed")); //$NON-NLS-1$
			return notAllowed;
		}
		return null;
	}

	public List<Integer> getExpectedRigths() {
		return null;
	}

}
