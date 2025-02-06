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
package com.arcadsoftware.afs.client.server.ui.propertysection;

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.ui.propertysection.AbstractAFSPropertySection;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.propertysource.ServerPropertySource;

public class ServerConnectionPropertySection extends AbstractAFSPropertySection {

	@Override
	public String getSectionTitle() {
		return Activator.resString("label.server"); //$NON-NLS-1$
	}

	@Override
	public void createContent(Composite parent) {
		createText(parent, Activator.resString("label.server"), ServerPropertySource.PROPERTY_NAME, true); //$NON-NLS-1$
		createText(parent, Activator.resString("label.url"), ServerPropertySource.PROPERTY_URL);//$NON-NLS-1$
	}
}
