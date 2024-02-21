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
package com.arcadsoftware.rest.connection;

import com.arcadsoftware.rest.XStreamCompact;

/**
 * Specific XStream mapper to convert ConnectionUserBean objects.
 */
public class XmlCurrentUserStream extends XStreamCompact {

	/**
	 * Constructor, configured to convert ConnectionUserBean objects.
	 */
	public XmlCurrentUserStream() {
		super(XmlCurrentUserStream.class.getClassLoader());
		alias("user", ConnectionUserBean.class); //$NON-NLS-1$
		useAttributeFor(ConnectionUserBean.class, "id"); //$NON-NLS-1$
		useAttributeFor(ConnectionUserBean.class, "principal"); //$NON-NLS-1$
		useAttributeFor(ConnectionUserBean.class, "changePWD"); //$NON-NLS-1$
		useAttributeFor(ConnectionUserBean.class, "canChangePWD"); //$NON-NLS-1$
		useAttributeFor(ConnectionUserBean.class, "login"); //$NON-NLS-1$ //$NON-NLS-2$
		aliasAttribute(ConnectionUserBean.class, "fullname", "name"); //$NON-NLS-1$ //$NON-NLS-2$
		alias("profile", Profile.class); //$NON-NLS-1$
		addImplicitCollection(Profile.class, "rights"); //$NON-NLS-1$
		alias("right", Right.class); //$NON-NLS-1$
		useAttributeFor(Right.class, "id"); //$NON-NLS-1$
		useAttributeFor(Right.class, "param"); //$NON-NLS-1$		
		alias("autoprofile", AutoProfile.class); //$NON-NLS-1$
		useAttributeFor(AutoProfile.class, "limit"); //$NON-NLS-1$		
	}

}
