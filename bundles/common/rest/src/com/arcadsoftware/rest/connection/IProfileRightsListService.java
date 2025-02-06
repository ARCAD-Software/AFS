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
package com.arcadsoftware.rest.connection;

import java.util.List;

import org.restlet.data.Language;

/**
 * OSGi service used to provide documentation about the Access Right used by the given profile.
 * 
 * @author ARCAD Software
 */
public interface IProfileRightsListService {

	/**
	 * Get a non null list of Right information objects.
	 * 
	 * @param profile
	 * @param language
	 * @return
	 */
	public List<RightInfo> getProfileRights(Profile profile, Language language);
}
