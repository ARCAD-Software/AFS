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
package com.arcadsoftware.afs.client.server.admin.common.core.model.category;

import java.util.List;

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.rest.console.Category;
import com.arcadsoftware.rest.console.SectionId;

public class CategoriesWrapper extends ArcadCollection {

	public CategoriesWrapper(List<Category> list) {
		for (final Category c : list) {
			final CategoryWrapper categoryWrapper = new CategoryWrapper(c);
			add(categoryWrapper);
			categoryWrapper.setParent(this);
			for (final SectionId sid : c.getList()) {
				add(new CategoryWrapper(sid));
			}
		}
	}
}