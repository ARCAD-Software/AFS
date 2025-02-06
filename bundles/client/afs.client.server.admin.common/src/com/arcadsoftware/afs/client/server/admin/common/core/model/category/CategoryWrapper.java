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

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.afs.client.server.admin.common.ui.IIconConsts;
import com.arcadsoftware.rest.console.Category;
import com.arcadsoftware.rest.console.SectionId;

public class CategoryWrapper extends ArcadEntity {

	private Category category;
	private SectionId sectionId = null;
	private boolean isCategory = true;

	public CategoryWrapper(Category category) {
		this.category = category;
		setLevel(1);
		isCategory = true;
	}

	public CategoryWrapper(SectionId section) {
		sectionId = section;
		setLevel(2);
		isCategory = false;
	}

	@Override
	public String getLabel() {
		if (isCategory) {
			return category.getLabel();
		} else {
			return sectionId.getLabel();
		}

	}

	public Category getCategory() {
		return category;
	}

	public SectionId getSectionId() {
		return sectionId;
	}

	public boolean isSection() {
		return !isCategory;
	}

	@Override
	public String getIconID() {
		if (isCategory) {
			return IIconConsts.CATEGORY;
		} else {
			return IIconConsts.SECTION;
		}
	}

}
