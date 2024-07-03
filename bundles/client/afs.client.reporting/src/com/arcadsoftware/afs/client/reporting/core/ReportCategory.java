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
package com.arcadsoftware.afs.client.reporting.core;

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.afs.client.reporting.Activator;

public class ReportCategory extends ArcadEntity {
	public static final String DEFAULT_CATEGORY = Activator.resString("category.default"); //$NON-NLS-1$
	public static final char CATEGORY_SEPARATOR = '/';
	private String category = ""; //$NON-NLS-1$

	public ReportCategory(String category, int level) {
		super();
		this.category = category;
		setLevel(level);
	}

	@Override
	public String getLabel() {
		final int pos = category.lastIndexOf(CATEGORY_SEPARATOR);
		if (pos != -1) {
			return category.substring(pos + 1);
		}
		return category;
	}
}
