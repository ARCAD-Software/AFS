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

public class ReportHeader extends ArcadEntity {

	private String name = ""; //$NON-NLS-1$
	private String description = "";//$NON-NLS-1$
	private String category = "";//$NON-NLS-1$
	private String urlOrFilename = ""; //$NON-NLS-1$
	private String version = "";//$NON-NLS-1$

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	public String getUrlOrFilename() {
		return urlOrFilename;
	}

	public String getVersion() {
		return (version == null) ? "" : version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public ReportHeader(String filename, String name, String description, String category) {
		this(filename, name, description, category, null);
	}

	public ReportHeader(String filename, String name, String description, String category, String version) {
		super();
		urlOrFilename = filename;
		this.name = name;
		this.description = description;
		if ((category == null) || category.equals("")) { //$NON-NLS-1$
			this.category = ReportCategory.DEFAULT_CATEGORY;
		} else { // $NON-NLS-1$
			this.category = category;
		}

		if (version == null) {
			this.version = ""; //$NON-NLS-1$
		} else {
			this.version = version;
		}
	}

	@Override
	public String getLabel() {
		return name;
	}

	public boolean isEqualTo(ReportHeader report) {
		if (report != null) {
			return report.getUrlOrFilename().equalsIgnoreCase(urlOrFilename)
					&& report.getCategory().equalsIgnoreCase(category);
		}
		return false;
	}

}
