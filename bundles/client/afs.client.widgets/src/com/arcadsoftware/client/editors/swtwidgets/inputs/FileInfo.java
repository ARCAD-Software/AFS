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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

public class FileInfo {

	private String absoluteFilename;
	private String relativeFilename;

	public FileInfo() {
		super();
	}

	public FileInfo(String absoluteFilename, String relativeFilename) {
		this();
		this.absoluteFilename = absoluteFilename;
		this.relativeFilename = relativeFilename;
	}

	public String getAbsoluteFilename() {
		return absoluteFilename;
	}

	public void setAbsoluteFilename(String absoluteFilename) {
		this.absoluteFilename = absoluteFilename;
	}

	public String getRelativeFilename() {
		return relativeFilename;
	}

	public void setRelativeFilename(String relativeFilename) {
		this.relativeFilename = relativeFilename;
	}
}