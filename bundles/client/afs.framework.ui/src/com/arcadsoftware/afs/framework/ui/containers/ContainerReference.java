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
package com.arcadsoftware.afs.framework.ui.containers;

import org.eclipse.swt.graphics.Image;

public class ContainerReference {

	private int identifier = -1;
	private String label = ""; //$NON-NLS-1$
	private Image image;
	private String viewid = "";//$NON-NLS-1$
	private String uniqueKey = "";//$NON-NLS-1$
	private String id = "";//$NON-NLS-1$
	private String category = "";//$NON-NLS-1$

	public String getCategory() {
		if (category == null) {
			category = "";//$NON-NLS-1$
		}
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getUniqueKey() {
		return uniqueKey;
	}

	public void setUniqueKey(String uniqueKey) {
		this.uniqueKey = uniqueKey;
	}

	public Image getImage() {
		return image;
	}

	public String getLabel() {
		return label;
	}

	public int getIdentifier() {
		return identifier;
	}

	public String getViewid() {
		return viewid;
	}

	public void setViewid(String viewid) {
		this.viewid = viewid;
	}

	public void setIdentifier(int identifier) {
		this.identifier = identifier;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public void setImage(Image image) {
		this.image = image;
	}

}
