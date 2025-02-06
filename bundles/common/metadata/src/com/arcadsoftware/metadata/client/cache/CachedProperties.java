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
package com.arcadsoftware.metadata.client.cache;

import java.util.Date;
import java.util.Properties;


public class CachedProperties implements ICacheableObject {

	private static final long serialVersionUID = -4049880696251582466L;
	private Properties content;
	private Date lastTest = new Date();
	private Date lastModification;
	private boolean lazzy = false;

	public CachedProperties() {
		super();
	}

	public CachedProperties(Date lastTest, Date lastModification, Properties content) {
		this(lastModification, content);
		this.lastTest = lastTest;
	}

	public CachedProperties(Date lastModification, Properties content) {
		this();
		this.content = content;
		this.lastModification = lastModification;
	}

	public Object getContent() {
		return content;
	}

	public void setContent(Object content) {
		if (content instanceof Properties) {
			this.content = (Properties) content;
		}
	}

	public Date getLastTest() {
		return lastTest;
	}

	public void setLastTest(Date lastTest) {
		this.lastTest = lastTest;
	}

	public Date getLastModification() {
		return lastModification;
	}

	public void setLastModification(Date lastModification) {
		this.lastModification = lastModification;
	}

	public boolean isLazzy() {
		return lazzy;
	}

	public void setLazzy(boolean lazzy) {
		this.lazzy = lazzy;
	}

	public boolean isCachableList() {
		return false;
	}

	public void setCachableList(boolean list) {
		// Do nothing
	}

}
