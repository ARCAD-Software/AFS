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


public class CachedString implements ICacheableObject {

	private static final long serialVersionUID = -7962197660122491520L;

	private String content;
	private Date lastTest = new Date();
	private Date lastModification;
	private boolean lazzy = false;

	public CachedString() {
		super();
	}

	public CachedString(Date lastTest, Date lastModification, String content) {
		this(lastModification, content);
		this.lastTest = lastTest;
	}

	public CachedString(Date lastTest, Date lastModification, String content, boolean lazzy) {
		this(lastTest, lastModification, content);
		this.lazzy = lazzy;
	}

	public CachedString(Date lastModification, String content, boolean lazzy) {
		this(lastModification, content);
		this.lazzy = lazzy;
	}

	public CachedString(Date lastModification, String content) {
		this();
		this.lastModification = lastModification;
		this.content = content;
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

	public Object getContent() {
		return content;
	}

	public void setContent(Object content) {
		this.content = content.toString();
	}

	public boolean isCachableList() {
		return false;
	}

	public boolean isLazzy() {
		return lazzy;
	}

	public void setCachableList(boolean list) {
		// Do nothing
	}

	public void setLazzy(boolean lazzy) {
		this.lazzy = lazzy;
	}

}
