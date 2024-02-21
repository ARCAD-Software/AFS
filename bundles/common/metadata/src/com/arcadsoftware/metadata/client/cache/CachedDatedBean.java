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
package com.arcadsoftware.metadata.client.cache;

import java.util.Date;

import com.arcadsoftware.beanmap.IDatedBean;

public class CachedDatedBean implements ICacheableObject {

	private static final long serialVersionUID = 7730718895404986167L;
	private IDatedBean content;
	private Date lastTest = new Date();
	private boolean lazzy = false;
	private boolean list = false;

	public CachedDatedBean() {
		super();
	}

	public CachedDatedBean(Date lastTest, IDatedBean content) {
		this(content);
		this.lastTest = lastTest;
	}

	public CachedDatedBean(IDatedBean content) {
		this();
		this.content = content;
	}

	public CachedDatedBean(IDatedBean content, boolean lazzy) {
		this(content);
		this.lazzy = lazzy;
	}

	public Object getContent() {
		return content;
	}

	public void setContent(Object content) {
		if (content instanceof IDatedBean) {
			this.content = (IDatedBean) content;
		}
	}

	public Date getLastTest() {
		return lastTest;
	}

	public void setLastTest(Date lastTest) {
		this.lastTest = lastTest;
	}

	public boolean isLazzy() {
		return lazzy;
	}

	public void setLazzy(boolean lazzy) {
		this.lazzy = lazzy;
	}

	public Date getLastModification() {
		if (content == null) {
			return new Date(0);
		}
		return content.getDate();
	}

	public boolean isCachableList() {
		return list;
	}

	public void setLastModification(Date date) {
		if (content != null) {
			content.setDate(date);
		}
	}

	public void setCachableList(boolean list) {
		this.list = list;
	}

}
