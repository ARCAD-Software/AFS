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
package com.arcadsoftware.metadata.xml;

import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;

public class JsonCriteriaStream extends JSonBeanMapStream {

	public JsonCriteriaStream() {
		super(JsonMetaDataStream.class.getClassLoader(), true, true, false);
	}
	
	@Override
	protected void InitializeBase() {
		super.InitializeBase();
		XmlCriteriaStream.initialize(this);
		setMode(NO_REFERENCES);
	}

	@Override
	public Object fromXML(String xml) {
		return super.fromXML(xml);
	}

	@Override
	public String toXML(Object object) {
		return super.toXML(object);
	}
}