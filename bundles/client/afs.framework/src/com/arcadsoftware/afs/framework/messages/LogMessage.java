/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.afs.framework.messages;

import com.arcadsoftware.beanmap.BeanMap;

public class LogMessage extends UserMessage {
	
	private int level;
	
	public LogMessage(UserMessage m, int level) {
		this(m.getCode(), m.getTextLevel1(),m.getTextLevel2(),level);
	}
	
	public LogMessage(String code, String textLevel1, String textlevel2, int level) {
		super(code, textLevel1, textlevel2);
		this.level = level;
	}
	
	public LogMessage() {
		super();
	}
	
	public void fromBeanMap(BeanMap bean) {
		super.fromBeanMap(bean);
		int level = bean.getInt(IMSGConstants.MESSAGE_LEVEL);
		setLevel(level);		
	}
	
	@Override
	public BeanMap toBeanMap() {
		BeanMap result = super.toBeanMap();
		result.put(IMSGConstants.MESSAGE_LEVEL, level);
		return result;
	}

	public int getLevel() {
		return level;
	}

	public void setLevel(int level) {
		this.level = level;
	}
	
	public String getMessageEntity() {
		return IMSGConstants.ENTITY_MESSAGE;
	}

	@Override
	public String toString() {		
		String value = super.toString();
		value = value + "\nLevel: " + level;
		return value;
	}	
	
}
