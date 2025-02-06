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
package com.arcadsoftware.afs.framework.messages;

import com.arcadsoftware.beanmap.BeanMap;

public class UserMessage {

	private String code;
	private String textLevel1;
	private String textLevel2;

	public UserMessage() {
	}

	public UserMessage(String code, String textLevel1, String textlevel2) {
		this.code = code;
		this.textLevel1 = textLevel1;
		textLevel2 = textlevel2;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getTextLevel1() {
		return textLevel1;
	}

	public void setTextLevel1(String textLevel1) {
		this.textLevel1 = textLevel1;
	}

	public String getTextLevel2() {
		return textLevel2;
	}

	public void setTextLevel2(String textLevel2) {
		this.textLevel2 = textLevel2;
	}

	public String getLabel() {
		return code + ": " + textLevel1; //$NON-NLS-1$
	}

	@Override
	public String toString() {
		return code + ": " + textLevel1 + "\n" + textLevel2; //$NON-NLS-1$ //$NON-NLS-2$
	}

	public void fromBeanMap(BeanMap bean) {
		final String code = bean.getString(IMSGConstants.MESSAGE_CODE);
		final String level1 = bean.getString(IMSGConstants.MESSAGE_TXTLVL1);
		final String level2 = bean.getString(IMSGConstants.MESSAGE_TXTLVL2);
		setCode(code);
		setTextLevel1(level1);
		setTextLevel2(level2);
	}

	public BeanMap toBeanMap() {
		final BeanMap message = new BeanMap(getMessageEntity());
		message.setId(1);
		message.put(IMSGConstants.MESSAGE_CODE, getCode());
		message.put(IMSGConstants.MESSAGE_TXTLVL1, getTextLevel1());
		message.put(IMSGConstants.MESSAGE_TXTLVL2, getTextLevel2());
		return message;
	}

	public String getMessageEntity() {
		return IMSGConstants.ENTITY_MESSAGE;
	}

}
