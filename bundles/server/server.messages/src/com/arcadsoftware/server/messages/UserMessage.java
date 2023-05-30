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
package com.arcadsoftware.server.messages;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * 
 */
public class UserMessage {

	protected final static String TO_STRING_FORMAT = "[%1$s] %2$s : %3$s"; //$NON-NLS-1$
	
	private String code;
	private String textLevel1;
	private String textLevel2;
	
	/**
	 * 
	 */
	public UserMessage() {
		super();
	}
	
	/**
	 * 
	 */
	public UserMessage(String code, String textLevel1,String textlevel2) {
		this();
		this.code = code;
		this.textLevel1 = textLevel1;
		this.textLevel2 = textlevel2;
	}

	/**
	 * 
	 */
	public String getCode() {
		return code;
	}

	/**
	 * 
	 */
	public void setCode(String code) {
		this.code = code;
	}

	/**
	 * 
	 */
	public String getTextLevel1() {
		return textLevel1;
	}

	/**
	 * 
	 */
	public void setTextLevel1(String textLevel1) {
		this.textLevel1 = textLevel1;
	}

	/**
	 * 
	 */
	public String getTextLevel2() {
		return textLevel2;
	}

	/**
	 * 
	 */
	public void setTextLevel2(String textLevel2) {
		this.textLevel2 = textLevel2;
	}
	
	/**
	 * 
	 */
	public String getLabel() {
		return code+": "+textLevel1; //$NON-NLS-1$
	}
	
	/**
	 * 
	 */
	public void fromBeanMap(BeanMap bean) {
		String code = bean.getString(IMSGConstants.MESSAGE_CODE);
		String level1 = bean.getString(IMSGConstants.MESSAGE_TXTLVL1);
		String level2 = bean.getString(IMSGConstants.MESSAGE_TXTLVL2);
		setCode(code);
		setTextLevel1(level1);
		setTextLevel2(level2);
	}	
	
	/**
	 * 
	 */
	public BeanMap toBeanMap() {
		BeanMap message = new BeanMap(getMessageEntity(), 1);
		message.put(IMSGConstants.MESSAGE_CODE,getCode());
		message.put(IMSGConstants.MESSAGE_TXTLVL1,getTextLevel1());
		String level2 = getTextLevel2();
		if((level2 != null) && (level2.length() > 0)) {
			message.put(IMSGConstants.MESSAGE_TXTLVL2,getTextLevel2());
		}
		return message;		
	}
	
	/**
	 * 
	 */
	public String getMessageEntity() {
		return IMSGConstants.ENTITY_MESSAGE;
	}
	
	/**
	 * 
	 */
	public String getStringRepresentation() {
		return String.format(TO_STRING_FORMAT, code, textLevel1, textLevel2);
	}
}