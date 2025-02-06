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
package com.arcadsoftware.server.messages;

import java.util.ArrayList;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 * 
 */
public class StatusMessage extends LogMessage implements IMSGConstants {
	
	/**
	 * 
	 */
	public static StatusMessage failStatusMessage(UserMessage informationMessage) {
		return new StatusMessage(informationMessage, MESSAGE_STATUS_FAILED);
	}
	
	/**
	 * 
	 */
	public static StatusMessage successStatusMessage(UserMessage informationMessage) {
		return new StatusMessage(informationMessage, MESSAGE_STATUS_OK);
	}	

	private int status;
	private final ArrayList<LogMessage> details = new ArrayList<LogMessage>();
	
	/**
	 * 
	 */
	public StatusMessage(UserMessage m,int status) {
		this(m.getCode(), m.getTextLevel1(), m.getTextLevel2(), status);
	}
	
	/**
	 * 
	 */
	public StatusMessage(String code, String textLevel1, String textlevel2, int status) {
		super(code, textLevel1, textlevel2, MESSAGE_LEVEL_INFO);
		this.status = status;		
	}
	
	/**
	 * Default constructor.
	 */
	public StatusMessage() {
		super();
	}
	
	@Override
	public String getMessageEntity() {
		return ENTITY_STATUSMESSAGE;
	}
	
	@Override
	public void fromBeanMap(BeanMap bean) {
		super.fromBeanMap(bean);
		setStatus(bean.getInt(MESSAGE_STATUS));	
		details.clear();
		BeanMapList messages = bean.getBeanMapList(MESSAGE_DETAILS);
		if (messages != null) {
			for(BeanMap b: messages) {
				String code = b.getString(MESSAGE_CODE);
				String lvl1 = b.getString(MESSAGE_TXTLVL1);
				String lvl2 = b.getString(MESSAGE_TXTLVL2);
				details.add(new LogMessage(code, lvl1, lvl2, b.getInt(MESSAGE_LEVEL)));
			}
		}
	}
	
	@Override
	public BeanMap toBeanMap() {
		BeanMap result = super.toBeanMap();
		result.put(MESSAGE_STATUS, status);
		BeanMapList l = new BeanMapList();
		for (LogMessage log : details) {
			BeanMap b = log.toBeanMap();
			l.add(b);
		}
		result.put(MESSAGE_DETAILS, l);		
		return result;
	}
	
	/**
	 * 
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * 
	 */
	public void setStatus(int status) {
		this.status = status;
	}

	/**
	 * 
	 */
	public void addDetail(LogMessage log) {
		details.add(log);
	}
	
	/**
	 * 
	 */
	public void setInformation(UserMessage informationMessage) {
		setCode(informationMessage.getCode());
		setTextLevel1(informationMessage.getTextLevel1());
		setTextLevel2(informationMessage.getTextLevel2());
	}

}
