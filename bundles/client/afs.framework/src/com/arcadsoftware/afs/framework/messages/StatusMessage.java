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

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class StatusMessage extends LogMessage {
	
	public static StatusMessage failStatusMessage(UserMessage informationMessage) {
		return new StatusMessage(informationMessage,IMSGConstants.MESSAGE_STATUS_FAILED);
	}
	
	public static StatusMessage successStatusMessage(UserMessage informationMessage) {
		return new StatusMessage(informationMessage,IMSGConstants.MESSAGE_STATUS_OK);
	}	

	private int status;
	private final ArrayList<LogMessage> details = new ArrayList<LogMessage>();
	
	public StatusMessage(UserMessage m,int status) {
		this(m.getCode(), m.getTextLevel1(),m.getTextLevel2(),status);
	}
	
	public StatusMessage(String code, String textLevel1,String textlevel2,int status) {
		super(code,textLevel1,textlevel2,IMSGConstants.MESSAGE_LEVEL_INFO);
		this.status = status;		
	}
	
	public StatusMessage() {
		super();
	}

	@Override
	public String getMessageEntity() {
		return IMSGConstants.ENTITY_STATUSMESSAGE;
	}
	
	public void fromBeanMap(BeanMap bean) {
		super.fromBeanMap(bean);
		int status = bean.getInt(IMSGConstants.MESSAGE_STATUS);
		setStatus(status);	
		details.clear();
		BeanMapList messages = (BeanMapList)bean.get(IMSGConstants.MESSAGE_DETAILS); 
		for (BeanMap b:messages) {
			String code = b.getString(IMSGConstants.MESSAGE_CODE);
			String lvl1 = b.getString(IMSGConstants.MESSAGE_TXTLVL1);
			String lvl2 = b.getString(IMSGConstants.MESSAGE_TXTLVL2);
			int level = b.getInt(IMSGConstants.MESSAGE_LEVEL);
			LogMessage log = new LogMessage(code, lvl1,lvl2,level);
			details.add(log);
		}
	}
	
	@Override
	public BeanMap toBeanMap() {
		BeanMap result = super.toBeanMap();
		result.put(IMSGConstants.MESSAGE_STATUS, status);
		BeanMapList l = new BeanMapList();
		for (LogMessage log : details) {
			BeanMap b = log.toBeanMap();
			l.add(b);
		}
		result.put(IMSGConstants.MESSAGE_DETAILS, l);		
		return result;
	}
	
	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public void addDetail(LogMessage log) {
		details.add(log);
	}
	
	public List<LogMessage> getDetails() {
		return details;
	}
	
	public void setInformation(UserMessage informationMessage) {
		setCode(informationMessage.getCode());
		setTextLevel1(informationMessage.getTextLevel1());
		setTextLevel2(informationMessage.getTextLevel2());
	}
}
