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
public class LogMessage extends UserMessage implements IMSGConstants {
	
	private int level;
	
	/**
	 * 
	 */
	public LogMessage(UserMessage m,int level) {
		this(m.getCode(), m.getTextLevel1(), m.getTextLevel2(), level);
	}
	
	/**
	 * 
	 */
	public LogMessage(String code, String textLevel1, String textlevel2, int level) {
		super(code, textLevel1, textlevel2);
		this.level = level;
	}
	
	/**
	 * 
	 */
	public LogMessage() {
		super();
	}
	
	/**
	 * 
	 */
	@Override
	public void fromBeanMap(BeanMap bean) {
		super.fromBeanMap(bean);
		setLevel(bean.getInt(MESSAGE_LEVEL));		
	}
	
	/**
	 * 
	 */
	@Override
	public BeanMap toBeanMap() {
		BeanMap result = super.toBeanMap();
		result.put(MESSAGE_LEVEL,level);
		return result;
	}

	/**
	 * 
	 */
	public int getLevel() {
		return level;
	}

	/**
	 * 
	 */
	public void setLevel(int level) {
		this.level = level;
	}
	
	/**
	 * 
	 */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder();
		switch (level) {
		case MESSAGE_LEVEL_ERROR:
			result.append("[ERROR  ] "); //$NON-NLS-1$
			break;
		case MESSAGE_LEVEL_WARNING:
			result.append("[WARNING] "); //$NON-NLS-1$
			break;			
		case MESSAGE_LEVEL_INFO:
			result.append("[INFO   ] "); //$NON-NLS-1$
			break;
		case MESSAGE_LEVEL_VERBOSE:
			result.append("[VERBOSE] "); //$NON-NLS-1$
			break;
		default:
			result.append("[VERBOSE]"); //$NON-NLS-1$
			break;		
		}
		result.append(getLabel());
		String level2 = getTextLevel2();
		if(level2 != null && level2.length() > 0){
			result.append("\n") //$NON-NLS-1$
					.append(getTextLevel2());
		}
		return result.toString();		
	}

}