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

import com.arcadsoftware.beanmap.BeanMap;

/**
 * Represents a log message with a severity level, extending {@link UserMessage} and implementing {@link IMSGConstants}.
 * <p>
 * This class adds a logging level (error, warning, info, verbose) to a structured user message.
 * It supports serialization and deserialization through {@link BeanMap}, and provides a custom string representation
 * based on the message level.
 */
public class LogMessage extends UserMessage implements IMSGConstants {
	
	private int level;

	/**
	 * Constructs a new {@code LogMessage} from an existing {@link UserMessage} with the specified level.
	 *
	 * @param m the base user message
	 * @param level the severity level of the message (e.g., {@link IMSGConstants#MESSAGE_LEVEL_ERROR})
	 */
	public LogMessage(UserMessage m, int level) {
		this(m.getCode(), m.getTextLevel1(), m.getTextLevel2(), level);
	}

	/**
	 * Constructs a new {@code LogMessage} with the given components.
	 *
	 * @param code the message code
	 * @param textLevel1 the main message text
	 * @param textlevel2 the secondary message text (optional)
	 * @param level the severity level of the message
	 */
	public LogMessage(String code, String textLevel1, String textlevel2, int level) {
		super(code, textLevel1, textlevel2);
		this.level = level;
	}

	/**
	 * Default constructor creating an empty {@code LogMessage}.
	 */
	public LogMessage() {
		super();
	}

	/**
	 * Populates the {@code LogMessage} from a {@link BeanMap}.
	 *
	 * @param bean the {@code BeanMap} containing message data
	 */
	@Override
	public void fromBeanMap(BeanMap bean) {
		super.fromBeanMap(bean);
		setLevel(bean.getInt(MESSAGE_LEVEL));		
	}

	/**
	 * Serializes the {@code LogMessage} into a {@link BeanMap}.
	 *
	 * @return a {@code BeanMap} representing the message
	 */
	@Override
	public BeanMap toBeanMap() {
		BeanMap result = super.toBeanMap();
		result.put(MESSAGE_LEVEL, level);
		return result;
	}

	/**
	 * Returns the severity level of the message.
	 *
	 * @return the message level
	 */
	public int getLevel() {
		return level;
	}

	/**
	 * Sets the severity level of the message.
	 *
	 * @param level the new message level
	 */
	public void setLevel(int level) {
		this.level = level;
	}

	/**
	 * Returns a string representation of the log message,
	 * including its severity prefix and text levels.
	 *
	 * @return the formatted log message as a string
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
		default:
			result.append("[VERBOSE] "); //$NON-NLS-1$
			break;		
		}
		result.append(getLabel());
		String level2 = getTextLevel2();
		if (level2 != null && level2.length() > 0) {
			result.append("\n") //$NON-NLS-1$
				  .append(getTextLevel2());
		}
		return result.toString();		
	}
}
