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
 * Represents a user-facing message consisting of a message code,
 * a primary text (level 1), and an optional secondary text (level 2).
 * <p>
 * This class is used to encapsulate structured messages that may be localized,
 * logged, or displayed to the user. It supports conversion to and from {@link BeanMap}
 * for serialization purposes.
 */
public class UserMessage {

	protected final static String TO_STRING_FORMAT = "[%1$s] %2$s : %3$s"; //$NON-NLS-1$

	private String code;
	private String textLevel1;
	private String textLevel2;

	/**
	 * Default constructor.
	 */
	public UserMessage() {
		super();
	}

	/**
	 * Constructs a {@code UserMessage} with the specified code and texts.
	 *
	 * @param code the message code (used for lookup or classification)
	 * @param textLevel1 the primary text of the message (short description)
	 * @param textlevel2 the secondary text of the message (details or explanation)
	 */
	public UserMessage(String code, String textLevel1, String textlevel2) {
		this();
		this.code = code;
		this.textLevel1 = textLevel1;
		this.textLevel2 = textlevel2;
	}

	/**
	 * Returns the message code.
	 *
	 * @return the message code
	 */
	public String getCode() {
		return code;
	}

	/**
	 * Sets the message code.
	 *
	 * @param code the new message code
	 */
	public void setCode(String code) {
		this.code = code;
	}

	/**
	 * Returns the primary text of the message.
	 *
	 * @return the level 1 text
	 */
	public String getTextLevel1() {
		return textLevel1;
	}

	/**
	 * Sets the primary text of the message.
	 *
	 * @param textLevel1 the new level 1 text
	 */
	public void setTextLevel1(String textLevel1) {
		this.textLevel1 = textLevel1;
	}

	/**
	 * Returns the secondary text of the message.
	 *
	 * @return the level 2 text
	 */
	public String getTextLevel2() {
		return textLevel2;
	}

	/**
	 * Sets the secondary text of the message.
	 *
	 * @param textLevel2 the new level 2 text
	 */
	public void setTextLevel2(String textLevel2) {
		this.textLevel2 = textLevel2;
	}

	/**
	 * Returns a short label for this message in the format: {@code "code: textLevel1"}.
	 *
	 * @return a short label string
	 */
	public String getLabel() {
		return code + ": " + textLevel1; //$NON-NLS-1$
	}

	/**
	 * Populates this message from the given {@link BeanMap}.
	 *
	 * @param bean the bean map containing message attributes
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
	 * Converts this message into a {@link BeanMap} for serialization.
	 *
	 * @return a {@code BeanMap} representation of this message
	 */
	public BeanMap toBeanMap() {
		BeanMap message = new BeanMap(getMessageEntity(), 1);
		message.put(IMSGConstants.MESSAGE_CODE, getCode());
		message.put(IMSGConstants.MESSAGE_TXTLVL1, getTextLevel1());
		String level2 = getTextLevel2();
		if ((level2 != null) && (level2.length() > 0)) {
			message.put(IMSGConstants.MESSAGE_TXTLVL2, level2);
		}
		return message;
	}

	/**
	 * Returns the name of the entity used for serialization.
	 *
	 * @return the message entity string
	 */
	public String getMessageEntity() {
		return IMSGConstants.ENTITY_MESSAGE;
	}

	/**
	 * Returns a string representation of this message using the format:
	 * {@code [code] textLevel1 : textLevel2}.
	 *
	 * @return the formatted string representation
	 */
	public String getStringRepresentation() {
		return String.format(TO_STRING_FORMAT, code, textLevel1, textLevel2);
	}
}