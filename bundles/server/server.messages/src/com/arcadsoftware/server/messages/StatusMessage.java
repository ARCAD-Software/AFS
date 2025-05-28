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
 * Represents a status message that includes a result status (e.g., success or failure),
 * along with optional details and logging information.
 * <p>
 * A {@code StatusMessage} is an extension of {@link LogMessage} that adds a
 * status indicator and a list of detailed {@link LogMessage} objects.
 * It supports conversion to and from a {@link BeanMap}, enabling easy serialization.
 */
public class StatusMessage extends LogMessage implements IMSGConstants {

	/**
	 * Creates a failed {@code StatusMessage} from the given {@link UserMessage}.
	 *
	 * @param informationMessage the message to wrap
	 * @return a {@code StatusMessage} with failed status
	 */
	public static StatusMessage failStatusMessage(UserMessage informationMessage) {
		return new StatusMessage(informationMessage, MESSAGE_STATUS_FAILED);
	}

	/**
	 * Creates a successful {@code StatusMessage} from the given {@link UserMessage}.
	 *
	 * @param informationMessage the message to wrap
	 * @return a {@code StatusMessage} with success status
	 */
	public static StatusMessage successStatusMessage(UserMessage informationMessage) {
		return new StatusMessage(informationMessage, MESSAGE_STATUS_OK);
	}

	private int status;
	private final ArrayList<LogMessage> details = new ArrayList<LogMessage>();

	/**
	 * Constructs a {@code StatusMessage} from an existing {@link UserMessage} and status.
	 *
	 * @param m the base message
	 * @param status the status code (e.g., {@link IMSGConstants#MESSAGE_STATUS_OK})
	 */
	public StatusMessage(UserMessage m, int status) {
		this(m.getCode(), m.getTextLevel1(), m.getTextLevel2(), status);
	}

	/**
	 * Constructs a {@code StatusMessage} with code, texts, and status.
	 *
	 * @param code the message code
	 * @param textLevel1 the primary message text
	 * @param textlevel2 the secondary message text
	 * @param status the status code (e.g., success, failed, etc.)
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

	/**
	 * Returns the entity name used for serialization.
	 *
	 * @return {@code "statusmessage"}
	 */
	@Override
	public String getMessageEntity() {
		return ENTITY_STATUSMESSAGE;
	}

	/**
	 * Populates this object from a {@link BeanMap}.
	 *
	 * @param bean the bean map containing message data
	 */
	@Override
	public void fromBeanMap(BeanMap bean) {
		super.fromBeanMap(bean);
		setStatus(bean.getInt(MESSAGE_STATUS));
		details.clear();
		BeanMapList messages = bean.getBeanMapList(MESSAGE_DETAILS);
		if (messages != null) {
			for (BeanMap b : messages) {
				String code = b.getString(MESSAGE_CODE);
				String lvl1 = b.getString(MESSAGE_TXTLVL1);
				String lvl2 = b.getString(MESSAGE_TXTLVL2);
				details.add(new LogMessage(code, lvl1, lvl2, b.getInt(MESSAGE_LEVEL)));
			}
		}
	}

	/**
	 * Converts this message into a {@link BeanMap}.
	 *
	 * @return a {@code BeanMap} representation of this message
	 */
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
	 * Gets the status of this message.
	 *
	 * @return the status code
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * Sets the status of this message.
	 *
	 * @param status the new status code
	 */
	public void setStatus(int status) {
		this.status = status;
	}

	/**
	 * Adds a detailed {@link LogMessage} to this status message.
	 *
	 * @param log the detailed log message to add
	 */
	public void addDetail(LogMessage log) {
		details.add(log);
	}

	/**
	 * Updates this message's main fields from another {@link UserMessage}.
	 *
	 * @param informationMessage the source message
	 */
	public void setInformation(UserMessage informationMessage) {
		setCode(informationMessage.getCode());
		setTextLevel1(informationMessage.getTextLevel1());
		setTextLevel2(informationMessage.getTextLevel2());
	}
}