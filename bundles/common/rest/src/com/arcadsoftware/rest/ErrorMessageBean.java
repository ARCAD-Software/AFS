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
package com.arcadsoftware.rest;

import java.util.Date;

import com.thoughtworks.xstream.XStreamException;

/**
 * Error message sent back by a web service
 */
public class ErrorMessageBean implements Cloneable {
	
	/**
	 * Retrieve an error message from an xml fragment.
	 * 
	 * @param xml
	 * @return
	 */
	public static ErrorMessageBean deserialize(String xml) {
		try {
			return (ErrorMessageBean)new XStreamCompact().fromXML(xml);
		} catch (XStreamException e) {
			return new ErrorMessageBean(new Date(), "",xml); //$NON-NLS-1$
		}
	}

	private Date date;
	private String name;
	private String description;
	private String href;

	/**
	 * Create an error message.
	 */
	public ErrorMessageBean() {
		super();
		date = new Date();
	}
	
	/**
	 * Build an error message from partial information given by a simple description.
	 * 
	 * @param description a string of html description.
	 */
	public ErrorMessageBean(String description) {
		this();
		this.description = description;
		if (description != null) {
			int pos = description.indexOf("<title>"); //$NON-NLS-1$
			if (pos > -1) {
				name = description.substring(pos + 7, description.indexOf("</title>")); //$NON-NLS-1$
			}
		}
	}
	
	/**
	 * Build an error message with partial information.
	 * 
	 * @param date Date of the error declaration (basically Server Date).
	 * @param name The name of the error.
	 * @param description A human readable description of the error. 
	 */
	public ErrorMessageBean(Date date, String name, String description) {
		super();
		this.date = date;
		this.name = name;
		this.description = description;
	}

	/**
	 * Build an ErrorMessageException with full information.
	 * 
	 * @param date Date of the error declaration (basically Server Date).
	 * @param name The name of the error.
	 * @param description A human readable description of the error. 
	 * @param href and HTTP reference to a web page explaining the error semantic.
	 */
	public ErrorMessageBean(Date date, String name, String description, String href) {
		this(date,name,description);
		this.href = href;
	}

	/**
	 * @return The error date (server relative).
	 */
	public Date getDate() {
		return date;
	}
	
	/**
	 * @return The error name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return Human readable error description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @return the HTTP reference to the full description of this error.
	 */
	public String getHref() {
		return href;
	}
	
	/**
	 * @return an xml fragment representation of this error message.
	 */
	public String serialize() {
		return new XStreamCompact().toXML(this);
	}

	@Override
	protected Object clone() {
		return new ErrorMessageBean(date,name,description,href);
	}
	
}