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
package com.arcadsoftware.mail;

import java.io.InputStream;

/**
 * Generic interface for Emails Attachments. 
 * 
 * <p>
 * Theses objects are used into Email receiving and sending.
 */
public interface Attachment {
	
	/**
	 * @return The attachment file name.
	 */
	public String getFileName();
	
	/**
	 * @return The attachment Name (can be diferent from the file name.
	 */
	public String getName();
	
	/**
	 * @return Attachment object content.
	 */
	public InputStream getContentStream();
	
	/**
	 * @return Attachment object content.
	 */
	public byte[] getContent();
	
	/**
	 * @return Attachment object MIME type
	 */
	public String getMimeType();
	
	/**
	 * @return Attachment object content size
	 */
	public long getSize();
}