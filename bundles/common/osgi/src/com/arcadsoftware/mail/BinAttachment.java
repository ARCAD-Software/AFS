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
/*
 * Créé le 21/07/2006
 *
 */
package com.arcadsoftware.mail;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

/**
 * Gestion des pièces jointe du message.
 * 
 * @author ARCAD Software
 */
public class BinAttachment implements Attachment {

	private byte[] content = new byte[0];
	private String filename = ""; //$NON-NLS-1$
	private String name = "No_Name_Attachment"; //$NON-NLS-1$
	private String mimeType;
	private String contentId;

	/**
	 * @param aMimeType
	 */
	public BinAttachment(String aMimeType) {
		super();
		mimeType = aMimeType;
	}

	/**
	 * @param aName
	 * @param aMimeType
	 */
	public BinAttachment(String aName, String aMimeType) {
		this(aMimeType);
		if ((aName != null) && (aName.length() > 0)) {
			name = aName;
		}
		filename = aName;
	}

	/**
	 * @param aName
	 * @param aMimeType
	 * @param aContent
	 */
	public BinAttachment(String aName, String aMimeType, byte[] aContent) {
		this(aName, aMimeType);
		content = aContent;
	}

	/**
	 * 
	 * @param contentId
	 * @param aName
	 * @param aMimeType
	 * @param aContent
	 */
	public BinAttachment(String contentId, String aName, String aMimeType, byte[] aContent) {
		this(aName, aMimeType);
		content = aContent;
		this.contentId = contentId; 
	}

	/**
	 * @return
	 */
	public byte[] getContent() {
		return content;
	}

	/**
	 * @param bs
	 */
	public void setContent(byte[] bs) {
		content = bs;
	}

	/**
	 * @return
	 */
	public String getFileName() {
		return filename;
	}

	/**
	 * @param string
	 */
	public void setFileName(String string) {
		filename = string;
	}
	/**
	 * @return
	 */
	public String getMimeType() {
		return mimeType;
	}

	/**
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return
	 */
	public long getSize() {
		return content.length;
	}

	/**
	 * @return
	 */
	public InputStream getContentStream() {
		return new ByteArrayInputStream(content);
	}

	/**
	 * @param contentId the contentId to set
	 */
	public void setContentId(String contentId) {
		this.contentId = contentId;
	}

	/**
	 * @return the contentId
	 */
	public String getContentId() {
		return contentId;
	}

}