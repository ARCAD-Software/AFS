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
package com.arcadsoftware.mail;

import java.security.KeyStore;
import java.util.List;
import java.util.Properties;

/**
 * This OSGi service use the local configuration (or the given one) to send an email...
 */
public interface ISendMail {

	public static final String clazz = ISendMail.class.getName();
	
	/**
	 * Send an email.
	 * 
	 * @param fromEmail sender email address
	 * @param fromName sender name
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @param attachments attachment files.
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body, List<Attachment> attachments);
	
	/**
	 * Send an email without using current configuration.
	 * 
	 * @param transport smtp transport
	 * @param serverprops server system properties...
	 * @param login null if not required
	 * @param pwd
	 * @param charset used charset
	 * @param fromEmail 
	 * @param to
	 * @param subject
	 * @param body
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String transport, Properties serverprops, String login, String pwd, String charset, String fromEmail, String to, String subject, String body);
	
	/**
	 * Send a simple email.
	 * 
	 * @param to recipients addresses  
	 * @param subject email subject
	 * @param body email body text
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String to, String subject, String body);
	
	/**
	 * Send an email.
	 * 
	 * @param fromEmail sender email address
	 * @param fromName sender name
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body);
	
	/**
	 * Set Encryption and Email content signing parameters.
	 *  
	 * @param keyStore
	 * @param signAlias
	 * @param signPassword
	 * @param cryptAlias
	 * @param decryptAlias
	 */
	public void setSecurityParams(KeyStore keyStore, String signAlias, char[] signPassword, String cryptAlias, String decryptAlias);

}
