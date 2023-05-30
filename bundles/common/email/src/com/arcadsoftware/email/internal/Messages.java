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
package com.arcadsoftware.email.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	
	private static final String BUNDLE_NAME = "com.arcadsoftware.email.internal.messages"; //$NON-NLS-1$
	
	public static String Activator_Error_fire_event;
	public static String Activator_Info_mailer_starting;
	public static String EmailProcessTask_Authentification_required;
	public static String EmailProcessTask_debug_Log_process;
	public static String EmailProcessTask_Debug_Date;

	public static String EmailProcessTask_Debug_ProcessAborted;

	public static String EmailProcessTask_Debug_treatment_too_long;
	public static String EmailProcessTask_Error_authentification;
	public static String EmailProcessTask_Error_bad_configuration;
	public static String EmailProcessTask_Error_broken_connection;
	public static String EmailProcessTask_Error_database_access;
	public static String EmailProcessTask_Error_dead_lock;
	public static String EmailProcessTask_Error_folder_expunge;
	public static String EmailProcessTask_Error_folder_not_exist;
	public static String EmailProcessTask_Error_Folder_Not_Exist;
	public static String EmailProcessTask_Error_generic;
	public static String EmailProcessTask_Error_invalid_protocol;
	public static String EmailProcessTask_Error_Invalid_Protocol;
	public static String EmailProcessTask_Error_Mailbox_management;
	public static String EmailProcessTask_Error_no_messages_into_database;
	public static String EmailProcessTask_Error_null_session;
	public static String EmailProcessTask_Error_process;
	public static String EmailProcessTask_Error_text;
	public static String EmailProcessTask_Login;
	public static String EmailProcessTask_Message_Empty_or_Blacklisted;
	public static String EmailProcessTask_Message_Filter_recipient_CC_or_TO;
	public static String EmailProcessTask_Message_Filter_recipient_TO;
	public static String EmailProcessTask_Message_Filter_recipient_TO_unique;
	public static String EmailProcessTask_NotIgnoredFoundPattern;
	public static String HTMLParser_MissingReturnStatment;
	public static String HTMLParserTokenManager_InternalErrorInvalidLexicalState;
	public static String HTMLParserTokenManager_InternalErrorUseStaticCharStream;
	public static String HTMLParserTokenManager_StateUnchanged;
	public static String MailMessage_EmptySubjectText;
	public static String MailMessage_ErrorAttachmentMimeType;
	public static String MailMessage_ErrorCantReadOptionalRecipient;
	public static String MailMessage_ErrorCantReadRecipent;
	public static String MailMessage_ErrorDecoding;
	public static String MailMessage_ErrorDecodingAttachment;
	public static String MailMessage_ErrorDecodingContent;
	public static String MailMessage_ErrorEmptySubject;
	public static String MailMessage_ErrorHTMLConversion;
	public static String MailMessage_ErrorIOExceptionAMP;
	public static String MailMessage_ErrorIOExceptionHMP;
	public static String MailMessage_ErrorIOExceptionMPP;
	public static String MailMessage_ErrorIOExceptionMSOAMP;
	public static String MailMessage_ErrorIOExceptionMSOMP;
	public static String MailMessage_ErrorIOExceptionMSORTFMP;
	public static String MailMessage_ErrorIOExceptionNMP;
	public static String MailMessage_ErrorIOExceptionTMP;
	public static String MailMessage_ErrorMessageEmpty;
	public static String MailMessage_ErrorMessageTruncated;
	public static String MailMessage_ErrorMExceptionAMP;
	public static String MailMessage_ErrorMExceptionHMP;
	public static String MailMessage_ErrorMExceptionMPP;
	public static String MailMessage_ErrorMExceptionMSOMP;
	public static String MailMessage_ErrorMExceptionMT;
	public static String MailMessage_ErrorMExceptionNMP;
	public static String MailMessage_ErrorMExceptionTMP;
	public static String MailMessage_ErrorSenderEmpty;
	public static String MailMessage_ErrorSenderTooLong;
	public static String MailMessage_ErrorSenderUnknown;
	public static String MailMessage_ErrorSenderUseSystemAddress;
	public static String MailMessage_ErrorSpamMessage;
	public static String MailMessage_ErrorSubjectTooLong;
	public static String MailMessage_LogErrorDuringProcess;
	public static String MailMessage_MessageTruncated;
	public static String MAPIProps_Unknown;
	public static String MAPIValue_Unknown;
	public static String Message_Invalid;
	public static String ParseException_AtLine;
	public static String ParseException_Column;
	public static String ParseException_Encountered;
	public static String ParseException_WasExpected;
	public static String ParseException_WasExpecting;
	public static String ParserThread_Aborted;
	public static String RawInputStream_CantGetBytes;
	public static String RawInputStream_CantSkip;
	public static String RawInputStream_InStream;
	public static String RawInputStream_UnexpectedEnd;
	public static String SendMail_Error_emailprocessing;

	public static String SendMail_Error_Expeditors;

	public static String SendMail_Error_NoRecipients;

	public static String SendMail_Error_NoSMTPServer;
	public static String TNEFInputStream_InvalidCheckSum;
	public static String TNEFInputStream_InvalidLevel;
	public static String TNEFInputStream_InvalidSignature;
	public static String TNEFInputStream_NotValid;
	public static String TNEFInputStream_UnexpectedEnd;
	public static String TNEFUtils_DataSizeMismatch;
	public static String TNEFUtils_Failed;
	public static String TNEFUtils_InvalidCompressedRTFHeader;
	public static String TNEFUtils_UnknownCompressionType;
	public static String TokenMgrError_LexicalError;
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
