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
package com.arcadsoftware.email;

import java.io.IOException;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.mail.Address;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.util.ByteArrayDataSource;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.smime.SMIMECapabilitiesAttribute;
import org.bouncycastle.asn1.smime.SMIMECapability;
import org.bouncycastle.asn1.smime.SMIMECapabilityVector;
import org.bouncycastle.asn1.smime.SMIMEEncryptionKeyPreferenceAttribute;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.cert.jcajce.JcaCertStore;
import org.bouncycastle.cms.CMSAlgorithm;
import org.bouncycastle.cms.CMSException;
import org.bouncycastle.cms.jcajce.JcaSimpleSignerInfoGeneratorBuilder;
import org.bouncycastle.cms.jcajce.JceCMSContentEncryptorBuilder;
import org.bouncycastle.cms.jcajce.JceKeyTransRecipientInfoGenerator;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.mail.smime.SMIMEEnvelopedGenerator;
import org.bouncycastle.mail.smime.SMIMEException;
import org.bouncycastle.mail.smime.SMIMESignedGenerator;
import org.bouncycastle.operator.OperatorCreationException;

import com.arcadsoftware.email.internal.Messages;
import com.arcadsoftware.mail.Attachment;
import com.arcadsoftware.mail.FileAttachment;
import com.arcadsoftware.mail.ISendMail;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.osgi.SysOutLogged;

/**
 * Send Email messages facility class.
 */
public class SendMail implements ISendMail {

	static {
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
	}

	public static final String PROP_SMTPHOSTNAME = "mail.smtp.host"; //$NON-NLS-1$
	public static final String PROP_SMTPPORT = "mail.smtp.port"; //$NON-NLS-1$
	public static final String PROP_SMTPAUTH = "mail.smtp.auth"; //$NON-NLS-1$
	public static final String PROP_SMTPSHOSTNAME = "mail.smtps.host"; //$NON-NLS-1$
	public static final String PROP_SMTPSPORT = "mail.smtps.port"; //$NON-NLS-1$
	public static final String PROP_SMTPSAUTH = "mail.smtps.auth"; //$NON-NLS-1$
	public static final String PROP_CONNECTIONTIMEOUT = "mail.smtp.connectiontimeout"; //$NON-NLS-1$
	public static final String PROP_TIMEOUT = "mail.smtp.timeout"; //$NON-NLS-1$
	public static final String PROP_SMTPS_CONNECTIONTIMEOUT = "mail.smtps.connectiontimeout"; //$NON-NLS-1$
	public static final String PROP_SMTPS_TIMEOUT = "mail.smtps.timeout"; //$NON-NLS-1$
	public static final String PROP_STARTTLS = "mail.smtp.starttls.enable"; //$NON-NLS-1$
	public static final String PROP_SMTPS_STARTTLS = "mail.smtps.starttls.enable"; //$NON-NLS-1$
	public static final String TRANSPORT_SMTPS = "smtps"; //$NON-NLS-1$
	public static final String TRANSPORT_SMTP = "smtp"; //$NON-NLS-1$

	private static ILoggedPlugin logger = new SysOutLogged();
	
	private String transport = TRANSPORT_SMTP;
	private Properties serverprops;
	private String login;
	private String pwd;
	private String fromEmail;
	private String fromName;
	private String charset;
	private String mailerTag;
	private KeyStore keyStore;
	private String signAlias;
	private char[] signPassword;
	private String cryptAlias;
	private String decryptAlias;

	/**
	 * Use specific parameters to create a SendMail object.
	 * 
	 * @param transport the Email sending protocol (SMTP or SMTPS).
	 * @param serverprops Server connection parameters.
	 * @param login Server connection login.
	 * @param pwd Server connection password.
	 * @param fromEmail Default sender email address.
	 * @param charset Charset used for email encoding.
	 */
	public SendMail(String transport, Properties serverprops, String login, String pwd, String fromEmail, String charset) {
		this();
		this.transport = transport;
		this.serverprops = serverprops;
		this.login = login;
		this.pwd = pwd;
		this.fromEmail = fromEmail;
		this.charset = charset;
	}

	/**
	 * Create sendmail object using connected SMTP protocol.
	 * 
	 * @param hostname
	 * @param login
	 * @param pwd
	 */
	public SendMail(String hostname, String login, String pwd) {
		this(login,pwd);
		transport = TRANSPORT_SMTP;
		serverprops.put(PROP_SMTPPORT, "25"); //$NON-NLS-1$
		serverprops.put(PROP_SMTPHOSTNAME, hostname);
	}

	/**
	 * Create authenticated SendMail object.
	 * @param login
	 * @param pwd
	 */
	public SendMail(String login, String pwd) {
		this();
		serverprops.put(PROP_SMTPAUTH,"true"); //$NON-NLS-1$
		this.login = login;
		this.pwd = pwd;
	}

	/**
	 * Default constructor.
	 */
	public SendMail() {
		super();
		// Default Email parameters may be defined into system properties...
		serverprops = (Properties)System.getProperties().clone();
		// Avoid thread deadlock...
		serverprops.put(PROP_CONNECTIONTIMEOUT,"120000");  //$NON-NLS-1$
		serverprops.put(PROP_TIMEOUT,"120000"); //$NON-NLS-1$
		charset = "utf-8"; //$NON-NLS-1$
		mailerTag = "Arcad-Evolution"; //$NON-NLS-1$
	}
	
	public static void setLogger(ILoggedPlugin logger) {
		SendMail.logger = logger;
	}
	
	
	/**
	 * Set server parameters.
	 * 
	 * @param host Email server address.
	 * @param ssl Use SSL.
	 */
	public void setServerParams(String host, boolean ssl) {
		setServerParams(host, 0, true, ssl);
	}
	
	/**
	 * Set server parameters.
	 * 
	 * @param host Email server address.
	 * @param port Email server port.
	 * @param auth Use login connection.
	 * @param ssl Use SSL.
	 */
	public void setServerParams(String host, int port, boolean auth, boolean ssl) {
		if (port == 0) {
			if (ssl) {
				port = 25;
			} else {
				port = 465;
			}
		}
		if (ssl) {
			transport = TRANSPORT_SMTPS;
			serverprops.put(PROP_SMTPSAUTH,Boolean.toString(auth));
			serverprops.put(PROP_SMTPSHOSTNAME, host);
			serverprops.put(PROP_SMTPSPORT, Integer.toString(port));
		} else {
			transport = TRANSPORT_SMTP;
			serverprops.put(PROP_SMTPAUTH,Boolean.toString(auth));
			serverprops.put(PROP_SMTPHOSTNAME, host);
			serverprops.put(PROP_SMTPPORT, Integer.toString(port));
		}		
	}

	/**
	 * Set email server connection timeout (default is 2 minutes).
	 * @param timeout Time out in milli-seconds.
	 */
	public void setServerTimeout(int timeout) {
		if (timeout > 0) {
			serverprops.put(PROP_CONNECTIONTIMEOUT,Integer.toString(timeout));
			serverprops.put(PROP_TIMEOUT,Integer.toString(timeout));
		}
	}
	
	/**
	 * Send an email throw specified transfer protocol and server connection.
	 * 
	 * @param transport the transport name see TRANSPORT_SMTP and TRANSPORT_SMTPS constants.
	 * @param serverprops the server connections properties (host name, port and other specifics parameters).
	 * @param login login to connect to the server.
	 * @param pwd password to connect to the server.
	 * @param fromEmail sender email address
	 * @param fromName sender name
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @param attachments attachment files.
	 * @param charset the character set used to encode the message, null if default used.
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String transport, Properties serverprops, String login, String pwd, String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body, String charset, List<Attachment> attachments, boolean htmlMail) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, null, fromName, to, cc, bcc, subject, body, charset, attachments, htmlMail);
	}

	/**
	 * Send an Email using the default parameters.
	 * 
	 * @param fromEmail sender email address
	 * @param fromName sender name
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @param charset the character set used to encode the message, null if default used.
	 * @param attachments attachment files.
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body, String charset, List<Attachment> attachments) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, fromName, to, cc, bcc, subject, body, charset, attachments, false);
	}
	
	/**
	 * Send an Email using the default parameters.
	 * 
	 * @param fromEmail sender email address
	 * @param fromName sender name
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @param charset the character set used to encode the message, null if default used.
	 * @param attachments attachment files.
	 * @param htmlMail true if it is an html body mail 
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body, String charset, List<Attachment> attachments, boolean htmlMail) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, fromName, to, cc, bcc, subject, body, charset, attachments,htmlMail);
	}

	/**
	 * Send an Email using the default parameters.
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
	public boolean sendEmail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body, List<Attachment> attachments) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, fromName, to, cc, bcc, subject, body, charset, attachments, false);
	}

	/**
	 * Send an Email using the default parameters.
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
	public boolean sendEmail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, fromName, to, cc, bcc, subject, body, charset, null, false);
	}

	/**
	 * Send a simple email through specified transfer protocol and server connection.
	 * 
	 * @param transport the transport name see TRANSPORT_SMTP and TRANSPORT_SMTPS constants.
	 * @param serverprops the server connections properties (host name, port and other specifics parameters).
	 * @param login login to connect to the server.
	 * @param pwd password to connect to the server.
	 * @param charset the character set used to encode the message, null if default used.
	 * @param fromEmail sender email address
	 * @param to recipients addresses  
	 * @param subject email subject
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String transport, Properties serverprops, String login, String pwd, String charset, String fromEmail, String to, String subject, String body) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, null, to, null, null, subject, body, charset, null, false);
	}

	/**
	 * Send an Email using the default parameters.
	 * 
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @param charset the character set used to encode the message, null if default used.
	 * @param attachments attachment files.
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String to, String cc, String bcc, String subject, String body, String charset, List<Attachment> attachments) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, null, to, cc, bcc, subject, body, charset, attachments, false);
	}

	/**
	 * Send a simple Email using the default parameters.
	 * 
	 * @param to recipients addresses  
	 * @param subject email subject
	 * @param body email body text
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String to, String subject, String body) {
		return sendEmail(transport, serverprops, login, pwd, fromEmail, null, to, null, null, subject, body, charset, null, false);
	}
	
	/**
	 * Send an email through specified transfer protocol and server connection.
	 * 
	 * @param transport the transport name see TRANSPORT_SMTP and TRANSPORT_SMTPS constants.
	 * @param serverprops the server connections properties (host name, port and other specifics parameters).
	 * @param login login to connect to the server.
	 * @param pwd password to connect to the server.
	 * @param fromEmail sender email address
	 * @param replyToEmail sender email to reply to.
	 * @param fromName sender name
	 * @param to recipients addresses  
	 * @param cc recipients addresses
	 * @param bcc recipients addresses
	 * @param subject email subject
	 * @param body email body text
	 * @param attachments attachment files.
	 * @param charset the character set used to encode the message, null if default used.
	 * @return true if the email has been sent.
	 */
	public boolean sendEmail(String transport, Properties serverprops, String login, String pwd, String fromEmail, String replyToEmail, String fromName, String to, String cc, String bcc, String subject, String body, String charset, List<Attachment> attachments, boolean htmlMail) {
		try {
			if ((charset == null) || (charset.length() == 0)) {
				charset = this.charset;
			}
			// Propagate SMTP options to SMTPS Transport implementation...
			if (serverprops.get(PROP_CONNECTIONTIMEOUT) != null) {
				serverprops.put(PROP_SMTPS_CONNECTIONTIMEOUT, serverprops.get(PROP_CONNECTIONTIMEOUT));
			}
			if (serverprops.get(PROP_TIMEOUT) != null) {
				serverprops.put(PROP_SMTPS_TIMEOUT, serverprops.get(PROP_TIMEOUT));
			}
			if (serverprops.get(PROP_STARTTLS) != null) {
				serverprops.put(PROP_SMTPS_STARTTLS, serverprops.get(PROP_STARTTLS));
			}
			// Force the verification of the SMTP server host name.
			if (TRANSPORT_SMTPS.equals(transport)) {
				serverprops.put("mail.smtps.ssl.checkserveridentity", "true"); //$NON-NLS-1$ //$NON-NLS-1$
				// FIXME set mail.smtps.ssl.socketFactory with a locally configured socket factory !
				// Always trust the server hostname...
				Object host = serverprops.get(PROP_SMTPSHOSTNAME);
				if (host != null) {
					if (!serverprops.contains("mail.smtps.ssl.trust")) { //$NON-NLS-1$
						serverprops.put("mail.smtps.ssl.trust", host); //$NON-NLS-1$
					}
				}
			} else {
				// FIXME only if starttls is set to true...
				serverprops.put("mail.smtp.ssl.checkserveridentity", "true"); //$NON-NLS-1$ //$NON-NLS-1$
				// FIXME set mail.smtp.ssl.socketFactory with a locally configured socket factory !
				Object host = serverprops.get(PROP_SMTPHOSTNAME);
				if (host != null) {
					if (!serverprops.contains("mail.smtp.ssl.trust")) { //$NON-NLS-1$
						serverprops.put("mail.smtp.ssl.trust", host); //$NON-NLS-1$
					}
				}
			}
			// Attaching to default Session, or we could start a new one
			Session session = Session.getInstance(serverprops);
			// Create a new message
			MimeMessage message = new MimeMessage(session);
			// Set the FROM field
			if ((fromEmail != null) && (fromEmail.length() > 0) && (fromName != null) && (fromName.length() > 0)) {
				message.setFrom(new InternetAddress(fromEmail,fromName));
			} else if ((fromEmail != null) && (fromEmail.length() > 0)) {
				message.setFrom(new InternetAddress(fromEmail));
			} else if ((this.fromEmail != null) && (this.fromEmail.length() > 0)) {
				if ((this.fromName != null) && (this.fromName.length() > 0)) {
					message.setFrom(new InternetAddress(this.fromEmail,this.fromName));
				} else {
					message.setFrom(new InternetAddress(this.fromEmail));
				}
			} else {
				logger.debug(Messages.SendMail_Error_Expeditors + subject);
				return false;
			}
			// Set the ReplyTo field
			if ((replyToEmail != null) && (replyToEmail.length() > 0)) {
				if ((fromName != null) && (fromName.length() > 0)) {
					message.setReplyTo(new InternetAddress[]{new InternetAddress(replyToEmail,fromName)});
				} else {
					message.setReplyTo(new InternetAddress[]{new InternetAddress(replyToEmail)});
				}
			}
			// Set the recipient list
			InternetAddress[] tos = new InternetAddress[0];
			InternetAddress[] ccs = new InternetAddress[0];
			InternetAddress[] bccs = new InternetAddress[0];
			int capacity = 1;
			if ((to != null) && (to.length() > 0)) {
				tos = InternetAddress.parse(to, false);
				if (tos != null) {
					capacity += tos.length;
				}
			}
			if ((cc != null) && (cc.length() > 0)) {
				ccs = InternetAddress.parse(cc, false);
				if (ccs != null) {
					capacity += ccs.length;
				}
			}
			if ((bcc != null) && (bcc.length() > 0)) {
				bccs = InternetAddress.parse(bcc, false);
				if (bccs != null) {
					capacity += bccs.length;
				}
			}
			// Remove duplicated recipients.
			ArrayList<InternetAddress> r = new ArrayList<InternetAddress>(capacity);
			if (tos != null) {
				tos = clearedRecipientList(tos,r);
			}
			if (ccs != null) {
				ccs = clearedRecipientList(ccs,r);
			}
			if (bccs != null) {
				bccs = clearedRecipientList(bccs,r);
			}
			// Add final recipients to the message...
			boolean norecipients = true;
			if ((tos != null) && (tos.length > 0)) {
				message.setRecipients(MimeMessage.RecipientType.TO, tos);
				norecipients = false;
			}
			if ((ccs != null) && (ccs.length > 0)) {
				message.setRecipients(MimeMessage.RecipientType.CC ,ccs);
				norecipients = false;
			}
			if ((bccs != null) && (bccs.length > 0)) {
				message.setRecipients(MimeMessage.RecipientType.BCC ,bccs);
				norecipients = false;
			}
			if (norecipients) {
				logger.debug(Messages.SendMail_Error_NoRecipients + subject);
				return false;
			}
			// Set Email Subject.
			message.setSubject(subject,charset);
			// 
			message.setHeader("X-Mailer", mailerTag); //$NON-NLS-1$
			// Empèche les renvois de message de type "On Vacation"
			message.setHeader("Auto-Submitted", "auto-generated"); //$NON-NLS-1$ //$NON-NLS-2$
			message.setHeader("Precedence", "list"); //$NON-NLS-1$ //$NON-NLS-2$
			message.setSentDate(new Date());
			if ((keyStore == null) && ((attachments == null) || (attachments.size() == 0))) {
				if (htmlMail) {
					message.setContent(body, "text/html"); //$NON-NLS-1$
				} else {
					message.setText(body, charset);
				}
			} else {
				MimeBodyPart messageBodyPart = new MimeBodyPart();
				if (htmlMail) {
					messageBodyPart.setContent(body, "text/html"); //$NON-NLS-1$
				} else {
					messageBodyPart.setText(body, charset);
				}
				if (keyStore == null) {
					MimeMultipart multipart = new MimeMultipart();
					multipart.addBodyPart(messageBodyPart);
					appendAttachments(multipart, attachments);
					message.setContent(multipart, multipart.getContentType());
				} else {
					messageBodyPart = generateSecuredMessage(messageBodyPart, attachments);
					message.setContent(messageBodyPart, messageBodyPart.getContentType());
				}
			}
			message.saveChanges();
			// Send the message...
			String host;
			int port;
			if (TRANSPORT_SMTPS.equals(transport)) {
				host = serverprops.getProperty(PROP_SMTPSHOSTNAME);
				try {
					port = Integer.parseInt(serverprops.getProperty(PROP_SMTPSPORT));
				} catch (NumberFormatException e) {
					port = 465;
				}
			} else { // Assume SMTP Protocol...
				host = serverprops.getProperty(PROP_SMTPHOSTNAME);
				try {
					port = Integer.parseInt(serverprops.getProperty(PROP_SMTPPORT));
				} catch (NumberFormatException e) {
					port = 25;
				}
			}
			if (host == null) {
				logger.warn(Messages.SendMail_Error_NoSMTPServer + subject, null);
			}
			if ((login != null) && (login.length() == 0)) {
				login = null;
				pwd = null;
			} else if ((pwd == null) && (login != null)) {
				pwd = ""; //$NON-NLS-1$
			}
			Transport tr = session.getTransport(transport);
			try {
				Address[] addresses = message.getAllRecipients();
				if ((addresses != null) && (addresses.length > 0)) {
					// Always use the parameterized version of connect even some parameters are null.
					tr.connect(host, port, login, pwd);
					tr.sendMessage(message, addresses);
					return true;
				}
				return false;
			} finally {
				tr.close();
			}
		} catch (Throwable e) {
			logger.error(Messages.SendMail_Error_emailprocessing, e);
			return false;
		}
	}

	private void appendAttachments(MimeMultipart multipart, List<Attachment> attachments)
			throws MessagingException, IOException {
		MimeBodyPart messageBodyPart;
		for(Attachment a: attachments) {
			messageBodyPart = new MimeBodyPart();
			if (a instanceof FileAttachment) {
				messageBodyPart.setDataHandler(new DataHandler(new FileDataSource(((FileAttachment)a).getFile())));
			} else if (a instanceof DataSource) {
				messageBodyPart.setDataHandler(new DataHandler((DataSource)a));
			} else {
				messageBodyPart.setDataHandler(new DataHandler(new ByteArrayDataSource(a.getContentStream(),a.getMimeType())));
			}
			messageBodyPart.setFileName(a.getFileName());
			multipart.addBodyPart(messageBodyPart);
		}
	}

	private MimeBodyPart generateSecuredMessage(MimeBodyPart messageBodyPart,
			List<Attachment> attachments) throws MessagingException, IOException {
		ArrayList<Certificate> certList = new ArrayList<Certificate>();
		MimeBodyPart msg = new MimeBodyPart();
        MimeMultipart mp = new MimeMultipart();
        mp.addBodyPart(messageBodyPart);
        appendAttachments(mp, attachments);
        // be careful about setting extra headers here. Some mail clients
        // ignore the To and From fields (for example) in the body part
        // that contains the multipart. The result of this will be that the
        // signature fails to verify... Outlook Express is an example of
        // a client that exhibits this behaviour.
        msg.setContent(mp);
        try {
			if (keyStore.containsAlias(signAlias)) {
				if (!Collections.addAll(certList, keyStore.getCertificateChain(signAlias))) {
					logger.error("Unable to Sign the EMAIL Message, alias not found: " + signAlias, null);
					return msg;
				}
				Key sk = keyStore.getKey(signAlias, signPassword);
		        // create a CertStore containing the certificates we want carried
		        // in the signature
				JcaCertStore certs = new JcaCertStore(certList);
		        // create some smime capabilities in case someone wants to respond
		        SMIMECapabilityVector caps = new SMIMECapabilityVector();
		        caps.addCapability(SMIMECapability.dES_EDE3_CBC);
		        caps.addCapability(SMIMECapability.rC2_CBC, 128);
		        caps.addCapability(SMIMECapability.dES_CBC);
		        ASN1EncodableVector signedAttrs = new ASN1EncodableVector();
		        signedAttrs.add(new SMIMECapabilitiesAttribute(caps));
		        // add an encryption key preference for encrypted responses -
		        // normally this would be different from the signing certificate...
		        if ((decryptAlias != null) && keyStore.containsAlias(decryptAlias)) {
		        	Certificate dc = keyStore.getCertificate(decryptAlias);
		        	if (dc instanceof X509Certificate) {
				        IssuerAndSerialNumber issAndSer = new IssuerAndSerialNumber(
				        		new X500Name(((X509Certificate) dc).getIssuerX500Principal().getName()),
				        		((X509Certificate) dc).getSerialNumber());
				        signedAttrs.add(new SMIMEEncryptionKeyPreferenceAttribute(issAndSer));
		        	}
		        }
		        // create the generator for creating an smime/signed message
		        SMIMESignedGenerator signer = new SMIMESignedGenerator();
		        // add a signer to the generator - this specifies we are using SHA1 and
		        // adding the smime attributes above to the signed attributes that
		        // will be generated as part of the signature. The encryption algorithm
		        // used is taken from the key - in this RSA with PKCS1Padding
		        signer.addSignerInfoGenerator(new JcaSimpleSignerInfoGeneratorBuilder() //
		        		.setProvider(BouncyCastleProvider.PROVIDER_NAME) //
		        		.setSignedAttributeGenerator(new AttributeTable(signedAttrs)) //
		        		.build("SHA1withRSA", (PrivateKey) sk, (X509Certificate) keyStore.getCertificate(signAlias))); //$NON-NLS-1$
		        // add our pool of certs and cerls (if any) to go with the signature
		        signer.addCertificates(certs);
		        mp = signer.generate(msg); //$NON-NLS-1$
		        msg = new MimeBodyPart();
		        msg.setContent(mp);
			}        
			if (keyStore.containsAlias(signAlias)) {
		        Certificate[]   chain = keyStore.getCertificateChain(cryptAlias);
		        // create the generator for creating an smime/encrypted message
		        SMIMEEnvelopedGenerator  gen = new SMIMEEnvelopedGenerator();
		        gen.addRecipientInfoGenerator(new JceKeyTransRecipientInfoGenerator((X509Certificate) chain[0]).setProvider(BouncyCastleProvider.PROVIDER_NAME));
		        // create a subject key id - this has to be done the same way as
		        // it is done in the certificate associated with the private key
		        // version 3 only.
		        /*
		        MessageDigest dig = MessageDigest.getInstance("SHA1", BouncyCastleProvider.PROVIDER_NAME);
		        dig.update(cert.getPublicKey().getEncoded());
		        gen.addKeyTransRecipient(cert.getPublicKey(), dig.digest());
		        */
		        // create the base for our message
		        msg = gen.generate(msg, new JceCMSContentEncryptorBuilder(CMSAlgorithm.RC2_CBC).setProvider(BouncyCastleProvider.PROVIDER_NAME).build());
			}
        } catch (KeyStoreException|
        		SMIMEException|
        		CMSException|
        		CertificateEncodingException|
        		OperatorCreationException|
        		UnrecoverableKeyException|
        		NoSuchAlgorithmException e) {
        	logger.error(e.getLocalizedMessage(), e);
		}
		return msg;
	}

	private InternetAddress[] clearedRecipientList(InternetAddress[] addresses, ArrayList<InternetAddress> alreadyUsed) {
		ArrayList<InternetAddress> cleared = new ArrayList<InternetAddress>(addresses.length);
		for(InternetAddress adr:addresses) {
			boolean nused = true;
			String adrs = adr.getAddress();
			for (InternetAddress usedadr:alreadyUsed) {
				if (usedadr.getAddress().equalsIgnoreCase(adrs)) {
					nused = false;
					break;
				}
			}
			if (nused) {
				cleared.add(adr);
				alreadyUsed.add(adr);
			}
		}
		return cleared.toArray(new InternetAddress[cleared.size()]);
	}

	/**
	 * @return Email sending protocol (SMTP or SMTPS).
	 */
	public String getTransport() {
		return transport;
	}

	/**
	 * Email sending protocol (SMTP or SMTPS).
	 * @param transport
	 */
	public void setTransport(String transport) {
		this.transport = transport;
	}

	/**
	 * @return Email server connection login.
	 */
	public String getLogin() {
		return login;
	}

	/**
	 * Email server connection login.
	 * @param login
	 */
	public void setLogin(String login) {
		this.login = login;
	}

	/**
	 * @return Email server connection password.
	 */
	public String getPwd() {
		return pwd;
	}

	/**
	 * Email server connection password.
	 * @param pwd
	 */
	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	/**
	 * @return Default sender email address.
	 */
	public String getFromEmail() {
		return fromEmail;
	}

	/**
	 * Default sender email address.
	 * @param fromEmail
	 */
	public void setFromEmail(String fromEmail) {
		this.fromEmail = fromEmail;
	}

	/**
	 * @return Characters Set used to encode email messages (default is "utf-8").
	 */
	public String getCharset() {
		return charset;
	}

	/**
	 * Characters Set used to encode email messages (default is "utf-8").
	 * @param charset
	 */
	public void setCharset(String charset) {
		this.charset = charset;
	}

	/**
	 * @return Server identification parameters.
	 */
	public Properties getServerprops() {
		return serverprops;
	}

	/**
	 * Option sender name that decorate the sender email address.
	 * @param fromName Default sender name.
	 */
	public void setFromName(String fromName) {
		this.fromName = fromName;
	}

	/**
	 *  Default sender name.
	 * @return
	 */
	public String getFromName() {
		return fromName;
	}

	/**
	 * The mailer program name that it is included into email headers.
	 * @param mailerTag 
	 */
	public void setMailerTag(String mailerTag) {
		this.mailerTag = mailerTag;
	}

	/**
	 * @return The mailer program name that it is included into email headers.
	 */
	public String getMailerTag() {
		return mailerTag;
	}

	/**
	 * The KeyStore used to store all required certificates.
	 * 
	 * @param keyStore
	 */
	public void setKeyStore(KeyStore keyStore) {
		this.keyStore = keyStore;
	}

	/**
	 * KeyPair used to sign Message.
	 * @param signAlias
	 */
	public void setSignAlias(String signAlias) {
		this.signAlias = signAlias;
	}

	public void setSignPassword(char[] signPassword) {
		this.signPassword = signPassword;
	}

	/**
	 * KeyPair used to encrypt Message.
	 * @param cryptAlias
	 */
	public void setCryptAlias(String cryptAlias) {
		this.cryptAlias = cryptAlias;
	}

	/**
	 * Certificate used to decrypt message, added as a preference for responses.
	 * @param decryptAlias
	 */
	public void setDecryptAlias(String decryptAlias) {
		this.decryptAlias = decryptAlias;
	}

	/**
	 * 
	 * @param keyStore must contain the aliases used for sign and crypt operations.
	 * @param signAlias may be null. KeyPair used to sign Message.
	 * @param cryptAlias may be null. KeyPair used to encrypt Message.
	 * @param decryptAlias may be null. Certificate used to decrypt message, added as a preference for responses.
	 */
	public void setSecurityParams(KeyStore keyStore, String signAlias, char[] signPassword, String cryptAlias, String decryptAlias) {
		this.keyStore = keyStore;
		this.signAlias = signAlias;
		this.signPassword = signPassword;
		this.cryptAlias = cryptAlias;
		this.decryptAlias = decryptAlias;
	}
	
}
