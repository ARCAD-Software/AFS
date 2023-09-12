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
import java.util.Properties;
import org.junit.jupiter.api.Test;
import com.arcadsoftware.email.SendMail;

/**
 * This class is not really a Unit test but can be used to test SMTPS and Start TLS configuration in the mail sender
 * Configure the needed data below and DO NOT COMMIT any passwords or confidential information
 * Test done and validated on sept 2023 using GMX mail provider for Start TLS and SMTPS
 * 
 * GMX SMTPS Configuration server
 * Server : mail.gmx.com
 * Port : 465
 * 
 * GMX Start TLS Configuration server
 * Server : mail.gmx.com
 * Port : 587
 * 
 * Certificates can be retrieved using KeyStore Explorer option for explore TLS connection certificates
 * 
 * @author ARCAD Software
 */

class TestSendMail {

	// SMTPS Configuration
	public static final String PROP_SMTPSHOSTNAME = "mail.smtps.host"; //$NON-NLS-1$
	public static final String PROP_SMTPSPORT = "mail.smtps.port"; //$NON-NLS-1$
	public static final String PROP_SMTPSAUTH = "mail.smtps.auth"; //$NON-NLS-1$
	public static final String PROP_SMTPS_CONNECTIONTIMEOUT = "mail.smtps.connectiontimeout"; //$NON-NLS-1$
	public static final String PROP_SMTPS_TIMEOUT = "mail.smtps.timeout"; //$NON-NLS-1$
	
	// SMTP Configuration
	public static final String PROP_SMTPHOSTNAME = "mail.smtp.host"; //$NON-NLS-1$
	public static final String PROP_SMTPPORT = "mail.smtp.port"; //$NON-NLS-1$
	public static final String PROP_SMTPAUTH = "mail.smtp.auth"; //$NON-NLS-1$
	public static final String PROP_SMTP_CONNECTIONTIMEOUT = "mail.smtp.connectiontimeout"; //$NON-NLS-1$
	public static final String PROP_SMTP_TIMEOUT = "mail.smtp.timeout"; //$NON-NLS-1$
	public static final String PROP_SMTP_START_TLS = "mail.smtp.starttls.enable"; //$NON-NLS-1$
	
	// TLS Configuration
	public static final String TRUSTSTORE_PATH = "ssl.truststore.path";
	public static final String TRUSTSTORE_PWD = "ssl.truststore.pwd";
	public static final String TRUSTSTORE_TYPE = "ssl.truststore.type";
	
	private SendMail sendMail;
	
	//@Test
	public void sendEmailWithSMTPS() {
		String transport = SendMail.TRANSPORT_SMTPS;
		Properties serverProps = new Properties();
		// SMTPS Configuration
		serverProps.put(PROP_SMTPSHOSTNAME, "TO FILL");
		serverProps.put(PROP_SMTPSPORT, "465");
		serverProps.put(PROP_SMTPSAUTH, "true");
		serverProps.put(PROP_SMTPS_CONNECTIONTIMEOUT, "120000");
		serverProps.put(PROP_SMTPS_TIMEOUT, "120000");
		
		// Option to force fallback to true, check sendMail for more details
		//serverProps.put("mail.smtps.socketFactory.fallback", "true");
		
		// TLS Configuration like in cfg file
		// /!\ CONFIGURE TRUSTSTORE FOR TEST
		serverProps.put(TRUSTSTORE_PATH, "TO FILL");
		serverProps.put(TRUSTSTORE_PWD, "TO FILL");
		serverProps.put(TRUSTSTORE_TYPE, "jks");
		
		// /!\ CONFIGURE EMAIL SENDER FOR TEST
		String login = "TO FILL";
		String password = "TO FILL";
		String fromEmail = "TO FILL";
		String charset = "utf-8";
		
		sendMail = new SendMail(transport, serverProps, login, password, fromEmail, charset);
		sendMail.sendEmail("TO FILL", "AFS - TEST - SMTPS", "This is a test");

	}
	
	@Test
	public void sendEmailWithStartTLS() {
		String transport = SendMail.TRANSPORT_SMTP;
		Properties serverProps = new Properties();
		// SMTPS Configuration
		serverProps.put(PROP_SMTPHOSTNAME, "TO FILL");
		serverProps.put(PROP_SMTPPORT, "587");
		serverProps.put(PROP_SMTPAUTH, "true");
		serverProps.put(PROP_SMTP_CONNECTIONTIMEOUT, "120000");
		serverProps.put(PROP_SMTP_TIMEOUT, "120000");
		serverProps.put(PROP_SMTP_START_TLS, "true");
		
		// TLS Configuration like in cfg file
		// /!\ CONFIGURE TRUSTSTORE FOR TEST
		serverProps.put(TRUSTSTORE_PATH, "TO FILL");
		serverProps.put(TRUSTSTORE_PWD, "TO FILL");
		serverProps.put(TRUSTSTORE_TYPE, "jks");
		
		// /!\ CONFIGURE EMAIL SENDER FOR "TO FILL"TEST
		String login = "TO FILL";
		String password = "TO FILL";
		String fromEmail = "TO FILL";
		String charset = "utf-8";
		
		sendMail = new SendMail(transport, serverProps, login, password, fromEmail, charset);
		sendMail.sendEmail("TO FILL", "AFS - TEST - START TLS", "This is a test");

	}
}
