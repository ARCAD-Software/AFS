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
package com.arcadsoftware.email;

import java.io.File;
import java.io.FileInputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.Properties;
import java.util.Timer;

import com.arcadsoftware.email.internal.Activator;
import com.arcadsoftware.email.internal.Messages;
import com.arcadsoftware.mail.EmailProcessException;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.osgi.SysOutLogged;

import java.util.TimerTask;

import jakarta.mail.AuthenticationFailedException;
import jakarta.mail.Flags;
import jakarta.mail.Folder;
import jakarta.mail.FolderNotFoundException;
import jakarta.mail.Message;
import jakarta.mail.MessagingException;
import jakarta.mail.NoSuchProviderException;
import jakarta.mail.Part;
import jakarta.mail.Session;
import jakarta.mail.Store;
import jakarta.mail.internet.MimeMessage;

import org.bouncycastle.cms.RecipientId;
import org.bouncycastle.cms.RecipientInformation;
import org.bouncycastle.cms.RecipientInformationStore;
import org.bouncycastle.cms.jcajce.JceKeyTransEnvelopedRecipient;
import org.bouncycastle.cms.jcajce.JceKeyTransRecipientId;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.mail.smime.SMIMEEnveloped;
import org.bouncycastle.mail.smime.SMIMEUtil;

/**
 * This class implement a email Robot process that periodically check and email box and load new messages.
 * 
 * <p>
 * To use this class override this class and implement the method <code>storeMessage</code>. This
 * method get a valid email message and must save it, or process it.
 * 
 * <p>
 * This class offer 3 use case :
 * <nl>
 * <li>To run the process once, just call <code>run</code>
 * <li>This class implement the TimerTask class and can be Scheduler with a <code>Timer</code>. Helper
 * static method <code>schedule</code> create such a Timer.
 * <li>Just call methods <code>start()</code> and <code>stop()</code> to periodically run the process, according to the <code>timing</code> property.
 * 
 * @see TimerTask
 */
public abstract class IncomingEmailProcessTask extends TimerTask {
	
	static {
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
	}

	private static ILoggedPlugin logger = new SysOutLogged();

	/**
	 *  This factor is applied to the periodic delay to determine the first run.
	 */
	public static final int STARTING_DELAY_FACTOR = 2;	

	/**
	 *  This value is the number of iterations to wait until to retry after a fatal error. 
	 */
	public static final int DEFAULT_BROKECOUNT = 10;

	private int timing;
	private int runCount = 0;
	private String server;
	private int port;
	private String type;
	private int brokecount = 0;
	private String folderName;
	private String password;
	private String login;
	private boolean mimeStrict;
	private Timer timer;
	private int timeout = 12000;
	private RecipientId recId;

	private PrivateKey privateKey;
	
	/**
	 * Schedule a Incoming Email process
	 * 
	 * <p>
	 * To stop this process cancel the returned Timer. 
	 * 
	 * @param timing period of execution.
	 * @param task The process to run.
	 * @return a Timer used to be able to stop the process.
	 */
	public static final Timer shedule(int timing, IncomingEmailProcessTask task) {
		timing = timing * 60000;
		Timer timer = new Timer("Incoming Email Process"); //$NON-NLS-1$
		task.setTiming(timing);
		logger.info(String.format(Messages.Activator_Info_mailer_starting, // 
				STARTING_DELAY_FACTOR * timing / 60000));
		timer.schedule(task, STARTING_DELAY_FACTOR * timing*1L, timing);
		return timer;
	}
	
	/**
	 * Create a new process.
	 * 
	 * @param timing The period of the mailbox check.
	 * @param server The POP3/IMAP server host name.
	 * @param port The server port number (use 0 for default port number).
	 * @param type The server type (pop3, pop3s, imap, imaps).
	 * @param folderName The server folder in which look for incoming email (The default is INBOX).
	 * @param login The server login.
	 * @param password The server password.
	 * @param cert Certificate used to decrypt incoming messages.
	 * @see #start()
	 */
	public IncomingEmailProcessTask(int timing, String server, int port, String type, String folderName, String login, String password, X509Certificate cert, PrivateKey key) {
		this(timing, server, port, type, folderName, login, password);
		setCertificate(cert, key);
	}	

	/**
	 * Create a new process.
	 * 
	 * @param timing The period of the mailbox check.
	 * @param server The POP3/IMAP server host name.
	 * @param port The server port number (use 0 for default port number).
	 * @param type The server type (pop3, pop3s, imap, imaps).
	 * @param folderName The server folder in which look for incoming email (The default is INBOX).
	 * @param login The server login.
	 * @param password The server password.
	 * @see #start()
	 */
	public IncomingEmailProcessTask(int timing, String server, int port, String type, String folderName, String login, String password) {
		this();
		this.timing = timing;
		this.server = server;
		setType(type);
		setPort(port);
		setFolderName(folderName);
		this.login = login;
		this.password = password;
	}

	/**
	 * Default constructor.
	 */
	public IncomingEmailProcessTask() {
		super();
	}
	
	/**
	 * Start a periodic incoming emails check.
	 * <p>
	 * Don't forget to call <code>stop()</code> ot end this process.
	 */
	public void start() {
		if (timing > 0) {
			if (timer != null) {
				stop();
			}
			timer = shedule(timing,this);
		}
	}
	
	/**
	 * Stop the periodic incoming emails check.
	 */
	public void stop() {
		if (timer != null) {
			timer.cancel();
			timer = null;
		}
	}

	/**
	 * This method process the message.
	 * 
	 * <p>
	 * This method is executed into a specific thread.
	 * 
	 * @param message A new incoming email message.
	 * @throws EmailProcessException
	 */
	protected abstract void processMessage(MailMessage message) throws EmailProcessException;
	
	@Override
	public final void run() {
		if (brokecount != 0) {
			// Do not insist...
			if (brokecount > 0) {
				brokecount--;
			}
			if (brokecount > 0) {
				logger.info(String.format(Messages.EmailProcessTask_Error_bad_configuration,brokecount));
			} else {
				logger.warn(String.format(Messages.EmailProcessTask_Error_bad_configuration,brokecount));
			}
			return;
		}
		long startTime = System.currentTimeMillis();
		int nbe = 0;
		runCount++;
		// Configure the system.
		Session session = null;
		// Set system properties...
		try {
			// Ensure that connection will not lockdown this thread.
			Properties props = System.getProperties();
			String key = "mail."+type+".connectiontimeout"; //$NON-NLS-1$ //$NON-NLS-2$
			if (props.getProperty(key) == null) {
				props.setProperty(key, Integer.toString(timeout));
			}
			key = "mail."+type+".timeout"; //$NON-NLS-1$ //$NON-NLS-2$
			if (props.getProperty(key) == null) {
				props.setProperty(key, Integer.toString(timeout));
			}
			// Force to decode filenames...
			if (props.getProperty("mail.mime.decodefilename") == null) { //$NON-NLS-1$
				props.put("mail.mime.decodefilename", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			// Use a Strict Mime decode algorithm
			if (mimeStrict) {
				props.put("mail.mime.address.strict", "true"); //$NON-NLS-1$ //$NON-NLS-2$
				props.put("mail.mime.parameters.strict", "true"); //$NON-NLS-1$ //$NON-NLS-2$
				props.put("mail.mime.ignoreunknownencoding", "false"); //$NON-NLS-1$ //$NON-NLS-2$
				props.put("mail.mime.decodetext.strict", "true"); //$NON-NLS-1$ //$NON-NLS-2$				
			} else {
				props.put("mail.mime.address.strict", "false"); //$NON-NLS-1$ //$NON-NLS-2$
				props.put("mail.mime.parameters.strict", "false"); //$NON-NLS-1$ //$NON-NLS-2$
				props.put("mail.mime.ignoreunknownencoding", "true"); //$NON-NLS-1$ //$NON-NLS-2$
				props.put("mail.mime.decodetext.strict", "false"); //$NON-NLS-1$ //$NON-NLS-2$				
			}
			session = Session.getDefaultInstance(props, null);
		} catch (SecurityException e) {
			logger.debug(Messages.EmailProcessTask_Error_dead_lock);
		}
		// Run the Process...
		if (session != null) {
			try {
				Store store = session.getStore(type);
				if (store == null) {
					logger.error(Messages.EmailProcessTask_Error_invalid_protocol + type);
					brokecount = DEFAULT_BROKECOUNT;
				} else {
					// Connection...
					try {
						if (port >  0) { 
							store.connect(server, port, login, password);
						} else {
							store.connect(server, login, password);
						}
					} catch (IllegalStateException e) {
						// Already connected.
						logger.debug(e);
					}
					// Ouverture du dossier Boite de reception
					Folder folder = store.getFolder(folderName);
					if ((folder != null) && folder.exists()) {
						folder.open(Folder.READ_WRITE);
						try {
							boolean expungeneeded = false;
							//boolean someErrors = false; a traiter plus loin.
							//StringBuilder errorText = new StringBuilder();
							boolean someCErrors = false;
							StringBuilder cErrorText = new StringBuilder();
							for(Message msg: folder.getMessages()) {
								try {
									if (!msg.isSet(Flags.Flag.DELETED)) {
										MailMessage message = new MailMessage();
										if (message.load(msg, getBodyPart(msg))) {
											processMessage(message);
											msg.setFlag(Flags.Flag.DELETED, true);
											expungeneeded = type.startsWith("imap"); //$NON-NLS-1$
											nbe++;
										} else {
											// Le message n'est pas récupérable en l'état.
											someCErrors = true;
											cErrorText.append(String.format(Messages.EmailProcessTask_Error_text,message.getSender(),message.getSubject()));
											logger.warn(Messages.EmailProcessTask_Error_generic + message.getErrorMessage());
										}
									}
								} catch (Throwable e) {
									someCErrors = true;
									try {
										cErrorText.append(String.format(Messages.EmailProcessTask_Error_text,msg.getFrom().toString(),msg.getSubject()));
									} catch (MessagingException me) {
										cErrorText.append(Messages.EmailProcessTask_Error_broken_connection);
									}
									logger.warn(Messages.EmailProcessTask_Error_process + e.getLocalizedMessage(), e);
									if ((e instanceof EmailProcessException) && ((EmailProcessException)e).shouldStop()) {
										cErrorText.append(Messages.EmailProcessTask_Debug_ProcessAborted);
										break;
									}
								}
							}
							if (someCErrors && (Activator.getInstance() != null)) {
								Activator.getInstance().fireErrorNotification(cErrorText.toString());
							}
							if (expungeneeded) {
								try {
									folder.expunge();
								} catch (Throwable e) {
									logger.warn(Messages.EmailProcessTask_Error_folder_expunge + folderName, e);
								}
							}
						} finally {
							folder.close(true);
						}
					} else {
						logger.error(Messages.EmailProcessTask_Error_folder_not_exist + folderName);
						brokecount = DEFAULT_BROKECOUNT;
					}
				}
			} catch (AuthenticationFailedException e) {
				if (login != null) {
					logger.error(Messages.EmailProcessTask_Error_authentification + server + Messages.EmailProcessTask_Login + login, e);
				} else {
					logger.error(Messages.EmailProcessTask_Authentification_required + server, e);
				}
				brokecount = DEFAULT_BROKECOUNT;
			} catch (FolderNotFoundException e) {
				logger.error(Messages.EmailProcessTask_Error_Folder_Not_Exist + folderName, e);
				brokecount = DEFAULT_BROKECOUNT;
			} catch (NoSuchProviderException e) {
				logger.error(Messages.EmailProcessTask_Error_Invalid_Protocol + type, e);
				brokecount = -1; // Définitif...
			} catch (MessagingException e) {
				logger.error(Messages.EmailProcessTask_Error_Mailbox_management + e.getLocalizedMessage() + Messages.EmailProcessTask_Debug_Date + new Date().toString() + ")", e); //$NON-NLS-2$
			}
		} else {
			logger.error(Messages.EmailProcessTask_Error_null_session);
			brokecount = DEFAULT_BROKECOUNT;
		}
		// Log the run.
		if (timing > 0) {
			startTime = System.currentTimeMillis() - startTime;
			long i = (startTime * 100) / timing;
			if (nbe > 0) {
				logger.debug(String.format(Messages.EmailProcessTask_debug_Log_process, runCount, startTime, i, nbe));
			}
			if (i > 100) {
				logger.info(String.format(Messages.EmailProcessTask_Debug_treatment_too_long, startTime, timing));
			}
		}
	}

	private Part getBodyPart(Message msg) {
		if ((msg instanceof MimeMessage) && (recId != null)) {
			try {
				SMIMEEnveloped m = new SMIMEEnveloped((MimeMessage) msg);
				RecipientInformationStore recipients = m.getRecipientInfos();
		        RecipientInformation recipient = recipients.get(recId);
		        return SMIMEUtil.toMimeBodyPart(recipient.getContent(new JceKeyTransEnvelopedRecipient(privateKey) //
		        		.setProvider(BouncyCastleProvider.PROVIDER_NAME)));
			} catch (Exception e) {
				// erreur de décryptage ou message non crypté !
				logger.info(e);
			}
		}
		return msg;
	}

	/**
	 * Use a Strict Mime decode algorithm.
	 * 
	 * @param mimeStrict
	 */
	public void setMimeStrict(boolean mimeStrict) {
		this.mimeStrict = mimeStrict;
	}

	/**
	 * Use a Strict Mime decode algorithm.
	 * 
	 * @return
	 */
	public boolean isMimeStrict() {
		return mimeStrict;
	}

	/**
	 * Delay in minutes between to check. 
	 * 
	 * @return
	 * @see #start()
	 */
	public int getTiming() {
		return timing;
	}

	/**
	 * Delay in minutes between to check. 
	 * 
	 * @param timing
	 */
	public void setTiming(int timing) {
		if (timing < 0) {
			timing = 0;
		}
		this.timing = timing;
		if (timer != null) {
			start();
		}
	}

	/**
	 * @return Total number of execution of the process. 
	 */
	public int getRunCount() {
		return runCount;
	}

	/**
	 * @return Email server address.
	 */
	public String getServer() {
		return server;
	}

	/**
	 * @return Email server port.
	 */
	public int getPort() {
		return port;
	}

	/**
	 * @return Email server type (POP3, IMAP ...).
	 */
	public String getType() {
		return type;
	}

	/**
	 * @return The number of iteration before trying another run.
	 * @see #setBrokecount(int)
	 */
	public int getBrokecount() {
		return brokecount;
	}

	/**
	 * @return Email folder name (default is "INBOX").
	 */
	public String getFolderName() {
		return folderName;
	}

	/**
	 * @return Password used to connect to Email server.
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @return Login used to connect to Email server.
	 */
	public String getLogin() {
		return login;
	}
	
	/**
	 * Email server address.
	 * @param server
	 */
	public void setServer(String server) {
		this.server = server;
	}

	/**
	 * Email server port.
	 * @param port
	 */
	public void setPort(int port) {
		if (port > 0) {
			this.port = port;
		} else if (type.equals("pop3")) { //$NON-NLS-1$
			this.port = 110;
		} else if (type.equals("imap")) { //$NON-NLS-1$
			this.port = 143;
		} else if (type.equals("pop3s")) { //$NON-NLS-1$
			this.port = 992;
		} else if (type.equals("imaps")) { //$NON-NLS-1$
			this.port = 993;
		} else {
			this.port = 0;
		}
	}

	/**
	 * Email server type (POP3, IMAP ...).
	 * @param type
	 */
	public void setType(String type) {
		if ((type == null) || (type.length() == 0)) {
			this.type = "pop3"; //$NON-NLS-1$
		} else {
			this.type = type;
		}
	}

	/**
	 * Email folder name (default is "INBOX").
	 * @param folderName
	 */
	public void setFolderName(String folderName) {
		if ((folderName != null) && (folderName.length() > 0)) {
			this.folderName = folderName;
		} else {
			this.folderName = "INBOX"; //$NON-NLS-1$
		}
	}

	/**
	 * Password used to connect to Email server.
	 * @param password
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * Login used to connect to Email server.
	 * @param login
	 */
	public void setLogin(String login) {
		this.login = login;
	}

	/**
	 * @return True if this process is not working
	 * @see #setBrokecount(int)
	 */
	protected boolean isBroken() {
		return brokecount != 0;
	}

	/**
	 * Define the state of this process. Broken processes are assumed to need to
	 * a configuration change (and a Timer reset).
	 * <ul>
	 * <li>Set to zero to restart a broken process.
	 * <li>Set to a negative value to definitively broke the process.
	 * <li>Set to a positive value to momently broke the process (for a given amount of iterations). 
	 * </ul>
	 * 
	 * @param brokecount
	 */
	public void setBrokecount(int brokecount) {
		this.brokecount = brokecount;
	}

	/**
	 * The Email server connection timeout (default is 12000).
	 * @param timeout
	 */
	public void setTimeout(int timeout) {
		if (timeout <= 0) {
			this.timeout = 12000;
		} else {
			this.timeout = timeout;
		}
	}

	/**
	 * @return The Email server connection timeout (default is 12000).
	 */
	public int getTimeout() {
		return timeout;
	}

	/**
	 * Load Certificate used to decrypt emails from a KeyStore.
	 * 
	 * @param keyStore
	 * @param alias
	 */
	public void loadCertificate(KeyStore keyStore, String alias, char[] keypassword) {
		try {
			setCertificate((X509Certificate) keyStore.getCertificate(alias),
					(PrivateKey) keyStore.getKey(alias, keypassword));
		} catch (KeyStoreException | //
				UnrecoverableKeyException | //
				NoSuchAlgorithmException e) {
			logger.error(e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Load Certificate used to decrypt emails from a JKS KeyStore File.
	 * 
	 * @param keyStoreFile
	 * @param storepassword
	 * @param alias
	 */
	public void loadCertificate(File keyStoreFile, char[] storepassword, String alias, char[] keypassword) {
		try {
			KeyStore ks = KeyStore.getInstance("JKS", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
			try (FileInputStream fis = new FileInputStream(keyStoreFile)) {
				ks.load(fis, storepassword);
				loadCertificate(ks, alias, keypassword);
			} catch (Exception e) {
				logger.error(e.getLocalizedMessage(), e);
			}
		} catch (KeyStoreException | //
				java.security.NoSuchProviderException e) {
			logger.error(e.getLocalizedMessage(), e);
		}
	}
	
	/**
	 * Set the certificate used to decrypt emails.
	 * @param cert
	 */
	public void setCertificate(X509Certificate cert, PrivateKey key) {
		recId = new JceKeyTransRecipientId(cert);
		this.privateKey = key;
	}
	
	
	public static void setLogger(ILoggedPlugin logger) {
		IncomingEmailProcessTask.logger = logger;
	}
}
