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
 * Created 2006-7-20
 */
package com.arcadsoftware.email;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jakarta.mail.Address;
import jakarta.mail.Message;
import jakarta.mail.MessagingException;
import jakarta.mail.Multipart;
import jakarta.mail.Part;
import jakarta.mail.internet.MimeMultipart;
import jakarta.mail.internet.MimePart;
import jakarta.mail.internet.MimePartDataSource;
import jakarta.mail.internet.MimeUtility;
import jakarta.mail.util.SharedByteArrayInputStream;

import com.arcadsoftware.email.internal.Activator;
import com.arcadsoftware.email.internal.CallNormalizer;
import com.arcadsoftware.email.internal.HTMLParser;
import com.arcadsoftware.email.internal.Messages;
import com.arcadsoftware.mail.BinAttachment;
import com.arcadsoftware.mail.MIME2FileExtension;

/**
 * Cette classe est utilisée pour gérer toutes les subtilités de traitement d'un message Email en vue de son
 * enregistrement dans ARCAD-Customer. Cette classe est une classe intermédiaire qui à seulement pour utilité de retirer
 * de l'EJB tout traitement particulier aux Emails.
 * <p>
 * L'objectif étant d'obtenir les Adresses Email de l'expéditeur. Une Texte complet faisant office de contenu du message
 * et la liste de toutes les pièces jointes.
 * <p>
 * Cette classe remplace le traitement effectué dans l'EJB ACMailerStoreBean, en prennant en compte les FA, DM et
 * optimisations suivantes :
 * <p>
 * (Prise en charge depuis la version 3.4.1)
 * <ul>
 * <li>Les sujet vide sont signalés mais remplacés par un sujet type pour éviter le déclenchement d'une erreur dans la
 * suite du traitement.
 * <li>Les sujet dont la taille excède 4000 caractères sont tronqués (Erreur de niveau WARNING).
 * <li>Prise en compte du champ ReplyTo comme adresse de réponse
 * <li>Ajoute un nom par défaut aux pièces jointes qui n'en ont pas.
 * <li>traite les pièces jointes qui n'ont pas de noms de fichier associé.
 * <li>Obtention d'un texte (ascii) à partir d'un message envoyé uniquement en html.
 * </ul>
 * <p>
 * (Prise en charge depuis la versions 3.4.5)
 * <ul>
 * <li>Il y a 3 niveaux d'erreurs Warning, Error et Critical_Error (arrêt du traitement).
 * <li>Gestion des multi-erreurs (accumule les erreurs).
 * <li>Détecte les message en provenance de postmaster et adresses associées.
 * <li>Prise en compte des messages éventuellement tagué comme SPAMs par un filtre style SpamFilter.
 * </ul>
 * <p>
 * Régression(!) : le texte HTML est considéré comme une pièce jointe normale. Il sera ajouté aux pièces jointes du CAS.
 * Rien ne permet de différencier une version HTML du texte et une pièce jointe qui serait un document HTML et dans ce
 * cas la pièce jointe n'était pas jointe au CAS.
 * <p>
 * (Prise en charge depuis la version Refonte Arcad-Customer 2009)
 * <ul>
 * <li>Conversion à Java 1.5.
 * <li>Support Multilangue.
 * <li>Test support Java 1.6.
 * <li>Correction bug extraction message au format RTF.
 * </ul>
 * TODO Prise en charge des messages en RTF pur (à convertir en Text, voir javax.swing.text.rtf.RTFEditorKit)... TODO
 * Exploter les VCARD pour les utilisateurs inconus.
 * 
 * @author ARCAD Software
 */
public class MailMessage {

	private static final int LABEL_LIMIT = 255;
	private static final int TEXT_LIMIT = 4000;
	private static final String TRUNCATED_MESSAGE = Messages.MailMessage_MessageTruncated;
	private static final String TRUNCATED_MESSAGE_END = "\n..."; //$NON-NLS-1$
	public static final int CRITICAL_ERROR = 3;
	public static final int ERROR = 2;
	public static final int WARNING = 1;
	public static final int CORRECT = 0;

	/**
	 * Cette chaîne de caractère est utilisée pour identifier un Email qui aurait été tagué comme spam par un filtre
	 * anti-spam.
	 */
	private static String Property_Filter_Prefix = ""; //$NON-NLS-1$

	private String sender = ""; //$NON-NLS-1$
	private String replyTo = ""; //$NON-NLS-1$
	private String text = ""; //$NON-NLS-1$
	private String htmlText = ""; //$NON-NLS-1$
	private String cc = ""; //$NON-NLS-1$
	private String to = ""; //$NON-NLS-1$
	private String bcc = ""; //$NON-NLS-1$
	private int errorLevel = CORRECT;
	private String xmailer = ""; //$NON-NLS-1$
	private boolean deliveryStatus;
	private boolean autoGenerated;
	private String errorMessage = ""; //$NON-NLS-1$
	private String subject = ""; //$NON-NLS-1$
	private final ArrayList<BinAttachment> attachments = new ArrayList<BinAttachment>();

	/**
	 * Création du message.
	 */
	public MailMessage() {
		super();
	}

	/**
	 * Charge le message à partir du message de la boite aux lettres.
	 * 
	 * @param msg
	 *            message original
	 * @return true si le message a pu être traité (ce qui n'empêche pas qu'il soit en Erreur).
	 */
	public boolean load(Message msg, Part content) {
		// Récupération du Sujet du message...
		try {
			subject = msg.getSubject();
		} catch (Exception e) {}
		if ((subject == null) || (subject.length() == 0)) {
			subject = Messages.MailMessage_EmptySubjectText;
			setErrorLevel(ERROR, Messages.MailMessage_ErrorEmptySubject);
		} else if (subject.length() > LABEL_LIMIT) {
			setErrorLevel(WARNING, Messages.MailMessage_ErrorSubjectTooLong);
			subject = subject.substring(0, LABEL_LIMIT);
		}
		if ((Property_Filter_Prefix.length() > 0) &&
				(subject.startsWith(Property_Filter_Prefix))) {
			deliveryStatus = true;
			setErrorLevel(ERROR, Messages.MailMessage_ErrorSpamMessage);
		}
		// Récupération de l'adresse Email de l'expéditeur.
		try {
			String retrievedAddresses = retrieveAddresses(msg.getFrom());
			sender = decodeAddress(retrievedAddresses);
			if (sender == null) {
				setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorSenderUnknown);
				return false;
			}
			if (sender.length() == 0) {
				setErrorLevel(ERROR, Messages.MailMessage_ErrorSenderEmpty);
			}
			if (sender.length() > LABEL_LIMIT) {
				setErrorLevel(WARNING, Messages.MailMessage_ErrorSenderTooLong);
				sender = sender.substring(0, LABEL_LIMIT);
			}
			// prise en charge du tag ReplyTo.
			if ((msg.getReplyTo() != null) && (msg.getReplyTo().length > 0)) {
				replyTo = decodeAddress(retrieveAddresses(msg.getReplyTo()));
			} else {
				replyTo = sender;
			}
		} catch (MessagingException ex) {
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorCantReadRecipent);
			return false;
		}
		// Recupération (optionnelle du mailer...)
		try {
			String[] mailer = msg.getHeader("X-Mailer"); //$NON-NLS-1$
			StringBuilder sb = new StringBuilder();
			if (mailer != null) {
				for (String s : mailer) {
					if (sb.length() > 0) {
						sb.append(' ');
					}
					sb.append(s);
				}
			}
			xmailer = sb.toString();
		} catch (MessagingException e) {
			Activator.getInstance().debug(e.getLocalizedMessage(), e);
		}
		try {
			String[] ag = msg.getHeader("Auto-Submitted"); //$NON-NLS-1$
			if (ag != null) {
				for (String s : ag) {
					if ((s != null) && !"no".equalsIgnoreCase(s)) { //$NON-NLS-1$
						autoGenerated = true;
					}
				}
			}
		} catch (MessagingException e) {
			Activator.getInstance().debug(e.getLocalizedMessage(), e);
		}
		// Récupération des autres adresses du message...
		try {
			// Destinataire
			Address[] ads = msg.getRecipients(Message.RecipientType.TO);
			if ((ads != null) && (ads.length > 0)) {
				to = retrieveAddresses(ads);
			}
			// Copie
			ads = msg.getRecipients(Message.RecipientType.CC);
			if ((ads != null) && (ads.length > 0)) {
				cc = retrieveAddresses(ads);
			}
		} catch (MessagingException e) {
			setErrorLevel(WARNING, Messages.MailMessage_ErrorCantReadOptionalRecipient);
			Activator.getInstance().debug(Messages.MailMessage_ErrorCantReadOptionalRecipient, e);
		}
		try {
			// Copie cachée (au cas ou, mais normalement la liste est vide !)
			Address[] ads = msg.getRecipients(Message.RecipientType.BCC);
			if ((ads != null) && (ads.length > 0)) {
				bcc = retrieveAddresses(ads);
			}
		} catch (MessagingException e) {
			Activator.getInstance().debug(e.getLocalizedMessage(), e);
		}
		// Teste si l'on a affaire à un autre Robot Email ou des adresses système...
		String lcSender = sender.toLowerCase();
		if ((lcSender.startsWith("postmaster@")) || //$NON-NLS-1$
				(lcSender.startsWith("hostmaster@")) || //$NON-NLS-1$
				(lcSender.startsWith("mailer-daemon@")) || //$NON-NLS-1$
				(lcSender.startsWith("mailerdaemon@")) || //$NON-NLS-1$
				(lcSender.startsWith("abuse@")) || //$NON-NLS-1$
				(lcSender.startsWith("root@")) || //$NON-NLS-1$
				(lcSender.startsWith("uucp@")) || //$NON-NLS-1$
				(lcSender.startsWith("mailer@")) || //$NON-NLS-1$
				(lcSender.startsWith("mailmaster@"))) { //$NON-NLS-1$
			deliveryStatus = true;
			setErrorLevel(WARNING, Messages.MailMessage_ErrorSenderUseSystemAddress);
		}
		// [ML] Encapsulation de n'importe quelle erreur de décodage.
		try {
			// Récupération du contenu du message.
			if (!processContent(content)) {
				return false;
			}
		} catch (Exception e) {
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorDecoding + e.getMessage());
			Activator.getInstance().debug(Messages.MailMessage_ErrorDecoding, e);
			return false;
		}
		// Si le texte n'a pas été renseigné alors qu'il y a une version HTML
		// et on en déduit une version textuelle
		if ((text == null) || (text.length() == 0)) {
			if ((htmlText != null) && (htmlText.length() > 0)) {
				// code extrait de ACIndexStoreBean !!!
				HTMLParser parser = new HTMLParser(new StringReader(htmlText));
				try {
					BufferedReader bufferedReader = new BufferedReader(parser.getReader());
					StringBuilder HTMLText = new StringBuilder();
					char[] buffer = new char[32768];
					int bufferLength = 0;
					while (bufferLength != -1) {
						bufferLength = bufferedReader.read(buffer, 0, 32768);
						if (bufferLength != -1) {
							HTMLText.append(buffer, 0, bufferLength);
						}
					}
					bufferedReader.close();
					text = HTMLText.toString();
				} catch (IOException e) {
					setErrorLevel(WARNING, Messages.MailMessage_ErrorHTMLConversion);
					Activator.getInstance().debug(Messages.MailMessage_ErrorHTMLConversion, e);
				}
			} else {
				setErrorLevel(WARNING, Messages.MailMessage_ErrorMessageEmpty);
			}
		}
		// Si le texte dépasse les 4000 caractêres alors on le tronque.
		if ((text != null) && (text.length() > TEXT_LIMIT)) {
			addTextAttachment("text/plain", getUniqueFileName("body", ".txt"), text); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			setErrorLevel(WARNING, Messages.MailMessage_ErrorMessageTruncated);
			text = TRUNCATED_MESSAGE
					+ text.substring(0, TEXT_LIMIT - TRUNCATED_MESSAGE.length() - TRUNCATED_MESSAGE_END.length())
					+ TRUNCATED_MESSAGE_END;
		}
		// Résolution des ID des élément inlines.
		for (BinAttachment a : attachments) {
			if (a.getMimeType().endsWith("html") || a.getMimeType().endsWith("xhtml+xml")) { //$NON-NLS-1$ //$NON-NLS-2$
				replaceContentIds(a);
			}
		}
		return true;
	}

	/**
	 * Découpe le contenu du message. Récupère les textes (les assemble), et les pièces jointes.
	 * 
	 * @return
	 */
	private boolean processContent(Part part) {
		try {
			// Marque les messages de type rapport d'erreur !
			if ((part.isMimeType("multipart/report")) || //$NON-NLS-1$
					(part.isMimeType("message/delivery-status"))) { //$NON-NLS-1$
				deliveryStatus = true;
			}
			if (part.isMimeType("multipart/*")) { //$NON-NLS-1$
				if (part.getContent() instanceof Multipart) {
					// Traitement d'un Multipart...
					try {
						Multipart mp = (Multipart) part.getContent();
						// Une cast exception ici sera du à un pb de class loader...
						/*
						 * Palliative MimePartDataSource ds = new MimePartDataSource(message); MimeMultipart multi = new
						 * MimeMultipart(ds);
						 */
						int count = mp.getCount();
						boolean result = true;
						for (int i = 0; i < count; i++)
							if (!processContent(mp.getBodyPart(i))) {
								result = false;
							}
						return result;
					} catch (IOException e) {
						setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionMPP + e.getMessage());
						Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionMPP, e);
						return false;
					} catch (MessagingException e) {
						setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionMPP + e.getMessage());
						Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionMPP, e);
						return false;
					}
				} else if (part instanceof MimePart) {
					try {
						MimeMultipart multi = new MimeMultipart(new MimePartDataSource((MimePart) part));
						int count = multi.getCount();
						boolean result = true;
						for (int i = 0; i < count; i++)
							if (!processContent(multi.getBodyPart(i))) {
								result = false;
							}
						return result;
					} catch (MessagingException e) {
						setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionMPP + e.getMessage());
						Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionMPP, e);
						return false;
					}
				} else {
					Activator.getInstance().debug("The Email message Multipart is not decoded: " + part.getContentType()
							+ ", Description: " + part.getDescription());
					setErrorLevel(CRITICAL_ERROR, "The message is not correctly decoded.");
					return false;
				}
			} else if (part.isMimeType("message/rfc822")) { //$NON-NLS-1$
				// This is a Nested Message (i.e. a .msg file)
				try {
					return processContent((Part) part.getContent());
				} catch (IOException e) {
					setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionNMP + e.getMessage());
					Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionNMP, e);
					return false;
				} catch (MessagingException e) {
					setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionNMP + e.getMessage());
					Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionNMP, e);
					return false;
				}
			} else if (part.isMimeType("text/plain")) { //$NON-NLS-1$
				// This is plain text
				String disp = null;
				try {
					disp = part.getDisposition();
				} catch (MessagingException e) {
					Activator.getInstance().debug(e.getLocalizedMessage(), e);
				}
				// many mailers don't include a Content-Disposition
				if ((disp != null) && disp.equalsIgnoreCase(Part.ATTACHMENT)) {
					// Traite le texte comme une pièce jointe.
					return addAttachment(part);
				}
				// Ajoute le message au texte principal.
				try {
					if (part.getContent() != null) {
						if ((text == null) || (text.length() == 0)) {
							text = getStringContent(part);
						} else {
							text = text + "\n\n\n" + getStringContent(part); //$NON-NLS-1$
						}
					}
					return true;
				} catch (IOException e) {
					setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionTMP + e.getMessage());
					Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionTMP, e);
					return false;
				} catch (MessagingException e) {
					setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionTMP + e.getMessage());
					Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionTMP, e);
					return false;
				} catch (Exception e) {
					// [ML] Attention: cette méthode permet de récupèrer des Emails incomplés
					Activator.getInstance().error(Messages.MailMessage_LogErrorDuringProcess, e);
					setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorDecodingContent + e.getMessage());
					return false;
				}

			} else if (part.isMimeType("text/html")) { //$NON-NLS-1$
				// message au format HTML...
				// Enregistre le message HTML avant de l'enregistrer comme pièce jointe.
				if ((htmlText == null) || (htmlText.length() == 0)) {
					try {
						htmlText = getStringContent(part);
					} catch (IOException e) {
						setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionHMP + e.getMessage());
						Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionHMP, e);
						return false;
					} catch (MessagingException e) {
						setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionHMP + e.getMessage());
						Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionHMP, e);
						return false;
					} catch (Exception e) {
						// [ML] Attention: cette méthode permet de récupérer des Emails incomplés
						Activator.getInstance().error(Messages.MailMessage_LogErrorDuringProcess, e);
						setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorDecodingAttachment + e.getMessage());
						return false;
					}
				}
				return addAttachment(part);
			} else {
				return addAttachment(part);
			}
		} catch (MessagingException e) {
			// en théorie seuls les isMimeType peuvent atterir ici.
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionMT + e.getMessage());
			Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionMT, e);
			return false;
		} catch (Exception e) {
			// Erreur générale durant le traitement.
			Activator.getInstance().error(Messages.MailMessage_LogErrorDuringProcess, e);
			return false;
		}
	}

	private String getStringContent(Part part) throws IOException, MessagingException {
		final Object content = part.getContent();
		try {
			// Test if the part is not an undecoded part !
			if (content instanceof String) {
				return (String) content;
			}
			// Default treatment (do not decode message !)
			InputStream is = part.getInputStream();
			if ((part instanceof MimePart) && (is instanceof SharedByteArrayInputStream)) {
				try {
					is = MimeUtility.decode(is, ((MimePart) part).getEncoding());
				} catch (Exception e) {
					Activator.getInstance().debug(e);
				}
			}
			BufferedReader reader = new BufferedReader(new InputStreamReader(is));
			try {
				String thisLine = reader.readLine();
				StringWriter sw = new StringWriter();
				try {
					while (thisLine != null) {
						sw.write(thisLine);
						sw.write("\n"); //$NON-NLS-1$
						thisLine = reader.readLine();
					}
					sw.flush();
					return sw.toString();
				} finally {
					sw.close();
				}
			} finally {
				reader.close();
			}
		} catch (Exception t) {
			Activator.getInstance().debug(t);
		}
		if (content == null) {
			return ""; //$NON-NLS-1$
		}
		// Unsupported Encoded content !!!
		return content.toString();
	}

	/**
	 * @param part
	 * @return
	 */
	private boolean addAttachment(Part part) {
		// Ajoute les attachement de type texte à part... (pour qu il soient décodés)
		Object o;
		try {
			o = part.getContent();
		} catch (IOException e) {
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionAMP + e.getLocalizedMessage());
			Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionAMP, e);
			return false;
		} catch (MessagingException e) {
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionAMP + e.getLocalizedMessage());
			Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionAMP, e);
			return false;
		}
		String filename = null;
		try {
			filename = part.getFileName();
		} catch (MessagingException e) {
			Activator.getInstance().debug(e.getLocalizedMessage(), e);
		}
		String extname = null;
		try {
			// Traitement du nom du fichier...
			if ((filename == null) || (filename.length() == 0)) {
				String ds = part.getDisposition();
				if (ds != null) {
					int i = ds.indexOf("filename=\""); //$NON-NLS-1$
					if (i > -1) {
						filename = ds.substring(i + 10);
						i = filename.indexOf('"');
						if (i > -1) {
							filename = filename.substring(0, i);
						}
					}
				}
				if ((filename == null) || (filename.length() == 0)) {
					String mt = part.getContentType();
					if (mt != null) {
						int i = mt.indexOf("name=\""); //$NON-NLS-1$
						if (i > -1) {
							filename = mt.substring(i + 6);
							i = filename.indexOf('"');
							if (i > -1) {
								filename = filename.substring(0, i);
							}
						} else {
							i = mt.indexOf('/');
							if (i > -1) {
								mt = mt.substring(0, i).trim();
								if ("text".equalsIgnoreCase(mt)) { //$NON-NLS-1$
									filename = "body"; //$NON-NLS-1$
								} else if ("application".equalsIgnoreCase(mt)) { //$NON-NLS-1$
									filename = "file"; //$NON-NLS-1$
								} else {
									filename = mt;
								}
							}
						}
					} else {
						filename = "body"; //$NON-NLS-1$
					}
				}
			}
			if (filename.indexOf("=?") >= 0) { //$NON-NLS-1$
				// [ML] 03.2007 Dés-encodage des nom de fichier illégalement encodés.
				try {
					filename = MimeUtility.decodeText(filename);
				} catch (Throwable e) {
					Activator.getInstance().debug(e.getLocalizedMessage(), e);
				}
				// [ML] 11.2011
				try {
					if (Class.forName("java.text.Normalizer") != null) { //$NON-NLS-1$
						filename = CallNormalizer.normalize(filename);
					}
				} catch (Throwable e) {
					Activator.getInstance().debug(e.getLocalizedMessage(), e);
				}
			}
			int i = filename.indexOf("."); //$NON-NLS-1$
			if (i >= 0) {
				extname = filename.substring(i);
				filename = filename.substring(0, i);
			}
			if ((extname == null) || (extname.trim().length() <= 1)) {
				extname = MIME2FileExtension.getFileExtension(part.getContentType());
			}
			// Traitement d'une PJ textuelle...
			if (o instanceof String) {
				if ((extname == null) || (extname.length() == 0)) {
					if (part.isMimeType("text/html")) { //$NON-NLS-1$
						extname = ".html"; //$NON-NLS-1$
					} else {
						extname = ".txt"; //$NON-NLS-1$
					}
				}
				return addTextAttachment(part.getContentType(), getUniqueFileName(filename, extname), (String) o);
			}
			ByteArrayOutputStream ba = new ByteArrayOutputStream();
			// write to created file
			byte[] fileData = new byte[0];
			try {
				part.getDataHandler().writeTo(ba);
				fileData = ba.toByteArray();
			} catch (MessagingException e) {
				setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorMExceptionAMP + e.getMessage());
				Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionAMP, e);
				return false;
			} finally {
				ba.close();
			}
			if ((filename == null) || (filename.length() == 0)) {
				return addBinAttachment(getContentId(part), part.getContentType(), null, fileData);
			}
			return addBinAttachment(getContentId(part), part.getContentType(), getUniqueFileName(filename, extname),
					fileData);
		} catch (IOException e) {
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorIOExceptionAMP + e.getMessage());
			Activator.getInstance().debug(Messages.MailMessage_ErrorIOExceptionAMP, e);
			return false;
		} catch (MessagingException e) {
			setErrorLevel(CRITICAL_ERROR, Messages.MailMessage_ErrorAttachmentMimeType + e.getMessage());
			Activator.getInstance().debug(Messages.MailMessage_ErrorAttachmentMimeType, e);
			return false;
		}
	}

	private String getContentId(Part part) {
		// Decode the Content-ID of the given Part.
		try {
			String[] h = part.getHeader("Content-ID"); //$NON-NLS-1$
			if ((h != null) && (h.length > 0)) {
				String id = h[0];
				if (id == null) {
					return null;
				}
				if (id.length() < 4) {
					return id;
				}
				id = id.trim();
				if (id.charAt(0) == '<') {
					id = id.substring(1);
				}
				if (id.charAt(id.length() - 1) == '>') {
					id = id.substring(0, id.length() - 1);
				}
				return id;
			}
		} catch (MessagingException e) {
			Activator.getInstance().debug(Messages.MailMessage_ErrorMExceptionAMP + e.getLocalizedMessage(), e);
		}
		return null;
	}

	/**
	 * Création du la pièce jointe de type binaire.
	 * 
	 * @param mimeType
	 * @param filename
	 * @param bs
	 * @return
	 */
	private boolean addBinAttachment(String contentId, String mimeType, String filename, byte[] bs) {
		if ((mimeType == null) || (mimeType.length() == 0)) {
			mimeType = "application/octet-stream"; //$NON-NLS-1$
		}
		int i = mimeType.indexOf(';');
		if (i > -1) {
			mimeType = mimeType.substring(0, i);
		}
		if ((filename == null) || (filename.length() == 0)) {
			filename = getUniqueFileName("Attachment", MIME2FileExtension.getFileExtension(mimeType)); //$NON-NLS-1$
		}
		attachments.add(new BinAttachment(contentId, filename.trim(), mimeType.trim(), bs));
		return true;
	}

	/**
	 * Ajoute une pièce jointe de type texte.
	 * 
	 * @param mimeType
	 * @param filename
	 * @param content
	 * @return
	 */
	private boolean addTextAttachment(String mimeType, String filename, String content) {
		return addBinAttachment(null, mimeType, filename, content.getBytes());
	}

	/**
	 * Retourne un nom de fichier Unique dans la liste des pièces jointes.
	 * 
	 * @param name
	 * @param ext
	 * @return
	 */
	private String getUniqueFileName(String name, String ext) {
		String result = name + ext;
		boolean exist = true;
		int nb = 0;
		while (exist) {
			exist = false;
			for (BinAttachment a : attachments) {
				if (result.equalsIgnoreCase(a.getFileName())) {
					exist = true;
					break;
				}
			}
			if (exist) {
				nb++;
				result = name + String.valueOf(nb) + ext;
			}
		}
		return result;
	}

	/**
	 * Mise à jour du niveau d'erreur (gère les multi-erreurs).
	 * 
	 * @param level
	 * @param message
	 */
	private void setErrorLevel(int level, String message) {
		if (errorLevel > CORRECT) {
			errorMessage = errorMessage + ", " + message; //$NON-NLS-1$
		} else {
			errorMessage = message;
		}
		if (errorLevel < level) {
			errorLevel = level;
		}
	}

	/**
	 * Decode les adresses mail. Par exemple, les adresses peuvent être répertoriées ainsi : "Valérie SOLDAN
	 * <vsoldan@arcadsoftware.com>". On ne rend alors que l'adresse email contenu dans les <>
	 *
	 * @param address
	 * @return
	 */
	private String decodeAddress(String address) {
		if (address != null) {
			int toDecode = address.indexOf("<"); //$NON-NLS-1$
			if (toDecode >= 0) {
				return address.substring(toDecode + 1, address.indexOf(">")); //$NON-NLS-1$
			}
		}
		return address;
	}

	/**
	 * Construction d'une chaine contenant les adresses d'un tableau, chaque élément étant séparés par une virgule
	 *
	 * @param padresses
	 *            Tableau d'entrée
	 * @return la liste des adresses, null s'il y a eu une erreur, "" si le tableau était vide.
	 */
	private String retrieveAddresses(Address[] padresses) {
		StringBuilder sreturn = new StringBuilder();
		try {
			if (padresses != null) {
				for (Address a : padresses) {
					if (sreturn.length() > 0) {
						sreturn.append(" , "); //$NON-NLS-1$
					}
					sreturn.append(DecodeJavaIso(a.toString()));
				}
				return MimeUtility.decodeText(sreturn.toString());
			}
		} catch (Exception e) {
			Activator.getInstance().debug(e.getLocalizedMessage(), e);
		}
		return null;
	}

	/**
	 * Filtre le texte passé en paramètre et l'encode en ISO 
	 *
	 * @param text
	 * @return texte décodé
	 */
	private String DecodeJavaIso(String text) {
		// TODO Rewrite this conversion.
		text = text.trim();
		// Unquote the text...
		if (text.startsWith("\"")) { //$NON-NLS-1$
			// Récupération du 1er
			text = text.substring(1, text.length());
			// Et du dernier
			text = text.substring(0, text.lastIndexOf('\"')) + text.substring(text.lastIndexOf('\"') + 1);
			int i = 0;
			// remplace "\\=" par "="
			String temptext = ""; //$NON-NLS-1$
			while (i < text.length()) {
				// premier slash et second slash et signe egal
				if ((text.charAt(i) == '\\') &&
						(text.charAt(i + 1) == '\\') &&
						(text.charAt(i + 2) == '=')) {
					temptext = temptext + '=';
					i = i + 3;
				} else {
					temptext = temptext + text.charAt(i);
					i++;
				}
			}
			return temptext;
		}
		// Pas de filtre à appliquer
		return text;
	}

	private void replaceContentIds(BinAttachment attachment) {
		try {
			// FIXME Support HTML Charset !!!
			String content = new String(attachment.getContent());
			for (BinAttachment a : attachments) {
				String cid = a.getContentId();
				if ((cid != null) && (cid.length() > 0)) {
					content = content.replace("cid:" + cid, a.getFileName()); //$NON-NLS-1$
				}
			}
			attachment.setContent(content.getBytes());
		} catch (Throwable e) {
			Activator.getInstance().debug(e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Indique si le message est un message de notification automatique ou si l'expéditeur a été reconnu comme une
	 * adresse de service (e.g. mailerdeamon@)
	 * 
	 * @return
	 */
	public boolean isDeliveryStatus() {
		return deliveryStatus;
	}

	/**
	 * Retourne l'adresse de réponse indiqué dans le message ou à défaut l'adresse de l'expéditeur.
	 * 
	 * @return
	 */
	public String getReplyTo() {
		return replyTo;
	}

	/**
	 * @return
	 */
	public String getSender() {
		return sender;
	}

	/**
	 * @return
	 */
	public String getText() {
		return text;
	}

	/**
	 * @return
	 */
	public String getErrorMessage() {
		return errorMessage;
	}

	/**
	 * @return
	 */
	public String getSubject() {
		return subject;
	}

	/**
	 * Retourne le niveau d'erreur dans l'analyse du message : CRITICAL_ERROR = erreur critique qui à abouti à un
	 * abandon du traitement (aucune garantie ne peut être donné sur le contenu des autres propriétées. ERROR = une
	 * erreur c'est produite durant le traitement mais le message a été traité quand même. WARNING = un événement
	 * inhabituel ou le contenu du message n'est pas conforme à un message classique. CORRECT = le message a été
	 * normalement traité.
	 * 
	 * @return
	 */
	public int getErrorLevel() {
		return errorLevel;
	}

	/**
	 * Retourne le texte HTML si une version html du message était contenu dans le message. (Attention, il n'est pas
	 * possible de distinguer à coup sûr une pièce jointe au format html d'un corps de message au format html. C'est
	 * pour il faut prendre des précaution avec ce résultat).
	 * 
	 * @return
	 */
	public String getHtmlText() {
		return htmlText;
	}

	/**
	 * Nombre de pièces jointes.
	 * 
	 * @return
	 */
	public int getAttachmentCount() {
		return attachments.size();
	}

	/**
	 * Retourne la pièce jointe spécifié.
	 * 
	 * @param index
	 * @return
	 */
	public BinAttachment getAttachment(int index) {
		return (BinAttachment) attachments.get(index);
	}

	/**
	 * Retourne un itérateur sur la liste des pièces jointes.
	 * 
	 * @return
	 */
	public Iterator<BinAttachment> getAttachmentIterator() {
		return attachments.iterator();
	}

	/**
	 * @return Renvoie bcc.
	 */
	public String getBcc() {
		return bcc;
	}

	/**
	 * @return Renvoie cc.
	 */
	public String getCc() {
		return cc;
	}

	/**
	 * @return Renvoie to.
	 */
	public String getTo() {
		return to;
	}

	/**
	 * @return
	 */
	public List<BinAttachment> getAttachments() {
		return attachments;
	}

	/**
	 * @return X-Mailer header values (whitespaced).
	 */
	public String getXmailer() {
		return xmailer;
	}

	/**
	 * @return true if this message is auto-generated.
	 */
	public boolean isAutoGenerated() {
		return autoGenerated;
	}
}
