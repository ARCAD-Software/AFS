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
package com.arcadsoftware.crypt;

import java.security.MessageDigest;
import java.security.cert.X509Certificate;
import java.util.Date;

/**
 * Facade class to provide end-user understandable information about an TLS X509 Certificates.
 *
 * @author ARCAD Software
 * @see InstallCertificates
 */
public class CertificateInformation implements Cloneable {

	private final X509Certificate certificate;
	private String alias;

	/**
	 * Create CertificateInformation from a certificate.
	 * 
	 * @param certificate
	 */
	public CertificateInformation(X509Certificate certificate) {
		super();
		this.certificate = certificate;
	}

	/**
	 * Create CertificateInformation from a certificate and store its original alias.
	 * 
	 * @param alias
	 * @param certificate
	 */
	public CertificateInformation(String alias, X509Certificate certificate) {
		super();
		this.certificate = certificate;
		setAlias(alias);
	}

	/**
	 * Get the Certificate itself.
	 * 
	 * @return
	 */
	public X509Certificate getCertificate() {
		return certificate;
	}

	/**
	 * Get the Certificate Subject (the site associated to it).
	 * 
	 * @return
	 */
	public String getSubject() {
		return certificate.getSubjectX500Principal().toString();
	}

	/**
	 * Get the Certificate Serial Number.
	 *
	 * @return
	 */
	public String getSerialNumber() {
		return Crypto.byteArrayToHexString(certificate.getSerialNumber().toByteArray(), ':');
	}

	/**
	 * Get the organization that have generated this certificate.
	 * 
	 * @return
	 */
	public String getIssuer() {
		return certificate.getIssuerX500Principal().toString();
	}

	/**
	 * Get the certificate algorithm name.
	 * 
	 * @return
	 */
	public String getAlgorithm() {
		return certificate.getSigAlgName();
	}

	/**
	 * Get the SHA1 fingerprint of the certificate.
	 * 
	 * @return
	 */
	public String getFingerPrint() {
		// For CRC Only: safe !
		try {
			MessageDigest sha1 = MessageDigest.getInstance("SHA-1"); //$NON-NLS-1$
			sha1.reset();
			sha1.update(certificate.getEncoded());
			return Crypto.byteArrayToHexString(sha1.digest(), ':');
		} catch (final Exception e) {
			return ""; //$NON-NLS-1$
		}
	}

	/**
	 * @return true if the Certificate is currently valid.
	 */
	public boolean isValid() {
		try {
			certificate.checkValidity();
			return true;
		} catch (final Throwable e) {
			return false;
		}
	}

	/**
	 * Get the certificate last date of validity.
	 *
	 * @return
	 */
	public Date getExpirationDate() {
		return certificate.getNotAfter();
	}

	/**
	 * Get the certificate first date of validity.
	 *
	 * @return
	 */
	public Date getStartDate() {
		return certificate.getNotBefore();
	}

	@Override
	public int hashCode() {
		return certificate.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof CertificateInformation)
				&& certificate.equals(((CertificateInformation) obj).certificate);
	}

	@Override
	protected CertificateInformation clone() {
		return new CertificateInformation(alias, certificate);
	}

	@Override
	public String toString() {
		if (alias != null) {
			return alias + ":\n" + certificate.toString(); //$NON-NLS-1$
		}
		return certificate.toString();
	}

	/**
	 * Get the name of this certificate into the Key store.
	 * <p>
	 * This properties is only valid for certificate that are or will be stored.
	 *
	 * @return
	 */
	public String getAlias() {
		return alias;
	}

	/**
	 * Define the certificate alias.
	 * 
	 * @param alias
	 */
	public void setAlias(String alias) {
		this.alias = alias;
	}

}
