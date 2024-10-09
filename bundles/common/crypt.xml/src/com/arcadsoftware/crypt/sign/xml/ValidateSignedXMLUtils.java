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
package com.arcadsoftware.crypt.sign.xml;

import java.security.Key;
import java.security.KeyException;
import java.security.Provider;
import java.security.PublicKey;
import java.security.SignatureException;
import java.util.Iterator;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.jcp.xml.dsig.internal.dom.XMLDSigRI;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * This is a simple example of validating an XML Signature using the JSR 105 API. It assumes the key needed to validate
 * the signature is contained in a KeyValue KeyInfo.
 */
public class ValidateSignedXMLUtils {

	private static final String SIGNATURE_NODE = "Signature"; //$NON-NLS-1$

	/**
	 * @param inputFile
	 * @return
	 * @throws SignatureException
	 *             threw when signature is invalid or not found
	 */
	public static boolean validateFile(String inputFile) throws SignatureException {
		// Instantiate the document to be validated
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, ""); // Compliant
		dbf.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, ""); // compliant
		dbf.setNamespaceAware(true);
		Document doc;
		try {
			doc = dbf.newDocumentBuilder().parse(inputFile);
			return validate(doc);
		} catch (SignatureException e) {
			throw e;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 
	 * @param doc
	 * @return
	 * @throws Exception
	 */
	public static boolean validate(Document doc) throws Exception {
		// Find Signature element
		NodeList nl = doc.getElementsByTagNameNS(XMLSignature.XMLNS, SIGNATURE_NODE);
		if (nl.getLength() == 0) {
			throw new SignatureException("Cannot find Signature element");
		}
		// Create a DOM XMLSignatureFactory that will be used to unmarshal the
		// document containing the XMLSignature
		String providerName = System.getProperty("jsr105Provider", XMLDSigRI.class.getName()); //$NON-NLS-1$
		XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM", //$NON-NLS-1$
				(Provider) Class.forName(providerName).getConstructor().newInstance());
		// Create a DOMValidateContext and specify a KeyValue KeySelector
		// and document context
		DOMValidateContext valContext = new DOMValidateContext(new KeyValueKeySelector(), nl.item(0));
		// unmarshal the XMLSignature
		XMLSignature signature = fac.unmarshalXMLSignature(valContext);
		// Validate the XMLSignature (generated above)
		boolean coreValidity = signature.validate(valContext);
		// Check core validation status
		if (coreValidity == false) {
			signature.getSignatureValue().validate(valContext);
			// check the validation status of each Reference
			Iterator<?> i = signature.getSignedInfo().getReferences().iterator();
			while (i.hasNext()) {
				((Reference) i.next()).validate(valContext);
			}
			throw new SignatureException("Invalid Signature"); //$NON-NLS-1$
		}
		return true;
	}

	/*
	 * KeySelector which retrieves the public key out of the KeyValue element and returns it. NOTE: If the key algorithm
	 * doesn't match signature algorithm, then the public key will be ignored.
	 */
	private static class KeyValueKeySelector extends KeySelector {
		@Override
		public KeySelectorResult select(KeyInfo keyInfo, KeySelector.Purpose purpose, AlgorithmMethod method,
				XMLCryptoContext context) throws KeySelectorException {
			if (keyInfo == null) {
				throw new KeySelectorException("Null KeyInfo object!"); //$NON-NLS-1$
			}
			SignatureMethod sm = (SignatureMethod) method;
			List<?> list = keyInfo.getContent();
			for (int i = 0; i < list.size(); i++) {
				XMLStructure xmlStructure = (XMLStructure) list.get(i);
				if (xmlStructure instanceof KeyValue) {
					PublicKey pk = null;
					try {
						pk = ((KeyValue) xmlStructure).getPublicKey();
					} catch (KeyException ke) {
						throw new KeySelectorException(ke);
					}
					// make sure algorithm is compatible with method
					if (algEquals(sm.getAlgorithm(), pk.getAlgorithm())) {
						return new SimpleKeySelectorResult(pk);
					}
				}
			}
			throw new KeySelectorException("No KeyValue element found!"); //$NON-NLS-1$
		}
		// @@@FIXME: this should also work for key types other than DSA/RSA
		static boolean algEquals(String algURI, String algName) {
			return (algName.equalsIgnoreCase("DSA") && //$NON-NLS-1$
							algURI.equalsIgnoreCase(SignatureMethod.DSA_SHA1)) || //
					(algName.equalsIgnoreCase("RSA") && //$NON-NLS-1$
							algURI.equalsIgnoreCase(SignatureMethod.RSA_SHA1)) || //
					(algName.equalsIgnoreCase("EC") && //$NON-NLS-1$
							algURI.equalsIgnoreCase("http://www.w3.org/2001/04/xmldsig-more#ecdsa-sha256"));
		}
	}

	private static class SimpleKeySelectorResult implements KeySelectorResult {
		private PublicKey pk;

		SimpleKeySelectorResult(PublicKey pk) {
			this.pk = pk;
		}

		@Override
		public Key getKey() {
			return pk;
		}
	}
}