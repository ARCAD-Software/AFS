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

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.Provider;
import java.util.Collections;

import javax.xml.XMLConstants;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.crypto.dsig.spec.TransformParameterSpec;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.jcp.xml.dsig.internal.dom.XMLDSigRI;
import org.w3c.dom.Document;

/**
 * This is a simple example of generating an Enveloped XML Signature using the JSR 105 API. The resulting signature will
 * look like (key and signature values will be different):
 *
 * <pre>
 * <code>
 *<Envelope xmlns="urn:envelope">
 * <Signature xmlns="http://www.w3.org/2000/09/xmldsig#">
 *   <SignedInfo>
 *     <CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n
-20010315"/>
 *     <SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#dsa-sha1"/>
 *     <Reference URI="">
 *       <Transforms>
 *         <Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>
 *       </Transforms>
 *       <DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>
 *       <DigestValue>K8M/lPbKnuMDsO0Uzuj75lQtzQI=<DigestValue>
 *     </Reference>
 *   </SignedInfo>
 *   <SignatureValue>
 *     DpEylhQoiUKBoKWmYfajXO7LZxiDYgVtUtCNyTgwZgoChzorA2nhkQ==
 *   </SignatureValue>
 *   <KeyInfo>
 *     <KeyValue>
 *       <DSAKeyValue>
 *         <P>
 *           rFto8uPQM6y34FLPmDh40BLJ1rVrC8VeRquuhPZ6jYNFkQuwxnu/wCvIAMhukPBL
 *           FET8bJf/b2ef+oqxZajEb+88zlZoyG8g/wMfDBHTxz+CnowLahnCCTYBp5kt7G8q
 *           UobJuvjylwj1st7V9Lsu03iXMXtbiriUjFa5gURasN8=
 *         </P>
 *         <Q>
 *           kEjAFpCe4lcUOdwphpzf+tBaUds=
 *         </Q>
 *         <G>
 *           oe14R2OtyKx+s+60O5BRNMOYpIg2TU/f15N3bsDErKOWtKXeNK9FS7dWStreDxo2
 *           SSgOonqAd4FuJ/4uva7GgNL4ULIqY7E+mW5iwJ7n/WTELh98mEocsLXkNh24HcH4
 *           BZfSCTruuzmCyjdV1KSqX/Eux04HfCWYmdxN3SQ/qqw=
 *         </G>
 *         <Y>
 *           pA5NnZvcd574WRXuOA7ZfC/7Lqt4cB0MRLWtHubtJoVOao9ib5ry4rTk0r6ddnOv
 *           AIGKktutzK3ymvKleS3DOrwZQgJ+/BDWDW8kO9R66o6rdjiSobBi/0c2V1+dkqOg
 *           jFmKz395mvCOZGhC7fqAVhHat2EjGPMfgSZyABa7+1k=
 *         </Y>
 *       </DSAKeyValue>
 *     </KeyValue>
 *   </KeyInfo>
 * </Signature>
 *</Envelope>
 * </code>
 * </pre>
 */
public class SignXMLUtils {

	/**
	 * @param inputFile
	 * @param outputFile
	 * @return
	 */
	public static boolean createSignedFile(String inputFile, String outputFile) {
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, ""); // Compliant
		dbf.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, ""); // compliant
		dbf.setNamespaceAware(true);
		Document doc;
		try {
			doc = dbf.newDocumentBuilder().parse(inputFile);
			createSignedFile(doc, outputFile);
			return true;
		} catch (final Exception e) {
			return false;
		}
	}

	/**
	 * @param doc
	 * @param outputFile
	 * @throws Exception
	 */
	public static void createSignedFile(Document doc, String outputFile) throws Exception {
		// Create a DOM XMLSignatureFactory that will be used to generate the
		// enveloped signature
		final String providerName = System.getProperty("jsr105Provider", XMLDSigRI.class.getName()); //$NON-NLS-1$
		final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM",
				(Provider) Class.forName(providerName).getConstructor().newInstance());
		// Create a Reference to the enveloped document (in this case we are
		// signing the whole document, so a URI of "" signifies that) and
		// also specify the SHA1 digest algorithm and the ENVELOPED Transform.
		final Reference ref = fac.newReference("", //$NON-NLS-1$
				fac.newDigestMethod(DigestMethod.SHA1, null),
				Collections.singletonList(fac.newTransform(Transform.ENVELOPED, (TransformParameterSpec) null)),
				null, null);
		// Create the SignedInfo
		final SignedInfo si = fac.newSignedInfo(
				fac.newCanonicalizationMethod(CanonicalizationMethod.INCLUSIVE_WITH_COMMENTS,
						(C14NMethodParameterSpec) null),
				fac.newSignatureMethod(SignatureMethod.DSA_SHA1, null),
				Collections.singletonList(ref));
		// Create a DSA KeyPair
		final KeyPairGenerator kpg = KeyPairGenerator.getInstance("DSA"); //$NON-NLS-1$
		kpg.initialize(2048);
		final KeyPair kp = kpg.generateKeyPair();
		// Create a KeyValue containing the DSA PublicKey that was generated
		final KeyInfoFactory kif = fac.getKeyInfoFactory();
		final KeyValue kv = kif.newKeyValue(kp.getPublic());
		// Create a KeyInfo and add the KeyValue to it
		final KeyInfo ki = kif.newKeyInfo(Collections.singletonList(kv));
		// Create a DOMSignContext and specify the DSA PrivateKey and
		// location of the resulting XMLSignature's parent element
		final DOMSignContext dsc = new DOMSignContext(kp.getPrivate(), doc.getDocumentElement());
		// Create the XMLSignature (but don't sign it yet)
		final XMLSignature signature = fac.newXMLSignature(si, ki);
		// Marshal, generate (and sign) the enveloped signature
		signature.sign(dsc);
		// output the resulting document
		OutputStream os;
		os = new FileOutputStream(outputFile);
		final TransformerFactory tf = TransformerFactory.newInstance();
		tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, ""); // Compliant
		tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, ""); // compliant
		final Transformer trans = tf.newTransformer();
		trans.transform(new DOMSource(doc), new StreamResult(os));
	}
}
