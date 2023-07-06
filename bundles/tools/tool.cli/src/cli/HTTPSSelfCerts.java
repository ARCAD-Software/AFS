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
package cli;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.Key;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.PrivateKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.x500.X500NameBuilder;
import org.bouncycastle.asn1.x500.style.BCStyle;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.KeyPurposeId;
import org.bouncycastle.asn1.x509.KeyUsage;
import org.bouncycastle.asn1.x509.SubjectKeyIdentifier;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.bc.BcX509ExtensionUtils;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.InternetAddressUtils;
import com.arcadsoftware.crypt.RandomGenerator;
import com.arcadsoftware.tool.cli.Command;

public class HTTPSSelfCerts extends Command {

	public static void main(String[] args) {
		new HTTPSSelfCerts(args).exec();
	}

	public HTTPSSelfCerts() {
		super();
	}

	public HTTPSSelfCerts(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		// Register the BouncyCastle provider.
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
		String ksp = getArgumentValue(new String[] {"-ks", "-keystore"}, (String) null);
		File ks;
		if (ksp == null) {
			ks = new File(getHomeDirectory(), "security/keystore.p12");
		} else {
			ks = new File(ksp);
		}
		String kst = getArgumentValue(new String[] {"-kst", "-keystoretype", "-keystore.type"}, "PKCS12");
		char[] kpwd = getArgumentValue(new String[] {"-ksp", "-keystorepwd", "-keystore.password"}, (char[]) null);
		if (kpwd == null) {
			if (isArgument("-rdp", "-randompwd")) {
				kpwd = RandomGenerator.complexRandonPassword();
			} else {
				kpwd = readSecret("Enter the password used for the Key Store: ");
				if (kpwd == null) {
					printError("ERROR: no password provided !");
					return ERROR_MISSING_PARAMETER;
				}
				char[] pwd2 = readSecret("Confirm this password: ");
				if (!Arrays.equals(kpwd, pwd2)) {
					printError("The given passwords do not matches."); 
					return ERROR_WRONG_PARAMETER;
				}
			}
		}
		// Open or create the key Store...
		KeyStore store = null;
		try {
			store = KeyStore.getInstance(kst);
		} catch (Exception e) {
			printError("ERROR: Unsupported Key store type: " + kst);
			return ERROR_WRONG_PARAMETER;
		}
		if (ks.isFile() && (ks.length() > 0)) {
			try {
				println("Load the Key Store: " + ks.getCanonicalPath());
			} catch (IOException e1) {
				println("Load the Key Store: " + ks.getAbsolutePath());
			}
			try (FileInputStream is = new FileInputStream(ks)) { 
				store.load(is, kpwd);
			} catch (Exception e) {
				printError("Error: Unable to open the key store: " + e.getLocalizedMessage());
				return ERROR_WRONG_PARAMETER;
			}
		} else {
			try {
				println("Create a new Key Store: " + ks.getCanonicalPath());
			} catch (IOException e1) {
				println("Create a new Key Store: " + ks.getAbsolutePath());
			}
			ks.getParentFile().mkdirs();
			try {
				ks.createNewFile();
			} catch (Exception e) {
				printError("Error: Unable to create the Key Store file (check folder acces rights): " + e.getLocalizedMessage());
				return ERROR_WRONG_PARAMETER;
			}
			try {
				store.load(null, kpwd);
			} catch (Exception e) {
				printError("Error: Unable to initialize the Key Store: " + e.getLocalizedMessage());
				return ERROR_WRONG_PARAMETER;
			}
		}
		// Generate the private key...
		int keySize = getArgumentValue(new String[] {"-keysize", "-size"}, 2048); //$NON-NLS-1$ //$NON-NLS-2$
		if (keySize < 1024) {
			keySize = 1024;
		}
		String keya = getArgumentValue(new String[] {"-key", "-keyalgorithm"}, "RSA"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		KeyPair key = null;
		try {
			KeyPairGenerator kpGen = KeyPairGenerator.getInstance(keya, BouncyCastleProvider.PROVIDER_NAME);
			kpGen.initialize(keySize, new SecureRandom());
			key = kpGen.generateKeyPair();
		} catch (Exception e) {
			printError("Error: Unable to generated the private Key: " + e.getLocalizedMessage());
			return ERROR_WRONG_PARAMETER;
		}
		// Generate the certificate...
		// FIXME Recupérer le domain name dans la configuration !!!
		String domain = "localhost"; //$NON-NLS-1$
		try {
			domain = InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {}
		domain = getArgumentValue(new String[] {"-dns", "-domainname"}, domain); //$NON-NLS-1$ //$NON-NLS-2$
		// TODO Allow multiple DNS names...
		String subject = getArgumentValue("-issuer", "Arcad Software"); //$NON-NLS-1$ //$NON-NLS-2$
		String organization = getArgumentValue(new String[] {"-org", "-organization"}, subject); //$NON-NLS-1$ //$NON-NLS-2$
		String organizationUnit = getArgumentValue(new String[] {"-ou", "-organizationunit"}, "none"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		String locality = getArgumentValue(new String[] {"-loc", "-locality"}, "none"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		String country = getArgumentValue(new String[] {"-c", "-country"}, Locale.getDefault().getCountry()); //$NON-NLS-1$ //$NON-NLS-2$
		String state = getArgumentValue(new String[] {"-st", "-state"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
		String email = getArgumentValue(new String[] {"-em", "-email"}, "none@email.com"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		String signatureAlgorithm = getArgumentValue(new String[] {"-sign", "-signaturealgorithm"}, "SHA256WithRSAEncryption"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		String l = getArgumentValue(new String[] {"-l", "-limit"}, "3650").toLowerCase(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		long limit = 3650;
		long f = 1l;
		if (!l.isEmpty()) {
			if (l.charAt(l.length() - 1) == 'w') {
				f = 7l;
				l = l.substring(0, l.length() -2).trim();
			} else if (l.charAt(l.length() - 1) == 'm') {
					f = 31l;
					l = l.substring(0, l.length() -2).trim();
			} else if (l.charAt(l.length() - 1) == 'y') {
				f = 365l;
				l = l.substring(0, l.length() -2).trim();
			}
			try {
				limit = Long.parseLong(l) * f;
			} catch (NumberFormatException e) {}
		}
		X509Certificate cert = null;
		try {
			cert = generateSelfSigned(key, domain, subject, organization, organizationUnit, locality, country, state, email, limit, signatureAlgorithm);
		} catch (Exception e) {
			printError("Error: Unable to generated the Server certificate: " + e.getLocalizedMessage());
			return ERROR_WRONG_PARAMETER;
		}
		// Save the new key...
		String keyAlias = getArgumentValue(new String[] {"-alias", "-keyalias"}, "serverkey");
		char[] keypwd = getArgumentValue(new String[] {"-keypwd", "-keypassword"}, (char[]) null);
		if (keypwd == null) {
			if (isArgument("-rdp", "-randompwd")) {
				keypwd = RandomGenerator.complexRandonPassword();
			} else {
				keypwd = readSecret("Enter the password used for the Key itself:\n(a password different from the key store is recommended.) ");
				if (keypwd == null) {
					printError("ERROR: no password provided !");
					return ERROR_MISSING_PARAMETER;
				}
				char[] pwd2 = readSecret("Confirm this password: ");
				if (!Arrays.equals(keypwd, pwd2)) {
					printError("The given passwords do not matches."); 
					return ERROR_WRONG_PARAMETER;
				}
			}
		}
		try {
			store.setKeyEntry(keyAlias, key.getPrivate(), keypwd, new Certificate[] {cert});
		} catch (Exception e) {
			printError("Error: Unable to record the Server certificate: " + e.getLocalizedMessage());
			return ERROR_WRONG_PARAMETER;
		}
		try (FileOutputStream os = new FileOutputStream(ks)) {
			store.store(os, kpwd);
		} catch (Exception e) {
			printError("Error: Unable to save the Key Store file: " + e.getLocalizedMessage());
			return ERROR_WRONG_PARAMETER;
		}
		println("Private key and self signed certificate generated.");
		// Generate a truststore (with public certificate) for the clients...
		if (!isArgument("-nct", "-noclientstore")) { //$NON-NLS-1$ //$NON-NLS-2$
			String tsp = getArgumentValue(new String[] {"-ts", "-truststore"}, (String) null);
			File ts;
			if (tsp == null) {
				ts = new File(ks.getParentFile(), "client_truststore" + ks.getName().substring(ks.getName().lastIndexOf('.')));
			} else {
				ts = new File(tsp);
			}
			String tst = getArgumentValue(new String[] {"-tst", "-truststoretype", "-truststore.type"}, kst);
			char[] tpwd = getArgumentValue(new String[] {"-tsp", "-truststorepwd", "-truststore.password"}, (char[]) null);
			if (tpwd == null) {
				if (isArgument("-rdp", "-randompwd")) {
					tpwd = RandomGenerator.complexRandonPassword();
					println("The Client Trust Store password is: " + new String(tpwd));
				} else {
					tpwd = readSecret("Enter the password used for the client trust Store:\n(a password different from the key store is recommended) ");
					if (tpwd == null) {
						printError("ERROR: no password provided !");
						return ERROR_MISSING_PARAMETER;
					}
					char[] pwd2 = readSecret("Confirm this password: ");
					if (!Arrays.equals(tpwd, pwd2)) {
						printError("The given passwords do not matches."); 
						return ERROR_WRONG_PARAMETER;
					}
				}
			}
			KeyStore cstore = null;
			try {
				cstore = KeyStore.getInstance(tst);
			} catch (Exception e) {
				printError("ERROR: Unsupported trust store type: " + tst);
				return ERROR_WRONG_PARAMETER;
			}
			if (ts.isFile() && (ts.length() > 0)) {
				try {
					println("Load the Trust Store: " + ts.getCanonicalPath());
				} catch (IOException e1) {
					println("Load the Trust Store: " + ts.getAbsolutePath());
				}
				try (FileInputStream is = new FileInputStream(ts)) { 
					cstore.load(is, tpwd);
				} catch (Exception e) {
					printError("Error: Unable to open the trust store: " + e.getLocalizedMessage());
					return ERROR_WRONG_PARAMETER;
				}
			} else {
				try {
					println("Create a new Trust Store: " + ts.getCanonicalPath());
				} catch (IOException e1) {
					println("Create a new Trust Store: " + ts.getAbsolutePath());
				}
				ts.getParentFile().mkdirs();
				try {
					ts.createNewFile();
				} catch (Exception e) {
					printError("Error: Unable to create the Trust Store file (check folder acces rights): " + e.getLocalizedMessage());
					return ERROR_WRONG_PARAMETER;
				}
				try {
					cstore.load(null, tpwd);
				} catch (Exception e) {
					printError("Error: Unable to initialize the Trust Store: " + e.getLocalizedMessage());
					return ERROR_WRONG_PARAMETER;
				}
			}
			try {
				cstore.setCertificateEntry(domain, cert);
			} catch (KeyStoreException e) {
				printError("Error: Unable to add the Certificate in the Trust Store: " + e.getLocalizedMessage());
				return ERROR_WRONG_PARAMETER;
			}
			try (FileOutputStream os = new FileOutputStream(ts)) {
				cstore.store(os, tpwd);
			} catch (Exception e) {
				printError("Error: Unable to save the Trust Store: " + e.getLocalizedMessage());
				return ERROR_WRONG_PARAMETER;
			}
		}
		// Update the configuration...
		Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.server.restful"); //$NON-NLS-1$
		props.put("keystore", ks.getAbsolutePath()); //$NON-NLS-1$;
		props.put("keystorepwd", Crypto.encrypt(kpwd)); //$NON-NLS-1$;
		props.put("keypwd", Crypto.encrypt(keypwd)); //$NON-NLS-1$;
		props.put("keytype", kst); //$NON-NLS-1$
		props.put("keyalias", keyAlias); //$NON-NLS-1$;
		saveOSGiConfiguration();
		println("Configuration updated.");
		println("The URL from the Client part must be starting with https://%s to validate this certificate.", domain);
		return 0;
	}
	
	private X509Certificate generateSelfSigned(KeyPair pair, String domain, String subject, String organization, 
			String organizationUnit, String locality, String country, String state, String email, long limitDays, String signatureAlgorithm) throws Exception {
		X509v3CertificateBuilder generator;
		X500NameBuilder issuerBuilder = new X500NameBuilder(BCStyle.INSTANCE);
		X500NameBuilder subjectBuilder = new X500NameBuilder(BCStyle.INSTANCE);
		subjectBuilder.addRDN(BCStyle.CN, domain);
	    issuerBuilder.addRDN(BCStyle.CN, subject);
	    issuerBuilder.addRDN(BCStyle.O, organization);
	    subjectBuilder.addRDN(BCStyle.O, organization);
	    if (organizationUnit != null) {
	    	issuerBuilder.addRDN(BCStyle.OU, organizationUnit);
	    	subjectBuilder.addRDN(BCStyle.OU, organizationUnit);
	    }
	    if (locality != null) {
	    	issuerBuilder.addRDN(BCStyle.L, locality);
	    	subjectBuilder.addRDN(BCStyle.L, locality);
	    }
	    if (country != null) {
	    	issuerBuilder.addRDN(BCStyle.C, country);
	    	subjectBuilder.addRDN(BCStyle.C, country);
	    }
	    if (state != null) {
	    	issuerBuilder.addRDN(BCStyle.ST, state);
	    	subjectBuilder.addRDN(BCStyle.ST, state);
	    }
	    if (email != null) {
	    	issuerBuilder.addRDN(BCStyle.E, email);
	    	subjectBuilder.addRDN(BCStyle.E, email);
	    }
		try (ASN1InputStream is = new ASN1InputStream(pair.getPublic().getEncoded())) {
			SubjectPublicKeyInfo publicKeyinfo = SubjectPublicKeyInfo.getInstance(is.readObject());
			generator = new X509v3CertificateBuilder(issuerBuilder.build(), //
					BigInteger.valueOf(System.currentTimeMillis()), // = serial
					new Date(System.currentTimeMillis() - 360000), //
					new Date(System.currentTimeMillis() + (86400000l * limitDays)), // 
					subjectBuilder.build(), //$NON-NLS-1$
					publicKeyinfo);
		}
		// Si le domain est une addresse IP alors il faut ajouter un SAN = IP:x.x.x.x
		if (InternetAddressUtils.isIPAddress(domain)) {
			generator.addExtension(Extension.subjectAlternativeName, false, new DERSequence(new GeneralName(GeneralName.iPAddress, domain)));
		} else {
			generator.addExtension(Extension.subjectAlternativeName, false, new DERSequence(new GeneralName(GeneralName.dNSName, domain)));
		}
		generator.addExtension(Extension.subjectKeyIdentifier, false, createSubjectKeyIdentifier(pair.getPublic()));
		generator.addExtension(Extension.basicConstraints, true, new BasicConstraints(true));
		KeyUsage usage = new KeyUsage(KeyUsage.keyCertSign | //
				KeyUsage.digitalSignature | //
				KeyUsage.keyEncipherment | //
				KeyUsage.dataEncipherment | //
				KeyUsage.cRLSign);
		generator.addExtension(Extension.keyUsage, false, usage);
		ASN1EncodableVector purposes = new ASN1EncodableVector();
		purposes.add(KeyPurposeId.id_kp_serverAuth);
		purposes.add(KeyPurposeId.id_kp_clientAuth);
		purposes.add(KeyPurposeId.anyExtendedKeyUsage);
		generator.addExtension(Extension.extendedKeyUsage, false, new DERSequence(purposes));
		return new JcaX509CertificateConverter() //
				.setProvider(BouncyCastleProvider.PROVIDER_NAME) //
				.getCertificate(generator.build(getContentSigner(pair.getPrivate(), signatureAlgorithm))); 
	}
	
	private SubjectKeyIdentifier createSubjectKeyIdentifier(Key key) throws Exception {
		try (ASN1InputStream is =  new ASN1InputStream(new ByteArrayInputStream(key.getEncoded()))) {
			ASN1Sequence seq = (ASN1Sequence) is.readObject();
			SubjectPublicKeyInfo info = SubjectPublicKeyInfo.getInstance(seq);
			return new BcX509ExtensionUtils().createSubjectKeyIdentifier(info);
		}
	}

	private ContentSigner getContentSigner(PrivateKey key, String signatureAlgorithm) throws OperatorCreationException {
		return new JcaContentSignerBuilder(signatureAlgorithm).setProvider(BouncyCastleProvider.PROVIDER_NAME).build(key);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "selfcerts"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to generate and configure self signed certificates for the HTTPS Server used by the application.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-ks|-keystore <path>]", "Define the path to the key store. If not defined the default value ./security/keystore.p12 will be used.");
		result.put("[-kst|-keystoretype <type>]", "Define the type of key store used. Default type is PKCS12.");
		result.put("[-ksp|-keystorepwd <password>]", "Define the paswword used for the key store.");
		result.put("[-rdp|-randompwd]", "If a password is not defined, instead of asking for it in the console the password will be randomly generated. Note that this option is only applicable with a new Key Store.");
		result.put("[-alias|-keyalias <alias>]", "Define the key alias in the Key Store, the default value is \"serverkey\". Note that this alias must be unique in the Key Store.");
		result.put("[-keypwd|-keypassword <password>]", "Define the password of the Key in the Key Store.");
		result.put("[-size|-keysize <int>]", "Define the private key size, the default value is 2048.");
		result.put("[-key|-keyalgorithm <name>]", "Define the private key algorithm, the default value is RSA.");
		result.put("[-dns|-domainname <dns>]", "This parameter corresponds to the domain name of this server, as the client has access to it. The default value is the actual DNS name of this host.");
		result.put("[-issuer <name>]", "Define the certificate Issuer name included into the server Certificate.");
		result.put("[-org|-organization <name>]", "Define the organization name included into the server Certificate.");
		result.put("[-ou|-organizationunit <name>]", "Define the organization unit name included into the server Certificate.");
		result.put("[-loc|-locality <locality>]", "Define the locality name included into the server Certificate.");
		result.put("[-c|-country <code>]", "Define the country code name included into the server Certificate.");
		result.put("[-st|-state <state>]", "Define the sate code name included into the server Certificate.");
		result.put("[-em|-email <address>]", "Define the contact email address included into the server Certificate.");
		result.put("[-sign|-signaturealgorithm <name>]", "Define the name of the algorithm used to generate the signature of the certificate. The default value is SHA256WithRSAEncryption.");
		result.put("[-l|-limit <limit>]", "Define the certificate limitation of validity, this value is exprimed in days, or with w, m or y suffis, resp. in weeks, month or years, the default value is 10y.");
		result.put("[-nct|-noclientstore]", "if present this option diasble the generation of a Trust Store for the usage of the clients of this server.");
		result.put("[-ts|-truststore <path>]", "Define the path to the client trust store. If not defined the default value ./security/clienttruststore.p12 will be used.");
		result.put("[-tst|-truststoretype <type>]", "Define the type of client trust store used. Default type is PKCS12.");
		result.put("[-tsp|-truststorepwd <password>]", "Define the password used for the client trust store.");
		return result;
	}

}
