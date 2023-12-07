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
package com.arcadsoftware.server.ssh.services;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermission;
import java.security.GeneralSecurityException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.Security;
import java.security.interfaces.RSAKey;
import java.util.Base64;
import java.util.HashSet;

import org.apache.commons.io.FileUtils;
import org.apache.sshd.common.NamedResource;
import org.apache.sshd.common.config.keys.FilePasswordProvider;
import org.apache.sshd.common.config.keys.writer.openssh.OpenSSHKeyEncryptionContext;
import org.apache.sshd.common.config.keys.writer.openssh.OpenSSHKeyPairResourceWriter;
import org.apache.sshd.common.util.security.SecurityUtils;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.log.LogService;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.ssh.model.SSHException;
import com.arcadsoftware.ssh.model.SSHKey;
import com.arcadsoftware.ssh.model.SSHKeyType;
import com.arcadsoftware.ssh.model.SSHKeyUpload;

import net.i2p.crypto.eddsa.EdDSASecurityProvider;

@Component(service = SSHService.class)
public class SSHService {

	private static final String PRIVATE_KEY_FILE = "private_key";
	private static final String KEYSTORE_DIRECTORY = "./ssh/keystore";
	private static final HashSet<PosixFilePermission> CHMOD_600 = new HashSet<PosixFilePermission>(2);
	
	static {
		CHMOD_600.add(PosixFilePermission.OWNER_READ);
		CHMOD_600.add(PosixFilePermission.OWNER_WRITE);
		if (Security.getProperty(EdDSASecurityProvider.PROVIDER_NAME) == null) {
			Security.addProvider(new EdDSASecurityProvider());
		}
	}

	private File keystoreDirectory;
	private LogService log;

	@Activate
	private void activate() {
		// Invoke this method here to force the loading of EdDSASecurityProviderRegistrar
		// with the correct classloader.
		// Otherwise, the instantiation may take place later, when the context classloader cannot
		// provide the net.i2p.crypto.eddsa.EdDSAKey class.
		SecurityUtils.getKeyPairResourceParser();
		try {
			keystoreDirectory = new File(KEYSTORE_DIRECTORY).getCanonicalFile();
		} catch (final IOException e) {
			if (log != null) {
				log.log(LogService.LOG_ERROR, "Error while revolving SSH keystore", e);
			}
			keystoreDirectory = new File(KEYSTORE_DIRECTORY).getAbsoluteFile();
		}
	}

	@Reference
	private void bindLog(final LogService log) {
		this.log = log;
	}

	private String computeKeyFingerprint(final KeyPair keyPair) throws IOException, GeneralSecurityException {
		final ByteArrayOutputStream publicKeyOutput = new ByteArrayOutputStream();
		OpenSSHKeyPairResourceWriter.INSTANCE.writePublicKey(keyPair, "", publicKeyOutput);
		publicKeyOutput.close();
		final String publicKey = publicKeyOutput.toString(StandardCharsets.UTF_8.name()).split(" ")[1].trim();
		final MessageDigest messageDigest = MessageDigest.getInstance("MD5");
		final byte[] digest = messageDigest.digest(Base64.getDecoder().decode(publicKey));
		final StringBuilder toRet = new StringBuilder();
		for (int i = 0; i < digest.length; i++) {
			if (i != 0) {
				toRet.append(":");
			}
			final String hex = Integer.toHexString(digest[i] & 0xff);
			if (hex.length() == 1) {
				toRet.append("0");
			}
			toRet.append(hex);
		}
		return toRet.toString();
	}

	public SSHKey create(final BeanMap sshKeyBeanMap) throws SSHException {
		final SSHKey newSSHKey = insert(sshKeyBeanMap);
		try {
			generateKeyPair(newSSHKey);
		} catch (IOException | GeneralSecurityException e) {
			delete(newSSHKey);
			throw new SSHException("Error occurred while creation new SSH key", e);
		}
		return newSSHKey;
	}

	public boolean delete(final SSHKey sshKey) {
		if (sshKey.getId() > 0) {
			return getEntity().dataDelete(sshKey.getId(), true);
		}
		return false;
	}

	/**
	 * Deletes all the files related to an {@link SSHKey}.
	 *
	 * @param key
	 * @throws IOException
	 */
	public void deleteKeyFiles(final SSHKey key) throws IOException {
		final File keyDirectory = getSSHKeyDirectory(key);
		if (keyDirectory.isDirectory()) {
			for (final File file : keyDirectory.listFiles()) {
				if (file.isFile() && !file.setWritable(true) && (log != null)) {
					log.log(LogService.LOG_WARNING, String.format("Cannot make file %s writable", file), null);
				}
			}
			FileUtils.deleteDirectory(keyDirectory);
		}
	}

	private void generateKeyPair(final SSHKey sshKey) throws IOException, GeneralSecurityException {
		final SSHKeyType keyType = sshKey.getType();
		final KeyPairGenerator generator = KeyPairGenerator.getInstance(keyType.getAlgorithm());
		if (sshKey.getLength() > 0) {
			generator.initialize(sshKey.getLength());
		}
		final KeyPair keyPair = generator.generateKeyPair();
		final OpenSSHKeyEncryptionContext encryption;
		if (sshKey.isEncrypted()) {
			encryption = null;
		} else {
			encryption = new OpenSSHKeyEncryptionContext();
			encryption.setCipherName("AES");
			encryption.setCipherMode("CTR");
			encryption.setCipherType("256");
			encryption.setPassword(sshKey.getPassphrase());
		}
		final ByteArrayOutputStream privateKeyOutput = new ByteArrayOutputStream();
		OpenSSHKeyPairResourceWriter.INSTANCE.writePrivateKey(keyPair, sshKey.getComment(), encryption, privateKeyOutput);
		privateKeyOutput.close();
		writePrivateKey(sshKey, privateKeyOutput.toByteArray());
		sshKey.setFingerprint(computeKeyFingerprint(keyPair));
		if (sshKey.isEncrypted()) {
			sshKey.setPassphrase(Crypto.encrypt(sshKey.getPassphrase().toCharArray()));
		}
		sshKey.setLength(getKeyLength(keyPair));
		getEntity().dataUpdate(sshKey.getBeanMap());
	}

	/**
	 * Load the {@link SSHKey} (ie. general information about a {@link KeyPair}
	 * stored in database)
	 *
	 * @param id
	 * @return the {@link SSHKey} for the given id, or null if not found
	 */
	public SSHKey get(final int id) {
		final MetaDataEntity e = getEntity();
		if (e != null) {
			final BeanMap b = e.dataSelection(id, null, false);
			if (b != null) {
				return new SSHKey(b);
			}
		}
		return null;
	}

	/**
	 * Only useful for RSA keys.
	 *
	 * @param keyPair
	 * @return the length of an RSA key or 256 for an ed25519 key.
	 */
	public int getKeyLength(final KeyPair keyPair) {
		PrivateKey pk = keyPair.getPrivate();
		if (pk instanceof RSAKey) {
			return ((RSAKey) pk).getModulus().bitLength();
		}
		return 256;
	}

	private MetaDataEntity getEntity() {
		return MetaDataEntity.loadEntity(SSHKey.ENTITY);
	}

	private File getPrivateKeyFile(final SSHKey sshKey) throws IOException {
		final File dir = getSSHKeyDirectory(sshKey);
		if (dir.isDirectory()) {
			for (File f: dir.listFiles()) {
				if (f.isFile()) {
					String name = f.getName();
					if (name.equals("id_rsa") || name.equals(PRIVATE_KEY_FILE)) { //$NON-NLS-1$
						return f;
					}
				}
			}
		}
		throw new IOException(String.format("Private key file for SSH key %d not found", sshKey.getId()));
	}
	
	public byte[] getPublicKey(final SSHKey sshKey) throws IOException, GeneralSecurityException {
		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		OpenSSHKeyPairResourceWriter.INSTANCE.writePublicKey(loadKeyPair(sshKey), sshKey.getComment(), output);
		output.close();
		return output.toByteArray();
	}

	private File getSSHKeyDirectory(final SSHKey key) {
		return new File(keystoreDirectory, "ks" + key.getId());
	}

	public SSHKey importKey(final SSHKeyUpload sshKeyUpload) throws SSHException {
		final KeyPair keyPair;
		final byte[] privateKeyBytes = sshKeyUpload.getPrivateKey().getBytes(StandardCharsets.UTF_8);
		try (final ByteArrayInputStream keyInput = new ByteArrayInputStream(privateKeyBytes)) {
			keyPair = loadKeyPair(keyInput, sshKeyUpload.getPassphrase());
		} catch (IOException | GeneralSecurityException e) {
			throw new SSHException("Error occurred while reading imported SSH key: " + e, e);
		}
		final SSHKey tempSSHKey = new SSHKey();
		tempSSHKey.setName(sshKeyUpload.getName());
		tempSSHKey.setType(SSHKeyType.fromAlgorithm(keyPair.getPrivate().getAlgorithm()));
		tempSSHKey.setLength(getKeyLength(keyPair));
		if (tempSSHKey.getType() == SSHKeyType.UNKNOWN) {
			throw new SSHException(String.format("%s key type is not supported", keyPair.getPrivate().getAlgorithm()));
		} else if (tempSSHKey.getType() == SSHKeyType.RSA && tempSSHKey.getLength() < 4096) {
			throw new SSHException(String.format("RSA key length is too short (%d); it must be 4096", tempSSHKey.getLength()));
		}
		try {
			tempSSHKey.setFingerprint(computeKeyFingerprint(keyPair));
		} catch (IOException | GeneralSecurityException e) {
			throw new SSHException("Error occurred while importing SSH key", e);
		}
		if (sshKeyUpload.getPassphrase() != null) {
			tempSSHKey.setPassphrase(Crypto.encrypt(sshKeyUpload.getPassphrase().toCharArray()));
		}
		final SSHKey importedSSHKey = new SSHKey(getEntity().dataCreate(tempSSHKey.getBeanMap()));
		try {
			writePrivateKey(importedSSHKey, privateKeyBytes);
			return importedSSHKey;
		} catch (IOException | GeneralSecurityException e) {
			if (importedSSHKey.getId() > 0) {
				delete(importedSSHKey);
			}
			throw new SSHException("Error occurred while importing SSH key", e);
		}
	}

	private SSHKey insert(final BeanMap sshKeyBeanMap) throws SSHException {
		final SSHKey tempSSHKey = new SSHKey(sshKeyBeanMap);
		if (tempSSHKey.getType() == SSHKeyType.UNKNOWN) {
			throw new SSHException(String.format("SSH key type %s is unknown", tempSSHKey.getAlgorithm()));
		}
		tempSSHKey.setLength(tempSSHKey.getType().getLength());
		MetaDataEntity e = getEntity();
		if (e != null) {
			BeanMap b = e.dataCreate(sshKeyBeanMap);
			if ((b != null) && (b.getId() > 0)) {
				return new SSHKey(b);
			}
		}
		throw new SSHException("Could not insert new SSH key in database");
	}

	/**
	 * Load the {@link KeyPair} linked to an {@link SSHKey} stored in
	 * database.<br />
	 * Can be an <b>RSA</b> or <b>ed25519</b> key pair.
	 *
	 * @param sshKey
	 * @return a {@link KeyPair}
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	public KeyPair loadKeyPair(final SSHKey sshKey) throws IOException, GeneralSecurityException {
		final File keyFile = getPrivateKeyFile(sshKey);
		try (InputStream input = new BufferedInputStream(new FileInputStream(keyFile))) {
			return loadKeyPair(sshKey, input);
		}
	}

	private KeyPair loadKeyPair(final SSHKey sshKey, final InputStream input)
			throws IOException, GeneralSecurityException {
		final String passphrase;
		if (sshKey.isEncrypted()) {
			passphrase = new String(Crypto.decrypt(sshKey.getPassphrase()));
		} else {
			passphrase = null;
		}
		return loadKeyPair(input, passphrase);
	}

	public KeyPair loadKeyPair(final InputStream input, final String passphrase)
			throws IOException, GeneralSecurityException {
		final FilePasswordProvider password;
		if (passphrase != null && !passphrase.isEmpty()) {
			password = FilePasswordProvider.of(passphrase);
		} else {
			password = null;
		}
		for (KeyPair keyPair : SecurityUtils.loadKeyPairIdentities(null, NamedResource.ofName(PRIVATE_KEY_FILE), input, password)) {
			return keyPair;
		}
		throw new IOException("Cannot load invalid private key");
	}

	private void writePrivateKey(final SSHKey sshKey, final byte[] privateKeyBytes)
			throws IOException, GeneralSecurityException {
		final File keyDirectory = getSSHKeyDirectory(sshKey);
		keyDirectory.mkdirs();
		final File keyFile = new File(keyDirectory, PRIVATE_KEY_FILE);
		Files.write(keyFile.toPath(), privateKeyBytes);
		try {
			Files.setPosixFilePermissions(keyFile.toPath(), CHMOD_600);
		} catch (final UnsupportedOperationException e) {
			if (log != null) {
				if (!keyFile.setReadable(true, true)) {
					log.log(LogService.LOG_DEBUG, "Unable to change file mode read: " + keyFile);
				}
				if (!keyFile.setWritable(false, false)) {
					log.log(LogService.LOG_DEBUG, "Unable to change file mode write: " + keyFile);
				}
				if (!keyFile.setExecutable(false, false)) {
					log.log(LogService.LOG_DEBUG, "Unable to change file mode execute: " + keyFile);
				}
			}
		}
	}
}