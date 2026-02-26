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
package com.arcadsoftware.server.ssh.internal;

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
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Security;
import java.security.interfaces.RSAKey;
import java.util.Base64;
import java.util.Dictionary;
import java.util.HashSet;
import java.util.Iterator;

import org.apache.sshd.common.NamedResource;
import org.apache.sshd.common.config.keys.FilePasswordProvider;
import org.apache.sshd.common.config.keys.writer.openssh.OpenSSHKeyEncryptionContext;
import org.apache.sshd.common.config.keys.writer.openssh.OpenSSHKeyPairResourceWriter;
import org.apache.sshd.common.util.security.SecurityUtils;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.ssh.model.ISSHService;
import com.arcadsoftware.ssh.model.SSHException;
import com.arcadsoftware.ssh.model.SSHKey;
import com.arcadsoftware.ssh.model.SSHKeyType;
import com.arcadsoftware.ssh.model.SSHKeyUpload;

public class SSHService implements ISSHService {

	private static final String PRIVATE_KEY_FILE = "private_key"; //$NON-NLS-1$
	private static final String PROP_KEYSTOREPATH = "ssh.keystore.path"; //$NON-NLS-1$
	private static final HashSet<PosixFilePermission> CHMOD_600 = new HashSet<>(2);

	static {
		CHMOD_600.add(PosixFilePermission.OWNER_READ);
		CHMOD_600.add(PosixFilePermission.OWNER_WRITE);
		// Blindage: this should have been ndone in the initialization of the Crypt bundle.
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			try {
				Security.addProvider(new BouncyCastleProvider());
			} catch (Exception e) {}
		}
	}

	private final Activator activator;
	private final File keystoreDirectory;

	public SSHService(Activator activator) {
		super();
		this.activator = activator;
		// The Keystore folder path may be declared in the OSGi configuration, with a lot of fallback...
		File folderPath = null;
		// 1. use the OSGi Configuration...
		Dictionary<String, Object> conf = activator.getConfiguration(activator.getContext().getBundle().getSymbolicName());
		if (conf != null) {
			Object o = conf.get(PROP_KEYSTOREPATH);
			if ((o instanceof String s) && !s.isBlank()) {
				folderPath = new File(s);
			}
		}
		// 2. use the system property...
		if (folderPath == null) {
			String s = System.getProperty("com.arcadsoftware.ssh.keypath"); //$NON-NLS-1$
			if (s != null) {
				folderPath = new File(s);
			}
		}
		// 3. use the legacy ./ssh folder but only if is exists !
		if (folderPath == null) {
			File dkd = new File("./ssh/keystore"); //$NON-NLS-1$
			if (dkd.isDirectory()) {
				folderPath = dkd;
			}
		}
		// 4. Use the "new" default folder !
		if (folderPath == null) {
			folderPath = new File("./files/ssh/keystore"); //$NON-NLS-1$
		}
		keystoreDirectory = folderPath;
		keystoreDirectory.mkdirs();
	}

	private String computeKeyFingerprint(final KeyPair keyPair) throws IOException, GeneralSecurityException {
		final ByteArrayOutputStream publicKeyOutput = new ByteArrayOutputStream();
		OpenSSHKeyPairResourceWriter.INSTANCE.writePublicKey(keyPair, "", publicKeyOutput); //$NON-NLS-1$
		publicKeyOutput.close();
		final String publicKey = publicKeyOutput.toString(StandardCharsets.UTF_8.name()).split(" ")[1].trim(); //$NON-NLS-1$
		final MessageDigest messageDigest = MessageDigest.getInstance("MD5"); //$NON-NLS-1$
		final byte[] digest = messageDigest.digest(Base64.getDecoder().decode(publicKey));
		final StringBuilder toRet = new StringBuilder();
		for (int i = 0; i < digest.length; i++) {
			if (i != 0) {
				toRet.append(':');
			}
			final String hex = Integer.toHexString(digest[i] & 0xff);
			if (hex.length() == 1) {
				toRet.append('0');
			}
			toRet.append(hex);
		}
		return toRet.toString();
	}

	@Override
	public SSHKey create(final BeanMap sshKeyBeanMap) throws SSHException {
		// Pretest of the key type...
		if (new SSHKey(sshKeyBeanMap).getType() == SSHKeyType.UNKNOWN) {
			throw new SSHException("SSH key type \"%s\" is unknown".formatted(sshKeyBeanMap.get(SSHKey.TYPE)));
		}
		final SSHKey newSSHKey = new SSHKey(createKey(sshKeyBeanMap));
		try {
			generateKeyPair(newSSHKey);
		} catch (IOException | GeneralSecurityException e) {
			// "manual" rollback...
			delete(newSSHKey);
			throw new SSHException("Error occurred while creating new SSH key: " + e, e);
		}
		return newSSHKey;
	}

	@Override
	public boolean delete(final SSHKey sshKey) {
		if (sshKey.getId() > 0) {
			return getEntity().dataDelete(sshKey.getId(), true);
		}
		return false;
	}

	@Override
	public void deleteKeyFiles(final SSHKey key) throws IOException {
		final File keyDirectory = getSSHKeyDirectory(key);
		if (keyDirectory.isDirectory()) {
			for (final File file : keyDirectory.listFiles()) {
				if (file.isFile()) {
					if (!file.setWritable(true)) {
						activator.warn("Cannot make file \"{}\" writable", file);
					} else if (!file.delete()) {
						activator.warn("Unable to delete file \"{}\".", file);
					}
				}
			}
			if (!keyDirectory.delete()) {
				activator.warn("Unable to delete directory \"{}\".", keyDirectory);
			}
		}
	}

	private void generateKeyPair(final SSHKey sshKey) throws IOException, GeneralSecurityException {
		final SSHKeyType keyType = sshKey.getType();
		KeyPairGenerator generator;
		try {
			generator = KeyPairGenerator.getInstance(keyType.getAlgorithm(), keyType.getProvider());
		} catch (NoSuchAlgorithmException e) {
			generator = KeyPairGenerator.getInstance(keyType.getAlgorithm());
		}
		if (sshKey.getLength() > 0) {
			generator.initialize(sshKey.getLength());
		}
		final KeyPair keyPair = generator.generateKeyPair();
		final OpenSSHKeyEncryptionContext encryption;
		if (sshKey.isEncrypted()) {
			encryption = null;
		} else {
			encryption = new OpenSSHKeyEncryptionContext();
			encryption.setCipherName("AES"); //$NON-NLS-1$
			encryption.setCipherMode("CTR"); //$NON-NLS-1$
			encryption.setCipherType("256"); //$NON-NLS-1$
			encryption.setPassword(sshKey.getPassphrase());
		}
		final ByteArrayOutputStream privateKeyOutput = new ByteArrayOutputStream();
		OpenSSHKeyPairResourceWriter.INSTANCE.writePrivateKey(keyPair, sshKey.getComment(), encryption,
				privateKeyOutput);
		privateKeyOutput.close();
		writePrivateKey(sshKey, privateKeyOutput.toByteArray());
		sshKey.setFingerprint(computeKeyFingerprint(keyPair));
		if (sshKey.isEncrypted()) {
			sshKey.setPassphrase(Crypto.encrypt(sshKey.getPassphrase().toCharArray()));
		}
		sshKey.setLength(getKeyLength(keyPair));
		BeanMap b = sshKey.getBeanMap().clone();
		// The name of the SSH key can not be changed trhough this process...
		b.remove("name"); //$NON-NLS-1$
		getEntity().dataUpdate(b);
	}

	@Override
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

	@Override
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
			for (File f : dir.listFiles()) {
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

	@Override
	public byte[] getPublicKey(final SSHKey sshKey) throws IOException, GeneralSecurityException {
		try (final ByteArrayOutputStream output = new ByteArrayOutputStream()) {
			OpenSSHKeyPairResourceWriter.INSTANCE.writePublicKey(loadKeyPair(sshKey), sshKey.getComment(), output);
			return output.toByteArray();
		}
	}

	private File getSSHKeyDirectory(final SSHKey key) {
		return new File(keystoreDirectory, "ks" + key.getId()); //$NON-NLS-1$
	}

	@Override
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
			throw new SSHException(keyPair.getPrivate().getAlgorithm() + " key type is not supported");
		} 
		if ((tempSSHKey.getType() == SSHKeyType.RSA) && (tempSSHKey.getLength() < 4096)) {
			throw new SSHException(String.format("RSA key length is too short (%d); it must be 4096", tempSSHKey.getLength()));
		}
		try {
			tempSSHKey.setFingerprint(computeKeyFingerprint(keyPair));
		} catch (IOException | GeneralSecurityException e) {
			throw new SSHException("Error occurred while importing SSH key: " + e.getLocalizedMessage(), e);
		}
		if (sshKeyUpload.getPassphrase() != null) {
			tempSSHKey.setPassphrase(Crypto.encrypt(sshKeyUpload.getPassphrase().toCharArray()));
		}
		final SSHKey importedSSHKey = new SSHKey(createKey(tempSSHKey.getBeanMap()));
		try {
			writePrivateKey(importedSSHKey, privateKeyBytes);
			return importedSSHKey;
		} catch (IOException e) {
			if (importedSSHKey.getId() > 0) {
				delete(importedSSHKey);
			}
			throw new SSHException("Error occurred while importing SSH key: " + e.getLocalizedMessage(), e);
		}
	}

	private void writePrivateKey(final SSHKey sshKey, final byte[] privateKeyBytes) throws IOException {
		final File keyDirectory = getSSHKeyDirectory(sshKey);
		keyDirectory.mkdirs();
		final File keyFile = new File(keyDirectory, PRIVATE_KEY_FILE);
		Files.write(keyFile.toPath(), privateKeyBytes);
		try {
			Files.setPosixFilePermissions(keyFile.toPath(), CHMOD_600);
		} catch (final Exception e) {
			activator.debug("Unable to change file \"{}\" access mode: ", keyFile, e.getLocalizedMessage(), e);
		}
	}

	private synchronized BeanMap createKey(BeanMap key) throws SSHException {
		MetaDataEntity e = getEntity();
		if (e != null) {
			String name = key.getString("name", ""); //$NON-NLS-1$ //$NON-NLS-2$
			if (name.isBlank()) {
				throw new SSHException("Could not add new SSH key in database, the name must not be empty.");
			}
			if (e.dataCount(false, "name", name) > 0) { //$NON-NLS-1$
				throw new SSHException("Could not add new SSH key in database, the name \"%s\" is already used by another key.".formatted(name));
			}
			BeanMap b = e.dataCreate(key);
			if ((b != null) && (b.getId() > 0)) {
				return b;
			}
			throw new SSHException("Could not add new SSH key in database, due to an violation of a constraint on the key data.");
		}
		throw new SSHException("Could not add new SSH key, the database is not accessible.");
	}

	@Override
	public KeyPair loadKeyPair(final SSHKey sshKey) throws IOException, GeneralSecurityException {
		final File keyFile = getPrivateKeyFile(sshKey);
		try (InputStream input = new BufferedInputStream(new FileInputStream(keyFile))) {
			return loadKeyPair(sshKey, input);
		}
	}

	private KeyPair loadKeyPair(final SSHKey sshKey, final InputStream input) throws IOException, GeneralSecurityException {
		if (sshKey.isEncrypted()) {
			return loadKeyPair(input, new String(Crypto.decrypt(sshKey.getPassphrase())));
		}
		return loadKeyPair(input, null);
	}

	@Override
	public KeyPair loadKeyPair(final InputStream input, final String passphrase)
			throws IOException, GeneralSecurityException {
		final FilePasswordProvider password;
		if ((passphrase != null) && !passphrase.isEmpty()) {
			password = FilePasswordProvider.of(passphrase);
		} else {
			password = null;
		}
		final Iterator<KeyPair> identities = SecurityUtils.loadKeyPairIdentities(null, NamedResource.ofName(PRIVATE_KEY_FILE), input, password).iterator();
		if (identities.hasNext()) {
			return identities.next();
		}
		throw new IOException("Cannot load invalid private key");
	}
}