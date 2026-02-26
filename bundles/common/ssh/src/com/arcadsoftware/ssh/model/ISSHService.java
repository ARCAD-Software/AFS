package com.arcadsoftware.ssh.model;

import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.security.KeyPair;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * This OSGi service store the SSH Keys.
 * 
 * @author ARCAD Software
 */
public interface ISSHService {

	/**
	 * Generate the requested KeyPair.
	 * 
	 * @param sshKeyBeanMap
	 * @return
	 * @throws SSHException
	 */
	public SSHKey create(final BeanMap sshKeyBeanMap) throws SSHException;

	/**
	 * Delete the gien SSH Key.
	 * 
	 * @param sshKey
	 * @return
	 */
	public boolean delete(final SSHKey sshKey);

	/**
	 * Deletes all the files related to an {@link SSHKey}.
	 *
	 * @param key
	 * @throws IOException
	 */
	public void deleteKeyFiles(final SSHKey key) throws IOException;
	
	/**
	 * Load the {@link SSHKey} (ie. general information about a {@link KeyPair}
	 * stored in database)
	 *
	 * @param id
	 * @return the {@link SSHKey} for the given id, or null if not found
	 */
	public SSHKey get(final int id);

	/**
	 * Only useful for RSA keys.
	 *
	 * @param keyPair
	 * @return the length of an RSA key or 256 for an ed25519 key.
	 */
	public int getKeyLength(final KeyPair keyPair);
	
	/**
	 * Return the public key stored int he keystore of this SSHKey.
	 * 
	 * @param sshKey
	 * @return
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	public byte[] getPublicKey(final SSHKey sshKey) throws IOException, GeneralSecurityException;

	/**
	 * Import the uploaded Key 
	 * 
	 * @param sshKeyUpload The key to import.
	 * @return a non null Key declaration.
	 * @throws SSHException
	 */
	public SSHKey importKey(final SSHKeyUpload sshKeyUpload) throws SSHException;
	
	/**
	 * Load the {@link KeyPair} linked to an {@link SSHKey} stored in
	 * database.
	 * 
	 * <p>
	 * Can be an <b>RSA</b> or <b>ed25519</b> key pair.
	 *
	 * @param sshKey
	 * @return a {@link KeyPair}
	 * @throws IOException
	 * @throws GeneralSecurityException
	 */
	public KeyPair loadKeyPair(final SSHKey sshKey) throws IOException, GeneralSecurityException;

	/**
	 * Load the {@link KeyPair} from the given stream source.
	 * database.
	 * 
	 * <p>
	 * Can be an <b>RSA</b> or <b>ed25519</b> key pair.
	 * 
	 * @param input the input stream.
	 * @param passphrase may be null if the stream is not encrypted.
	 * @return a non null KeyPair object.
	 * @throws IOException 
	 * @throws GeneralSecurityException
	 */
	public KeyPair loadKeyPair(final InputStream input, final String passphrase) throws IOException, GeneralSecurityException;

}
