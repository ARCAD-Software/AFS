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
package com.arcadsoftware.afs.client.core.security;

import java.io.IOException;

import org.eclipse.jface.preference.IPersistentPreferenceStore;

import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.crypt.Crypto;

public abstract class PreferenceBasedTrustStoreProvider implements ITrustStoreProvider {

	private static final String KEYSTORE_PASSWORD = "keystore.password"; //$NON-NLS-1$
	private static final String KEYSTORE_PATH = "keystore.path"; //$NON-NLS-1$
	private static final String TRUSTSTORE_PASSWORD = "truststore.password"; //$NON-NLS-1$
	private static final String TRUSTSTORE_PATH = "truststore.path"; //$NON-NLS-1$
	private final IPersistentPreferenceStore store; 

	public PreferenceBasedTrustStoreProvider() {
		store = getPreferenceStore();
		store.setDefault(KEYSTORE_PATH, ""); //$NON-NLS-1$
		store.setDefault(TRUSTSTORE_PATH, ""); //$NON-NLS-1$
		store.setDefault(KEYSTORE_PASSWORD, ""); //$NON-NLS-1$
		store.setDefault(TRUSTSTORE_PASSWORD, ""); //$NON-NLS-1$
	}
	
	protected abstract IPersistentPreferenceStore getPreferenceStore();
	
	protected void log(String message, Exception e) {
		Activator.getDefault().log(message, e);
	}
	
	@Override
	public String getTrustStorePath() {		
		return store.getString(TRUSTSTORE_PATH);
	}

	@Override
	public char[] getTrustStorePassword() {
		return Crypto.decrypt(store.getString(TRUSTSTORE_PASSWORD));
	}

	@Override
	public String getKeyStorePath() {
		return store.getString(KEYSTORE_PATH);
	}

	@Override
	public char[] getKeyStorePassword() {
		return Crypto.decrypt(store.getString(KEYSTORE_PASSWORD));
	}

	@Override
	public void setTrustStorePath(String path) {
		store.setValue(TRUSTSTORE_PATH, path);
	}

	@Override
	public void setTrustStorePassword(char[] password) {
		String pwd = new String(password);
		if((pwd.length() > 0) && !pwd.equals(store.getString(TRUSTSTORE_PASSWORD))) {
			final String cryptedPassword = Crypto.encrypt(password);			
			store.setValue(TRUSTSTORE_PASSWORD, cryptedPassword);			
		}
	}

	@Override
	public void setKeyStorePath(String path) {
		store.setValue(KEYSTORE_PATH, path);
	}

	@Override
	public void setKeyStorePassword(char[] password) {
		String pwd = new String(password);
		if((pwd.length() > 0) && !pwd.equals(store.getString(KEYSTORE_PASSWORD))) {
			final String cryptedPassword = Crypto.encrypt(password);			
			store.setValue(KEYSTORE_PASSWORD, cryptedPassword);			
		}
	}

	@Override
	public boolean save() {
		try {
			store.save();
			return true;
		} catch (IOException e) {
			log("Failed to save TrustStoreProvider", e);
			return false;
		}		
	}

	@Override
	public void resetToDefault() {
		try {				
			store.setValue(KEYSTORE_PATH, ""); //$NON-NLS-1$
			store.setValue(TRUSTSTORE_PATH, ""); //$NON-NLS-1$
			store.setValue(KEYSTORE_PASSWORD, ""); //$NON-NLS-1$
			store.setValue(TRUSTSTORE_PASSWORD, ""); //$NON-NLS-1$
			store.save();
		}
		catch (IOException e) {
			log("Failed to reset TrustStoreProvider", e);
		}		
	}

}

