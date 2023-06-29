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
package com.arcadsoftware.afs.client.core.security;

import java.io.IOException;
import java.util.Optional;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.osgi.service.log.LogService;

import com.arcadsoftware.aev.core.osgi.ServiceRegistry;
import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.crypt.Crypto;

public abstract class PreferenceBasedTrustStoreProvider implements ITrustStoreProvider{

	private static final String KEYSTORE_PASSWORD = "keystore.password";
	private static final String KEYSTORE_PATH = "keystore.path";
	private static final String TRUSTSTORE_PASSWORD = "truststore.password";
	private static final String TRUSTSTORE_PATH = "truststore.path";
	private final IPersistentPreferenceStore store; 

	public PreferenceBasedTrustStoreProvider(){
		store = getPreferenceStore();
		store.setDefault(KEYSTORE_PATH, "");
		store.setDefault(TRUSTSTORE_PATH, "");
		store.setDefault(KEYSTORE_PASSWORD, "");
		store.setDefault(TRUSTSTORE_PASSWORD, "");
	}
	
	protected abstract IPersistentPreferenceStore getPreferenceStore();
	
	protected Optional<LogService> getLogService() {
		return ServiceRegistry.lookup(LogService.class);
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
		if(pwd.length() > 0 && !pwd.equals(store.getString(TRUSTSTORE_PASSWORD))) {
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
		if(pwd.length() > 0 && !pwd.equals(store.getString(KEYSTORE_PASSWORD))) {
			final String cryptedPassword = Crypto.encrypt(password);			
			store.setValue(KEYSTORE_PASSWORD, cryptedPassword);			
		}
	}

	@Override
	public boolean save() {
		try {
			store.save();
			return true;
		}
		catch (IOException e) {
			getLogService().ifPresent(logger -> logger.log(LogService.LOG_ERROR, "Failed to save TrustStoreProvider", e));
			return false;
		}		
	}

	@Override
	public void resetToDefault() {
		try {				
			store.setValue(KEYSTORE_PATH, "");
			store.setValue(TRUSTSTORE_PATH, "");
			store.setValue(KEYSTORE_PASSWORD, "");
			store.setValue(TRUSTSTORE_PASSWORD, "");
			store.save();
		}
		catch (IOException e) {
			getLogService().ifPresent(logger -> logger.log(LogService.LOG_ERROR, "Failed to reset TrustStoreProvider", e));
		}		
	}

}

