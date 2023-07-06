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
package com.arcadsoftware.tool.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Dictionary;

import org.eclipse.equinox.internal.cm.reliablefile.ReliableFileInputStream;
import org.osgi.framework.Constants;
import org.osgi.service.cm.ConfigurationAdmin;

import com.arcadsoftware.cm.simple.ConfigurationStoreManager;

public class EquinoxConfigurations {

	private final ConfigurationStoreManager csm;
	
	public EquinoxConfigurations(File root) {
		super();
		csm = new ConfigurationStoreManager(root);
		csm.setUseCFG(true);
		csm.setUseINI(true);
		csm.setUseJSON(true);
	}
	
	public boolean load(File configurationFolder) throws ClassNotFoundException, IOException {
		if ((configurationFolder == null) || !configurationFolder.isDirectory()) {
			throw new FileNotFoundException("Configuration root folder not found.");
		}
		File parent = new File(configurationFolder, "org.eclipse.osgi"); //$NON-NLS-1$
		if (!parent.isDirectory()) {
			throw new FileNotFoundException("Invalid configuration root folder (\"org.eclipse.osgi\" sub folder not found): " + configurationFolder.getAbsolutePath());
		}
		for(File f: parent.listFiles()) {
			if (f.isDirectory()) {
				File data = new File(f, "data"); //$NON-NLS-1$
				if (data.isDirectory()) {
					File store = new File(data, "store"); //$NON-NLS-1$
					if (store.isDirectory()) {
						loadStore(store);
					}
				}
			}
		}
		return !csm.isEmpty();
	}

	private void loadStore(File store) throws IOException, ClassNotFoundException {
		for (File f: store.listFiles()) {
			if (f.getName().toLowerCase().endsWith(".cfg")) {
				try (ReliableFileInputStream ris = new ReliableFileInputStream(f)) {
					try (ObjectInputStream ois = new ObjectInputStream(ris)) {
						@SuppressWarnings("unchecked")
						Dictionary<String, Object> dictionary = (Dictionary<String, Object>) ois.readObject();
						dictionary.remove(ConfigurationAdmin.SERVICE_BUNDLELOCATION);
						dictionary.remove("org.eclipse.equinox.cm.change.count"); //$NON-NLS-1$
						dictionary.remove("org.eclipse.equinox.cm.location.bound"); //$NON-NLS-1$
						String pid = (String) dictionary.remove(Constants.SERVICE_PID);
						String factoryPid = (String) dictionary.remove(ConfigurationAdmin.SERVICE_FACTORYPID);
						csm.addConfiguration(factoryPid, pid, dictionary);
					}
				}
			}
		}		
	}

	public void forceCFG() {
		csm.setUseCFG(true);
		csm.setUseINI(false);
		csm.setUseJSON(false);
	}
	
	public void save() throws IOException {
		csm.save();
	}
}
