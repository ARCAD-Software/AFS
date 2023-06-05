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
package com.arcadsoftware.crypt.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.RandomGenerator;

public class Activator implements BundleActivator {

	// See Crypto class for details...
	public static char[] temp;
	
	@Override
	public void start(BundleContext context) throws Exception {
		// Just for the initialization of the Crypto class...
		Crypto.encrypt(RandomGenerator.randomString(26).toCharArray());
		recordDefaultMasterKey(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
	}
	
	private void recordDefaultMasterKey(BundleContext context) {
		if (!Crypto.isNull(temp)) {
			try {
				String loc = context.getProperty("osgi.configuration.area"); //$NON-NLS-1$
				if ((loc != null) && !loc.isEmpty()) {
					File l;
					if (loc.startsWith("file:")) { //$NON-NLS-1$
						try {
							l = new File(new URI(loc));
						} catch (URISyntaxException e) {
							l = new File("./configuration"); //$NON-NLS-1$
						}
					} else {
						l = new File(loc);
						if (!l.exists()) {
							l = new File("./configuration"); //$NON-NLS-1$
						}
					}
					if (l.isDirectory()) {
						l = new File(l, "config.ini"); //$NON-NLS-2$
						if (l.isFile()) {
							Properties p = new Properties();
							try (FileInputStream fis = new FileInputStream(l)) {
								p.load(fis);
							} catch (Exception e) {
								return;
							}
							if (!p.containsKey("com.arcadsoftware.masterkey.fog") && //$NON-NLS-1$
									!p.containsKey("com.arcadsoftware.masterkey")) { //$NON-NLS-1$
								p.put("com.arcadsoftware.masterkey.fog", Crypto.fog(temp, StandardCharsets.UTF_8)); //$NON-NLS-1$
							}
							if (!p.containsKey("com.arcadsoftware.salt.min.size")) { //$NON-NLS-1$
								p.put("com.arcadsoftware.salt.min.size", Crypto.SALTMINSIZE); //$NON-NLS-1$
							}
							if (!p.containsKey("com.arcadsoftware.iv.min.size")) { //$NON-NLS-1$
								p.put("com.arcadsoftware.iv.min.size", Crypto.IVMINSIZE); //$NON-NLS-1$
							}
							if (!p.containsKey("com.arcadsoftware.hash.min.iterations")) { //$NON-NLS-1$
								p.put("com.arcadsoftware.hash.min.iterations", Crypto.HASHMINITERATIONS); //$NON-NLS-1$
							}
							if (!p.containsKey("com.arcadsoftware.cypher.min.iterations")) { //$NON-NLS-1$
								p.put("com.arcadsoftware.cypher.min.iterations", Crypto.CIPHERMINITERATIONS); //$NON-NLS-1$
							}
							File ltemp = new File(l.getParentFile(), "config.bak"); //$NON-NLS-1$
							if (ltemp.isFile()) {
								if (ltemp.delete()) {
									if (!l.renameTo(ltemp)) {
										l.delete();
										// nothing more to do if not deleted.
									}
								}
							} else if (!l.renameTo(ltemp)) {
								l.delete();
								// nothing more to do if not deleted.
							}
							try (FileOutputStream fos = new FileOutputStream(l)) {
								p.store(fos, "Server Configuration File"); //$NON-NLS-1$
							} catch (Exception e) {
							}
						}
					}
				}
			} finally {
				Crypto.clear(temp);
			}
		}
	}

}
