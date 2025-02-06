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
package com.arcadsoftware.afs.framework.messages;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Locale;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.InvalidRegistryObjectException;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.framework.internals.Activator;

public class UserMessageManager extends AbstractUserMessageManager {

	private static final String USER_MESSAGE_EXTENSION = "com.arcadsoftware.afs.framework.userMessage"; //$NON-NLS-1$
	private static final String PROPERTY_FILE = "filename"; //$NON-NLS-1$
	private static UserMessageManager instance = new UserMessageManager();

	public static UserMessageManager getInstance() {
		return instance;
	}

	private UserMessageManager() {
		super(true);
	}

	@Override
	protected void load() {
		try {
			final IExtensionRegistry reg = Platform.getExtensionRegistry();
			if (reg != null) {
				for (final IConfigurationElement element : reg.getConfigurationElementsFor(USER_MESSAGE_EXTENSION)) {
					final String filename = element.getAttribute(PROPERTY_FILE);
					final String bundleName = element.getNamespaceIdentifier();
					final Bundle bundle = Platform.getBundle(bundleName);
					addBundleFile(bundle, filename);
				}
			}
		} catch (final InvalidRegistryObjectException e) {
			if (Activator.getDefault() != null) {
				Activator.getDefault().error(e.getLocalizedMessage(), e);
			}
		}
	}

	private File getBundleFile(Bundle bundle, String filename) {
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
			if (url == null) {
				return null;
			}
		}
		try {
			url = FileLocator.toFileURL(url);
			return new File(new URL(null, url.toString().replaceAll(" ", "%20")).toURI()); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (final URISyntaxException e) {
		} catch (final Throwable e) {}
		return null;
	}

	private void addBundleFile(Bundle bundle, String filename) {
		final Locale l = Locale.getDefault();
		final String country = l.getCountry();
		final String lang = l.getLanguage();
		final int pos = filename.lastIndexOf("."); //$NON-NLS-1$
		String basename;
		String extension;
		if (pos > 0) {
			basename = filename.substring(0, pos);
			extension = filename.substring(pos + 1, filename.length());
		} else {
			basename = filename;
			extension = "properties";//$NON-NLS-1$
		}
		// File resultFile = new File(basename+"_"+lang+"_"+country+"."+extension);
		File resultFile = getBundleFile(bundle, basename + '_' + lang + '_' + country + '.' + extension);
		if (resultFile == null) {
			// resultFile = new File(basename+"_"+lang+"."+extension);
			resultFile = getBundleFile(bundle, basename + '_' + lang + '.' + extension);
			if (resultFile == null) {
				resultFile = getBundleFile(bundle, basename + '.' + extension);
			}
		}
		if (resultFile != null) {
			try {
				addPropertyFile(resultFile);
			} catch (final FileNotFoundException e) {
			} catch (final IOException e) {}
		}
	}

}
