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
package com.arcadsoftware.server.messages.internal;

import java.io.File;
import java.io.IOException;
import java.util.Locale;

import org.osgi.framework.Bundle;

import com.arcadsoftware.osgi.FileBundleTracker;
import com.arcadsoftware.server.messages.MessageManager;

public class MessageBundleTracker extends FileBundleTracker {

	private final String country;
	private final String lang;
	
	public MessageBundleTracker(Activator activator) {
		super(activator.getContext());
		setActivator(activator);
		setHeader("Arcad-MessageFile"); //$NON-NLS-1$
		final Locale l = Locale.getDefault();		
		country = l.getCountry();
		lang = l.getLanguage();
	}
	
	@Override
	protected void addBundleFile(Bundle bundle, File file, String filename) {
		final int pos = filename.lastIndexOf('.'); 
		final String basename;
		final String extension;
		if (pos > 0) {
			basename = filename.substring(0, pos);
			extension = filename.substring(pos + 1, filename.length());
		} else {
			basename = filename;
			extension = "properties"; //$NON-NLS-1$
		}
		File resultFile =  getBundleFile(bundle, basename + '_' + lang + '_' + country + '.' + extension);
		if (resultFile == null) {
			resultFile = getBundleFile(bundle, basename + '_' + lang + '.' + extension);
			if (resultFile == null) {
				resultFile = getBundleFile(bundle,basename + '.' + extension);
			}
		}
		if (resultFile != null) {
			try {
				MessageManager.addPropertyFile(resultFile);
			} catch (IOException e) {
				getActivator().log(e);
			}
		}
	}
	
}