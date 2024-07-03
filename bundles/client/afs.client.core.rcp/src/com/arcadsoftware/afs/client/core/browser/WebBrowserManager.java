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
package com.arcadsoftware.afs.client.core.browser;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import com.arcadsoftware.afs.client.core.internal.Activator;

public class WebBrowserManager {

	public static void openURL(String url){
		IWorkbenchBrowserSupport support =
				  PlatformUI.getWorkbench().getBrowserSupport();
		IWebBrowser browser;
		try {
			browser = support.createBrowser("dropbrowser");
			browser.openURL(new URL(url));
		} catch (PartInitException | MalformedURLException e) {
			Activator.getDefault().error(e.getLocalizedMessage(), e);
		}
		
	}
	
}
