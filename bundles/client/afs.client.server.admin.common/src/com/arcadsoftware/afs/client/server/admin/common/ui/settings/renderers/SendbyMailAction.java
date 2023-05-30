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
package com.arcadsoftware.afs.client.server.admin.common.ui.settings.renderers;

import java.awt.Desktop;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;

import org.eclipse.jface.action.Action;

import com.arcadsoftware.afs.client.server.admin.common.Activator;

public class SendbyMailAction extends Action {

	private static final long serialVersionUID = 6786832822485063660L;

	public SendbyMailAction() {
		setText(Activator.resString("settingseditor.actions.sendmail.text")); //$NON-NLS-1$
	}
	
	public String encode(String s) throws UnsupportedEncodingException {
		return URLEncoder.encode(s, "utf-8") //$NON-NLS-1$
		                  .replaceAll("\\+", "%20") //$NON-NLS-1$ //$NON-NLS-2$
		                  .replaceAll("\\%0A", "%0D%0A"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	@Override
	public void run() {
		URI uri = null;
		try {
			String content = encode(getContent());
			String subject = encode("AFS-Settings: " + getSection()); //$NON-NLS-1$
			StringBuilder uriString = new StringBuilder();
			uriString.append("mailto:?");//$NON-NLS-1$
			uriString.append("subject=").append(subject);//$NON-NLS-1$
			uriString.append("&body=").append(content);//$NON-NLS-1$
			uri = new URI(uriString.toString());
			Desktop.getDesktop().mail(uri);
		} catch (IOException e) {
			Activator.getInstance().log(Activator.LOGLVL_FATAL,
					"SendbyMailAction:"+e.getLocalizedMessage(),e); //$NON-NLS-1$
		} catch (URISyntaxException e) {
			Activator.getInstance().log(Activator.LOGLVL_FATAL,
					"SendbyMailAction:"+e.getLocalizedMessage(),e); //$NON-NLS-1$
		}
	}
	
	public String getContent(){
		return ""; //$NON-NLS-1$
	}
	
	public String getSection(){
		return "Unknown"; //$NON-NLS-1$
	}	
	
}
