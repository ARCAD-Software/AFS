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
package com.arcadsoftware.afs.client.server.internals.ui.composites;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.ui.AFSFormatTools;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSStandardComposite;

public class ServerMainComposite extends AbstractAFSStandardComposite {

	private static final String DEFAULT_URL = "http://localhost:" + System.getProperty("afs.default.port", "5252"); //$NON-NLS-1$
	
	private Text nameText;
	
	private Text addressText;
	private Spinner port;
	private Button useHTTPS;
	
	private Server server;
		
	public ServerMainComposite(final Composite parent, final int style, final Server server) {
		super(parent, style,false);
		this.server = server;
		createControlPage();
	}

	@Override
	public void createControlPage() {
		nameText = 
			AFSFormatTools.createLabelledText(this, Activator.resString("server.wizard.add.name.label")); //$NON-NLS-1$
		addressText = 
			GuiFormatTools.createLabelledText(this, Activator.resString("server.wizard.add.address.label")); //$NON-NLS-1$
		port = GuiFormatTools.createLabelledSpinner(this, Activator.resString("server.wizard.add.port.label"));
		port.setMinimum(1);
		port.setMaximum(65535);		
		useHTTPS = AFSFormatTools.createLabelledCheckbox(this, Activator.resString("server.wizard.add.use.https.label"), false);
		
		loadFields();
		
		addCheckDataListeners(nameText);
		addCheckDataListeners(addressText);
		addCheckDataListeners(port);
	}

	private void loadFields() {
		try {
			final URL url;
			if(server == null) {
				url = new URL(DEFAULT_URL);				
			}
			else {
				nameText.setText(server.getName());
				url = new URL(server.getUrl());				
			}
			
			addressText.setText(url.getHost());
			port.setSelection(url.getPort() > -1 ? url.getPort() : url.getDefaultPort());
			useHTTPS.setSelection(url.getProtocol().equalsIgnoreCase("https"));			
		}
		catch(Exception e) {
			Activator.getInstance().log(e);
		}
	}
	
    public String getServerName(){
        return nameText.getText();
    }

    public String getUrl(){
        try {
			return buildURL().toString();
		}
        catch (MalformedURLException e) {
        	Activator.getInstance().error(e.getLocalizedMessage(),e);
			return "";
		}
    }	
	
	@Override
	public boolean checkData() {
		errorMessage = null;
		
		if (nameText!=null && nameText.getText().isEmpty()) { 
			errorMessage = Activator.resString("server.wizard.add.msg.error.nameIsMandatory"); //$NON-NLS-1$
			nameText.setFocus();
			return false;
		}
		else if (addressText != null && addressText.getText().isEmpty()) { 
			errorMessage = Activator.resString("server.wizard.add.msg.error.address.required"); //$NON-NLS-1$
			addressText.setFocus();
			return false;
		}
		else {
			try {
				buildURL();
			}
			catch(Exception e) {
				errorMessage = Activator.resString("server.wizard.add.msg.error.malformed.url"); //$NON-NLS-1$
				addressText.setFocus();
				return false;	
			}
		}
		return true;
	}
	
	private URL buildURL() throws MalformedURLException {
		return new URL((useHTTPS.getSelection() ? "https" : "http") + "://" + addressText.getText() + ":" + port.getSelection());
	}
}
