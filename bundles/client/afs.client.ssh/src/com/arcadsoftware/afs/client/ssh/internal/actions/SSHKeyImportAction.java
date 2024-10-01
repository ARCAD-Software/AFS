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
package com.arcadsoftware.afs.client.ssh.internal.actions;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;

import org.eclipse.jface.dialogs.MessageDialog;
import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.wizarded.AbstractConnectedWizardedAddAction;
import com.arcadsoftware.afs.client.core.ui.wizards.GenericConnectedWizardPage;
import com.arcadsoftware.afs.client.ssh.internal.Activator;
import com.arcadsoftware.afs.client.ssh.internal.ISSHRights;
import com.arcadsoftware.afs.client.ssh.internal.RightManager;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.ssh.model.SSHKey;
import com.arcadsoftware.ssh.model.SSHKeyUpload;
import com.arcadsoftware.ssh.model.SSHRoutes;

public class SSHKeyImportAction extends AbstractConnectedWizardedAddAction {

	private static final String IMPORTED = "imported";

	GenericConnectedWizardPage mainPage;

	BeanMapList existinglinks;

	public SSHKeyImportAction(final ServerConnection connexion) {
		super(connexion);
	}

	@Override
	public void addConnectedWizardPages(final ServerConnection connexion) {
		mainPage = new GenericConnectedWizardPage(connexion, "main", //$NON-NLS-1$
				Activator.resString("sshkey.wizard.import.page.main.text"), //$NON-NLS-1$
				Activator.resString("sshkey.wizard.import.page.main.description"), //$NON-NLS-1$
				SSHKey.ENTITY, "import_page");
		addConnectedWizardPage(mainPage);
	}

	@Override
	public Bundle getBundle() {
		return Activator.getDefault().getBundle();
	}

	@Override
	public UserMessage getErrorMessage() {
		return null;
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return RightManager.getInstance().getExpectedRights(ISSHRights.SSHKEY_ADD);
	}

	@Override
	public String getType() {
		return SSHKey.ENTITY;
	}

	@Override
	public String getWizardTitle() {
		return Activator.resString("sshkey.wizard.import.title"); //$NON-NLS-1$
	}

	@Override
	public void initBeanMap(final BeanMap beanmap) {
		// Nothing to do
	}

	@Override
	public boolean doBeforeSaving(BeanMap beanmap) {
		final SSHKeyUpload sshKey = new SSHKeyUpload(beanmap);
		final boolean alreadyExists = Optional.ofNullable(helper.read(SSHKey.ENTITY, sshKey.getName())) //
				.filter(bean -> !bean.isNewBeanMap()) //
				.isPresent();
		
		if(alreadyExists) {
			MessageDialog.openError(Activator.getDefault().getPluginShell(),
					"ARCAD Software", Activator.resString("sshkey.wizard.add.already.exists", sshKey.getName()));
		}
		
		return !alreadyExists;
	}
	
	@Override
	public boolean saveBeanMap(final BeanMap beanMap) {
		final String imported = beanMap.remove(IMPORTED).toString();
		final File importedFile = new File(imported);
		final String privateKey;
		try {
			SSHKeyUpload keyUpload = new SSHKeyUpload(beanMap);
			if(importedFile.exists()) {
				privateKey = String.join("\n", Files.readAllLines(importedFile.toPath()));
			}
			else {
				privateKey = imported;	
			}
			keyUpload.setPrivateKey(privateKey);	
			final String s = keyUpload.getPassphrase();
			if((s != null) && !s.isEmpty()) {
				keyUpload.setPassphrase(Crypto.fog(s.toCharArray()));				
			}
			final BeanMap uploadResult = helper.getConnection().getDataAccess().post(SSHRoutes.IMPORT_KEY, keyUpload.getBeanmap());
			if (uploadResult == null) {
				Activator.getDefault()
						.openError(Activator.resString("sshkey.wizard.import.server.error", helper.getLastMessage()));
			} else {
				keyUpload = new SSHKeyUpload(uploadResult);
				if (!keyUpload.isSuccessful()) {
					Activator.getDefault()
							.openError(Activator.resString("sshkey.wizard.import.error", keyUpload.getMessage()));
				} else {
					return true;
				}
			}
		} catch (final Exception e) {
			Activator.getDefault().log(e);
		}
		return false;
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("sshkey.action.import.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("sshkey.action.import.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SSHKEY_IMPORT.imageDescriptor());
	}
}
