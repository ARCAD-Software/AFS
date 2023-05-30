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
package com.arcadsoftware.afs.client.ssh.internal.actions;

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
import com.arcadsoftware.ssh.model.SSHKeyType;
import com.arcadsoftware.ssh.model.SSHRoutes;

public class SSHKeyAddAction extends AbstractConnectedWizardedAddAction {

	GenericConnectedWizardPage mainPage;

	BeanMapList existinglinks;

	public SSHKeyAddAction(final ServerConnection connexion) {
		super(connexion);
	}

	@Override
	public void addConnectedWizardPages(final ServerConnection connexion) {
		mainPage = new GenericConnectedWizardPage(connexion, "main", //$NON-NLS-1$
				Activator.resString("sshkey.wizard.add.page.main.text"), //$NON-NLS-1$
				Activator.resString("sshkey.wizard.add.page.main.description"), //$NON-NLS-1$
				SSHKey.ENTITY, "main_page");
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
		return Activator.resString("sshkey.wizard.add.title"); //$NON-NLS-1$
	}

	@Override
	public void initBeanMap(final BeanMap beanmap) {
		// nothing to do
	}

	@Override
	public boolean doBeforeSaving(BeanMap beanmap) {
		final SSHKey sshKey = new SSHKey(beanmap);
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
		final SSHKey sshKey = new SSHKey(beanMap);

		if (sshKey.getAlgorithm().equals("1")) {
			sshKey.setType(SSHKeyType.RSA);
		} else {
			sshKey.setType(SSHKeyType.EDDSA);
		}

		if (sshKey.isEncrypted()) {
			sshKey.setPassphrase(Crypto.fog(sshKey.getPassphrase()));
		}
		return helper.put(SSHRoutes.GENERATE_KEY, sshKey.getBeanMap());
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("sshkey.action.add.text"));//$NON-NLS-1$
		setToolTipText(Activator.resString("sshkey.action.add.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SSHKEY_CREATE.imageDescriptor());
	}
}
