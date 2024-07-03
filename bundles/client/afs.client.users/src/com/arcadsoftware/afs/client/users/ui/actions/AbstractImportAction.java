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
package com.arcadsoftware.afs.client.users.ui.actions;

import java.util.Collections;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.IACCMessages;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractConnectedBeanMapAction;
import com.arcadsoftware.afs.client.users.AuthType;
import com.arcadsoftware.afs.client.users.IUsersConsts;
import com.arcadsoftware.afs.client.users.LDAPAccessHelper;
import com.arcadsoftware.afs.client.users.internal.Activator;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractImportAction extends AbstractConnectedBeanMapAction {

	protected abstract AuthType getSelectedAuthType();

	protected abstract Object[] getSelectedItems();

	protected abstract void completeUserBeanMap(BeanMap user);

	protected abstract int[] getProfiles(BeanMap user);

	public AbstractImportAction(ServerConnection connection) {
		super(connection);
	}

	@Override
	protected void setInterface() {
		setText(Activator.resString("users.import.action.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("users.import.action.tooltip")); //$NON-NLS-1$
		setImageDescriptor(AFSIcon.USER_GROUP_IMPORT.imageDescriptor());
	}

	@Override
	protected boolean canExecute() {
		final Object[] items = getSelectedItems();
		if ((items == null) || (items.length == 0)) {
			return false;
		}
		if (!isAllowed()) {
			LogUITools.logError(Activator.getDefault().getBundle(),
					UserMessageManager.getInstance().getMessage(IACCMessages.ERR_ACTION_NO_RIGHT));
			MessageDialog.openError(Activator.getDefault().getPluginShell(), getText(),
					Activator.resString("msg.error.right.missing")); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	@SuppressWarnings("incomplete-switch")
	@Override
	protected boolean execute() {
		final Object[] items = getSelectedItems();
		if ((items != null) && (items.length > 0)) {
			switch (getSelectedAuthType()) {
			case IBMI:
				return importFromIBMi(items);
			case LDAP:
				return importFromLDAP(items);
			}
		}
		return false;
	}

	private boolean importFromIBMi(final Object[] items) {
		for (final Object o : items) {
			if (o instanceof BeanMap) {
				createIBMiUser((BeanMap) o);
			}
		}
		return true;
	}

	private void createIBMiUser(final BeanMap user) {
		boolean revert = false;
		final DataAccessHelper helper = new DataAccessHelper(connection);
		final BeanMap ibmiAuth = new BeanMap(AuthType.IBMI.code());
		try {
			completeUserBeanMap(user);
			revert = !helper.create(user);
			if (!revert) {
				final int[] profiles = getProfiles(user);
				if (profiles != null) {
					for (final int pId : profiles) {
						helper.createLink(IUsersConsts.ENTITY_USER, user.getId(), IUsersConsts.USER_LINK_PROFILES, pId);
					}
				}
				ibmiAuth.put("user", user.getId());
				ibmiAuth.put("login", user.getString("ibmiauth.login"));
				revert = !helper.create(ibmiAuth);
			}
		} catch (final Exception e) {
			Activator.getDefault().error("Unable to import the IBMi user \"" +
					user.getString("ibmiauth.login", "-unknown-") + //$NON-NLS-1$
					": " + e.getLocalizedMessage(), e); //$NON-NLS-1$
			revert = true;
		}
		if (revert) {
			helper.delete(user);
		}
	}

	private boolean importFromLDAP(final Object[] items) {
		BeanMap user = null;
		final LDAPAccessHelper ldapHelper = new LDAPAccessHelper(connection);
		for (final Object item : items) {
			if (item instanceof BeanMap) {
				user = (BeanMap) item;
				if (IUsersConsts.ENTITY_USER.equals(user.getType())) {
					// complete user beanMap
					completeUserBeanMap(user);
					// add LDAP auth
					ldapHelper.importUser(user, getProfiles(user));
				}
			}
		}
		return true;
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return Collections.emptyList();
	}
}