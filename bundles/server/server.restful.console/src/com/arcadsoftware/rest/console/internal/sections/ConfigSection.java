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
package com.arcadsoftware.rest.console.internal.sections;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.List;
import java.util.Properties;

import org.osgi.framework.Constants;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.rest.console.AbstractConfigSection;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.ConsoleText;
import com.arcadsoftware.rest.console.IActivableConsoleNode;
import com.arcadsoftware.rest.console.IConsoleActionService;

public class ConfigSection extends Section implements IActivableConsoleNode {

	private String pid;
	private String bid;
	
	@Override
	public String getId() {
		String id = super.getId();
		if ((id == null) || NULLID.equals(id)) {
			if (pid != null) {
				return pid.replace('.', '_');
			}
			if (bid != null) {
				return bid.replace('.', '_');
			}
			return getBundle().getSymbolicName().replace('.', '_');
		}
		return id;
	}

	@Override
	public String getLabel(Language language) {
		if (getLabel() == null) {
			return localize("console.label", language, (String)getBundle().getHeaders().get("Bundle-Name")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return super.getLabel(language);
	}

	@Override
	public String getCategory(Language language) {
		if (getCategory() == null) {
			return localize("console.category", language, (String)getBundle().getHeaders().get("Bundle-Vendor")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return super.getCategory(language);
	}

	@SuppressWarnings("unchecked")
	public List<ConsoleField> performAction(String actionId, Language language, Form params) {
		ConfigurationAdmin ca = getActivator().getConfigurationAdmin();
		if (ca == null) {
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.noconfservice", language)); //$NON-NLS-1$
		}
		Configuration c = null;
		try {
			c = ca.getConfiguration(getPid(), null);
			if (c == null) {
				throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.noconfservice", language)); //$NON-NLS-1$
			}
		} catch (Exception e) {
			getActivator().error(e.getLocalizedMessage(), e);
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.confaccess", language) + e.getLocalizedMessage()); //$NON-NLS-1$
		}
		@SuppressWarnings("rawtypes")
		Dictionary props = c.getProperties();
		if (props == null) {
			props = new Properties();
			props.put(Constants.SERVICE_PID, getPid());
		}
		if (!AbstractConfigSection.ACTION_SAVE.equalsIgnoreCase(actionId)) {
			// Recherche des actions externes:
			for(IConsoleActionService cas: getActivator().getConsoleActionServices(getRealId())) {
				if (actionId.equalsIgnoreCase(cas.getCode().replace(' ', '+'))) {
					Properties parameters = new Properties();
					for(String param: params.getNames()) {
						parameters.put(param.replace('_', '.'), params.getFirstValue(param));
					}
					return cas.run(getPid(), language, props, parameters);
				}
			}
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, getActivator().localize("error.invalidaction", language)); //$NON-NLS-1$
		}
		for(ConsoleProperty prop:getProperties()) {
			String value = params.getFirstValue(prop.getId().replace('.', '_'));
			if (value != null) {
				if (prop.getPassword() != null) {
					if (!"false".equalsIgnoreCase(prop.getPassword())) { //$NON-NLS-1$
						if (!"true".equalsIgnoreCase(prop.getPassword())) { //$NON-NLS-1$
							props.put(prop.getPassword(), "true"); //$NON-NLS-1$
						}
						value = Crypto.encrypt(value.toCharArray());
					}
				}
				props.put(prop.getId(), value);
				// TODO Property typées : si param ="" et que le type n'est pas string alors faire un remove !
			}
		}
		try {
			c.update(props);
			String result = localize("console.saved", language, null); //$NON-NLS-1$
			if (result != null) {
				ArrayList<ConsoleField> form = new ArrayList<ConsoleField>();
				form.add(new ConsoleText(result, ConsoleText.ICON_NONE, null)); //$NON-NLS-1$
				return form;
			}
			return getActivator().infoForm("label.success", "info.confsuccess", null, ConsoleAction.ICON_INFO, language); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (IOException e) {
			getActivator().error(e.getLocalizedMessage(), e);
			return getActivator().infoForm("label.error", "error.confrec", null, ConsoleAction.ICON_ERROR, language); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	@Override
	public List<ConsoleField> getForm(Language language) {
		ConfigurationAdmin ca = getActivator().getConfigurationAdmin();
		if (ca == null) {
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.noconfservice", language)); //$NON-NLS-1$
		}
		@SuppressWarnings("rawtypes")
		Dictionary props = null;
		try {
			String loc = null;
			/*
			Bundle[] bundles = Activator.getInstance().getContext().getBundles();
			if (bundles != null) {
				for (int i = 0; i < bundles.length; i++) {
					if (bundles[i].getSymbolicName().equalsIgnoreCase(bid)) {
						loc = bundles[i].getLocation();
						break;
					}
				}
			}*/
			Configuration c = ca.getConfiguration(getPid(), loc);
			if (c != null) {
				props = c.getProperties();
			}
		} catch (Exception e) {
			getActivator().error(e.getLocalizedMessage(), e);
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.confaccess", language) + e.getLocalizedMessage()); //$NON-NLS-1$
		}
		List<ConsoleField> result = super.getForm(language);
		for(ConsoleField c:result) {
			if (c instanceof ConsoleProperty) {
				if (props != null) {
					Object val = props.get(((ConsoleProperty) c).getId());
					if (val != null) {
						if ((((ConsoleProperty) c).getPassword() != null) && !"false".equalsIgnoreCase(((ConsoleProperty)c).getPassword())) { //$NON-NLS-1$
							if ("true".equalsIgnoreCase(((ConsoleProperty) c).getPassword())) { //$NON-NLS-1$
								val = new String(Crypto.decrypt(val.toString()));
							} else {
								Object enc = props.get(((ConsoleProperty) c).getPassword());
								if ((enc != null) && ("true".equalsIgnoreCase(enc.toString()) || //$NON-NLS-1$ 
										"yes".equalsIgnoreCase(enc.toString()))) { //$NON-NLS-1$
									val = new String(Crypto.decrypt(val.toString()));
								}
							}
							((ConsoleProperty) c).setPassword("true"); //$NON-NLS-1$
						}
						((ConsoleProperty) c).setDefaultvalue(val.toString());
					}
				}
				((ConsoleProperty) c).setId(((ConsoleProperty) c).getId().replace('.', '_'));
			}
		}
		result.add(new ConsoleAction(AbstractConfigSection.ACTION_SAVE, null, getActivator().localize("label.save", language), ConsoleAction.ICON_OK, false, null)); //$NON-NLS-1$
		for(IConsoleActionService cas: getActivator().getConsoleActionServices(getRealId())) {
			result.add(new ConsoleAction(cas.getCode(), null, cas.getLabel(language), cas.getIcon(), cas.isHidden(), cas.getHelp(language)));
		}
		return result;
	}

	public boolean isActivated() {
		return (bid == null) || (getActivator().findBundle(bid) != null);
	}

	/**
	 * @param pid the pid to set
	 */
	public void setPid(String pid) {
		this.pid = pid;
	}

	/**
	 * @return the pid
	 */
	public String getPid() {
		if (pid != null) {
			return pid;
		}
		if (bid != null) {
			return pid;
		}
		return getBundle().getSymbolicName();
	}

	public String getBid() {
		return pid;
	}
}
