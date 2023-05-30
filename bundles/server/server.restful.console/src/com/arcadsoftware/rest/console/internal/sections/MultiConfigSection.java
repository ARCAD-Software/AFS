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
package com.arcadsoftware.rest.console.internal.sections;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.List;
import java.util.Properties;

import org.osgi.framework.Bundle;
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
import com.arcadsoftware.rest.console.ConsoleSet;
import com.arcadsoftware.rest.console.ConsoleText;
import com.arcadsoftware.rest.console.IConsoleActionService;
import com.arcadsoftware.rest.console.internal.Activator;

public class MultiConfigSection extends AbstractSection {

	private ArrayList<ConfigSection> sections = new ArrayList<ConfigSection>();
	
	public void setBundle(Bundle bundle) {
		super.setBundle(bundle);
		for(ConfigSection cs:sections) {
			cs.setBundle(bundle);
		}		
	}

	public void setdefaultMessageFile(String path) {
		super.setdefaultMessageFile(path);
		for(ConfigSection cs:sections) {
			if ((cs.getMessages() == null) || (cs.getMessages().length() == 0)) {
				cs.setMessages(getMessages());
			}
			cs.setdefaultMessageFile(path);
		}		
	}

	public void setActivator(Activator activator) {
		super.setActivator(activator); 
		for(ConfigSection cs:sections) {
			cs.setActivator(activator);
		}		
	}

	public List<ConsoleField> performAction(String actionId, Language language, Form params) {
		if (!AbstractConfigSection.ACTION_SAVE.equalsIgnoreCase(actionId)) { //$NON-NLS-1$
			for(IConsoleActionService cas: getActivator().getConsoleActionServices(getId())) {
				if (actionId.equalsIgnoreCase(cas.getCode())) {
					Properties parameters = new Properties();
					for(String key:params.getNames()) {
						parameters.put(key.replace('_', '.'), params.getFirstValue(key));
					}
					return cas.run(getId(), language, null, parameters);
				}
			}
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, getActivator().localize("error.invalidaction", language)); //$NON-NLS-1$
		}
		ConfigurationAdmin ca = getActivator().getConfigurationAdmin();
		if (ca == null) {
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.noconfservice", language)); //$NON-NLS-1$
		}
		ArrayList<String> result = new ArrayList<String>();
		for(ConfigSection cs:sections) {
			String err = save(ca, cs, language, params);
			if (err != null) {
				result.add(err);
			}
		}		
		if (result.size() == 0) {
			return getActivator().infoForm("label.success", "info.confsuccess", null, ConsoleAction.ICON_INFO, language); //$NON-NLS-1$ //$NON-NLS-2$
		}
		ArrayList<ConsoleField> dlg = new ArrayList<ConsoleField>(2);
		dlg.add(new ConsoleSet(getActivator().localize("error", language), ConsoleAction.ICON_ERROR, null)); //$NON-NLS-1$
		for(String err:result) {
			dlg.add(new ConsoleText(err, ConsoleField.ICON_NONE,null));
		}
		return dlg;
	}
	
	@SuppressWarnings("unchecked")
	private String save(ConfigurationAdmin ca, ConfigSection cs, Language language, Form params) {
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
		}
		if (loc == null) {
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, getActivator().localize("error.noconfservice", language)); //$NON-NLS-1$
		}*/
		Configuration c = null;
		try {
			c = ca.getConfiguration(cs.getPid(), loc);
			if (c == null) {
				return '[' + cs.getLabel(language) + "] " + getActivator().localize("error.noconfservice", language); //$NON-NLS-1$ //$NON-NLS-2$
			}
		} catch (Exception e) {
			getActivator().error(e.getLocalizedMessage(), e);
			return '[' + cs.getLabel(language) + "] " +getActivator().localize("error.confaccess", language) + e.getLocalizedMessage(); //$NON-NLS-1$ //$NON-NLS-2$
		}
		@SuppressWarnings("rawtypes")
		Dictionary props = c.getProperties();
		if (props == null) {
			props = new Properties();
			props.put(Constants.SERVICE_PID, cs.getPid());
		}
		String prefix = cs.getId() + '_';
		for(ConsoleProperty prop:cs.getProperties()) {
			String value = params.getFirstValue(prefix + prop.getId().replace('.', '_')); //$NON-NLS-1$
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
			}
		}
		try {
			c.update(props);
			return null;
		} catch (IOException e) {
			getActivator().error(e.getLocalizedMessage(), e);
			return '[' + cs.getLabel(language) + "] " + getActivator().localize("error.confrec", language) + ' ' + e.getLocalizedMessage(); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
	
	public boolean isActivated() {
		for(ConfigSection cs:sections) {
			if (cs.isActivated()) {
				return true;
			}
		}
		return false;
	}

	public List<ConsoleField> getForm(Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>();
		for(ConfigSection cs:sections) {
			String prefix = cs.getId() + '_';
			result.add(new ConsoleSet(cs.getLabel(language),cs.getIcon(),null));
			for(ConsoleField cf: cs.getForm(language)) {
				if (cf instanceof ConsoleProperty) {
					((ConsoleProperty)cf).setId(prefix + ((ConsoleProperty)cf).getId());
				}
				if (!(cf instanceof ConsoleAction)) {
					result.add(cf);
				}
			}
		}
		result.add(new ConsoleAction(AbstractConfigSection.ACTION_SAVE, null, getActivator().localize("label.save", language), ConsoleAction.ICON_OK, false, null)); //$NON-NLS-1$ //$NON-NLS-2$
		for(IConsoleActionService cas: getActivator().getConsoleActionServices(getId())) {
			result.add(new ConsoleAction(cas.getCode(), null, cas.getLabel(language), cas.getIcon(), cas.isHidden(), cas.getHelp(language)));
		}
		return result;
	}

	@Override
	public String getId() {
		String id = super.getId();
		if (NULLID.equals(id)) {
			if (sections.size() == 0) {
				return id;
			}
			StringBuilder sb = new StringBuilder();
			for(ConfigSection cs:sections) {
				sb.append(cs.getId());
			}
			return sb.toString();
		}
		return id;
	}

}
