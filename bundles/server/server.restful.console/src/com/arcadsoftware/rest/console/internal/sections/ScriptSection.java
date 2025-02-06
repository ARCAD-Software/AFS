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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

import org.osgi.framework.Bundle;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.groovy.security.SecuredCompilerConfiguration;
import com.arcadsoftware.mail.Attachment;
import com.arcadsoftware.mail.ISendMail;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.ConsoleSet;
import com.arcadsoftware.rest.console.ConsoleText;
import com.arcadsoftware.rest.console.IConsoleActionService;

import groovy.lang.Binding;
import groovy.lang.GroovyShell;

public class ScriptSection extends Section {

	public class API {
		
		private Language language;
		private String message;
		private boolean error;
		public List<ConsoleField> responseForm = new ArrayList<ConsoleField>();
		
		public Language getLanguage() {
			return language;
		}
		
		public void setError(boolean error) {
			this.error = error;
		}
		
		public boolean isError() {
			return error;
		}
		
		public void setMessage(String message) {
			this.message = message;
		}
		
		public void criticalError(int status, String message) {
			throw new ResourceException(Status.valueOf(status), message);
		}
		
		public String localize(String label){
			return ScriptSection.this.localize(label, language);
		}
		
		public void formSet(String label, int icon, String help) {
			responseForm.add(new ConsoleSet(localize(label), icon, localize(help)));
		}
		
		public void formText(String label, int icon, String help) {
			responseForm.add(new ConsoleText(localize(label), icon, localize(help)));
		}
		
		public void formAction(String id) {
			responseForm.add(ScriptSection.this.getPublicAction(id));
		}
		
		public void formProperty(String id, String label, int icon, String defaultvalue, String password, ArrayList<String> list, boolean readonly, boolean hidden, String help) {
			responseForm.add(new ConsoleProperty(id, localize(label), icon, defaultvalue, password, list, readonly, hidden, localize(help)));
		}
		
		public void formProperty(String id, String label, int icon, String defaultvalue) {
			responseForm.add(new ConsoleProperty(id, localize(label), icon, defaultvalue, null, null, false, false, null));
		}
		
		public void formProperty(String id, String label, int icon, String defaultvalue, ArrayList<String> list, boolean readonly) {
			responseForm.add(new ConsoleProperty(id, localize(label), icon, defaultvalue, null, list, readonly, false, null));
		}
		
		public void formProperty(String id, String label, String defaultvalue) {
			responseForm.add(new ConsoleProperty(id, localize(label), ConsoleField.ICON_NONE, defaultvalue, null, null, false, false, null));
		}
		
		public String encode(char[] value) {
			return Crypto.encrypt(value);
		}
		
		public char[] decode(String value) {
			return Crypto.decrypt(value);
		}
		
		public String fog(String value) {
			return Crypto.fog(value.toCharArray());
		}
		
		public String unfog(String value) {
			return new String(Crypto.unFog(value));
		}
		
		public String md5(char[] value) {
			return Crypto.md5(value);
		}
		
		public String sha1(char[] value) {
			return Crypto.sha1(value);
		}
		
		public String whirlpool(char[] value) {
			return Crypto.whirlpool(value);
		}
		
		public String hash(char[] value) {
			return Crypto.hash(value);
		}
		
		public String toHex(byte[] array) {
			return Crypto.byteArrayToHexString(array);
		}
		
		public String toHex(byte[] array, char sep) {
			return Crypto.byteArrayToHexString(array, sep);
		}

		public byte[] fromHex(String value) {
			return Crypto.hexStringToByteArray(value);
		}

		public byte[] fromHex(String value, char sep) {
			return Crypto.hexStringToByteArray(value.replace("" + sep, "")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		
		public Bundle findBundle(String id) {
			return ScriptSection.this.getActivator().findBundle(id);
		}
		
		public void log(String message) {
			ScriptSection.this.getActivator().info(message);
		}
		
		public void debug(String message) {
			ScriptSection.this.getActivator().debug(message);
		}
		
		public void log(Throwable e) {
			ScriptSection.this.getActivator().error(e.getLocalizedMessage(), e);
		}
		
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public boolean fireEvent(String topic,Properties properties) {
			ServiceReference ref = ScriptSection.this.getActivator().getContext().getServiceReference(EventAdmin.class.getName());
			if (ref == null) {
				return false;
			}
			EventAdmin ea = (EventAdmin)ScriptSection.this.getActivator().getContext().getService(ref);
			if (ea != null) {
				try {
					ea.postEvent(new Event(topic,(Dictionary)properties));
					return true;
				} catch (SecurityException e) {}
			}
			return false;
		}
		
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public boolean sendMail(String to, String subject, String body) {
			ServiceReference ref = ScriptSection.this.getActivator().getContext().getServiceReference(ISendMail.class.getName());
			if (ref == null) {
				return false;
			}
			ISendMail ea = (ISendMail)ScriptSection.this.getActivator().getContext().getService(ref);
			if (ea == null) {
				return false;
			}
			return ea.sendEmail(to, subject, body);
		}
		
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public boolean sendMail(String fromEmail, String fromName, String to, String cc, String bcc, String subject, String body, List<Attachment> attachments) {
			ServiceReference ref = ScriptSection.this.getActivator().getContext().getServiceReference(ISendMail.class.getName());
			if (ref == null) {
				return false;
			}
			ISendMail ea = (ISendMail)ScriptSection.this.getActivator().getContext().getService(ref);
			if (ea == null) {
				return false;
			}
			return ea.sendEmail(fromEmail, fromName, to, cc, bcc, subject, body, attachments);
		}
		
		public File getBundleFile(String filename) {
			return ScriptSection.this.getActivator().getBundleFile(ScriptSection.this.getBundle(), filename);
		}
		
		@SuppressWarnings({ "rawtypes", "unchecked" })
		public Dictionary getConfiguration(String sid) {
			ConfigurationAdmin ca = ScriptSection.this.getActivator().getConfigurationAdmin();
			if (ca == null) {
				return null;
			}
			Dictionary props = null;
			try {
				Configuration c = ca.getConfiguration(sid, null);
				if (c == null) {
					return null;
				}
				props = c.getProperties();
			} catch (Exception e) {
				ScriptSection.this.getActivator().error(e.getLocalizedMessage(), e);
				return null;
			}
			if (props == null) {
				props = new Properties();
				props.put(Constants.SERVICE_PID, sid);
			}
			return props;
		}
		
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public boolean setConfiguration(String sid, Dictionary newProps) {
			ConfigurationAdmin ca = ScriptSection.this.getActivator().getConfigurationAdmin();
			if (ca == null) {
				return false;
			}
			Dictionary props = null;
			Configuration c;
			try {
				c = ca.getConfiguration(sid, null);
				if (c == null) {
					return false;
				}
				props = c.getProperties();
			} catch (Exception e) {
				ScriptSection.this.getActivator().error(e.getLocalizedMessage(), e);
				return false;
			}
			if (props == null) {
				props = new Properties();
				props.put(Constants.SERVICE_PID, sid);
			}
			Enumeration e = newProps.keys();
			while(e.hasMoreElements()) {
				Object key = e.nextElement();
				props.put(key, newProps.get(key));
			}
			try {
				c.update(props);
			} catch (IOException e1) {
				ScriptSection.this.getActivator().error(e1.getLocalizedMessage(), e1);
				return false;
			}
			return true;
		}
	}
	
	@Override
	public List<ConsoleField> getForm(Language language) {
		List<ConsoleField> result = super.getForm(language);
		if (getLabel() != null) {
			result.add(0, new ConsoleText(localize(getLabel(), language), getIcon(), null));
		}
		for(IConsoleActionService cas: getActivator().getConsoleActionServices(getId())) {
			result.add(new ConsoleAction(cas.getCode(), null, cas.getLabel(language), cas.getIcon(), cas.isHidden(), cas.getHelp(language)));
		}
		return result;
	}

	public List<ConsoleField> performAction(String actionId, Language language, Form params) {
		ConsoleAction c = getAction(actionId);
		if ((c == null) || (c.getCode() == null) || (c.getCode().length() == 0)) {
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
		ClassLoader old = Thread.currentThread().getContextClassLoader();
		try {
			Thread.currentThread().setContextClassLoader(ScriptSection.class.getClassLoader());
			Binding b = new Binding();
			API api = new API();
			api.language = language;
			b.setVariable("api", api); //$NON-NLS-1$
			for(String name:params.getNames()) {
				b.setVariable(name, params.getValues(name));
			}
			GroovyShell shell = new GroovyShell(ScriptSection.class.getClassLoader(), b, new SecuredCompilerConfiguration());
			shell.evaluate("api.with {"+c.getCode()+"}"); //$NON-NLS-1$ //$NON-NLS-2$
			if (api.error) {
				throw new ResourceException(Status.SERVER_ERROR_INTERNAL, api.message);
			}
			return api.responseForm;
		} finally {
			Thread.currentThread().setContextClassLoader(old);
		}
	}

}
