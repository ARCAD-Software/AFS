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
package com.arcadsoftware.rest.console;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Dictionary;
import java.util.List;

import org.restlet.data.Form;
import org.restlet.data.Language;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.MultiLanguageMessages;

/**
 * Helper class that can be use to create a Console section based on Configuration 
 * but with a specific part or set of action.
 * 
 * <p>
 * The messages file must content the following strings :
 * 
 * <ul>
 * <li>console.label     - default is Bundle name.
 * <li>console.category  - default is "Administration".
 * <li>console.help      - default is empty.
 * <li>console.save      - default is "Save".
 * <li>console.save.help - default is empty.
 * <li>console.saved     - default is empty (used to fill a confirmation dialog box).
 * </ul>
 * 
 * @author ARCAD Software
 */
public abstract class AbstractConfigSection implements IRestConsoleSection {

	/**
	 * Utility class used to define a multi-language version of the Form. 
	 * This definition can be a constant.
	 */
	public static class Property {
		
		private String setKey;
		private String id;
		private String fid;
		private String defaultValue;
		private String secretValue;
		private String enums[];

		/**
		 * Default constructor.
		 */
		public Property() {
			super();
		}
		
		/**
		 * 
		 * @param setKey The Form Set in witch this property must be declared.
		 */
		public Property(String setKey) {
			super();
			this.setKey = setKey;
		}

		/**
		 * 
		 * @param setKey The Form Set in witch this property must be declared.
		 * @param id The property ID.
		 * @param defaultValue Default value.
		 */
		public Property(String setKey, String id, String defaultValue) {
			super();
			this.setKey = setKey;
			this.id = id;
			fid = id.replace('.', '_');
			this.defaultValue = defaultValue;
		}

		/**
		 * 
		 * @param id The property ID.
		 * @param defaultValue Default value.
		 */
		public Property(String id, String defaultValue) {
			super();
			this.id = id;
			fid = id.replace('.', '_');
			this.defaultValue = defaultValue;
		}

		/**
		 * 
		 * @param setKey The Form Set in witch this property must be declared.
		 * @param id The property ID.
		 * @param defaultValue Default value.
		 * @param secretValue The password crypt flag property, or "true". 
		 */
		public Property(String setKey, String id, String defaultValue, String secretValue) {
			super();
			this.setKey = setKey;
			this.id = id;
			fid = id.replace('.', '_');
			this.defaultValue = defaultValue;
			this.secretValue = secretValue;
		}

		/**
		 * Create a property that present a selection into a limited list of choices.
		 * @param setKey The Form Set in witch this property must be declared.
		 * @param id The property ID.
		 * @param defaultValue Default value.
		 * @param enums Values choices.
		 */
		public Property(String setKey, String id, String defaultValue, String... enums) {
			super();
			this.setKey = setKey;
			this.id = id;
			fid = id.replace('.', '_');
			this.defaultValue = defaultValue;
			this.enums = enums;
		}

		/**
		 * Set enumeration choices.
		 * @param value
		 */
		public void setEnums(String... value) {
			enums = value;
		}
		
		/**
		 * @return Property field ID (as stored into the HTTP Request).
		 */
		public String getFieldId() {
			return fid;
		}
	}
	
	public static final String ACTION_SAVE = "save"; //$NON-NLS-1$
	
	private MultiLanguageMessages messages;
	private AbstractConfiguredActivator activator;
	private String id;

	/**
	 * @param activator
	 * @param messages
	 */
	public AbstractConfigSection(AbstractConfiguredActivator activator, MultiLanguageMessages messages) {
		super();
		this.activator = activator;
		this.messages = messages;
		id = activator.getContext().getBundle().getSymbolicName().replace('.', '_');
	}

	public String getLabel(Language language) {
		return messages.get("console.label", language, (String)activator.getContext().getBundle().getHeaders().get("Bundle-Name")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public String getId() {
		return id;
	}

	public String getCategory(Language language) {
		return messages.get("console.category", language, (String)activator.getContext().getBundle().getHeaders().get("Bundle-Vendor")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public String getHelp(Language language) {
		return messages.get("console.help", language, ""); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public List<ConsoleField> getForm(Language language) {
		Dictionary<String, Object> props = getConfiguration();
		ArrayList<ConsoleField> form = new ArrayList<ConsoleField>();
		String s = messages.get("console.text", language, ""); //$NON-NLS-1$ //$NON-NLS-2$
		if ((s != null) && (s.length() > 0)) {
			form.add(new ConsoleText(s));
		}
		String curSet = null;
		List<Property> propsFields = getProperties();
		if (propsFields != null) {
			for(Property p:propsFields) {
				if ((p.setKey != null) && !p.setKey.equals(curSet)) {
					form.add(new ConsoleSet(messages.get(p.setKey, language)));
					curSet = p.setKey;
				}
				String ps = null;
				if (p.id != null) {
					String val = p.defaultValue;
					if (props.get(p.id) != null) {
						val = props.get(p.id).toString();
					}
					if ((p.secretValue != null) && !"false".equalsIgnoreCase(p.secretValue)) { //$NON-NLS-1$
						ps = "true"; //$NON-NLS-1$
						if (val != null) {
							if ("true".equalsIgnoreCase(p.secretValue)) { //$NON-NLS-1$
								// FIXME Do not use String to store passwords.
								val = new String(Crypto.decrypt(val.toString()));
							} else {
								Object enc = props.get(p.secretValue);
								if ((enc != null) && ("true".equalsIgnoreCase(enc.toString()) || //$NON-NLS-1$ 
										"yes".equalsIgnoreCase(enc.toString()))) { //$NON-NLS-1$
									// FIXME Do not use String to store passwords.
									val = new String(Crypto.decrypt(val.toString()));
								}
							}
						}
					}
					ArrayList<String> vals = null;
					if (p.enums != null) {
						vals = new ArrayList<String>(p.enums.length);
						Collections.addAll(vals, p.enums);
					}
					form.add(new ConsoleProperty(p.fid,messages.get(p.id, language),
							ConsoleProperty.ICON_NONE, val, ps, vals, vals != null, // enums are "readonly".
							false, messages.get(p.id+".help", language, (String) null))); //$NON-NLS-1$
				}
			}
		}
		form.add(new ConsoleAction(ACTION_SAVE, null, messages.get("console.save", language, "Save"), //$NON-NLS-1$ //$NON-NLS-2$ 
				ConsoleField.ICON_DISK, false, messages.get("console.save.help", language, (String) null))); //$NON-NLS-1$
		addActions(form,language);
		return form;
	}

	/**
	 * Override this method to declare new Actions into this section.
	 * 
	 * @param form
	 * @param language
	 */
	protected void addActions(ArrayList<ConsoleField> form, Language language) {
		// Nothing to do here.
	}

	public List<ConsoleField> performAction(String actionId, Language language, Form params) {
		if (ACTION_SAVE.equals(actionId)) {
			List<Property> propsFields = getProperties();
			if (propsFields == null) {
				return null;
			}
			Dictionary<String,Object> props = getConfiguration();
			for (Property p:propsFields) {
				if (p.id != null) {
					String value = params.getFirstValue(p.fid);
					if (value != null) {
						if (p.secretValue != null) {
							if (!"false".equalsIgnoreCase(p.secretValue)) { //$NON-NLS-1$
								if (!"true".equalsIgnoreCase(p.secretValue)) { //$NON-NLS-1$
									props.put(p.secretValue, "true"); //$NON-NLS-1$
								}
								// Restlet use String even for passwords...
								value = Crypto.encrypt(value.toCharArray());
							}
						}
						props.put(p.id, value);
						// TODO Property typées : si param ="" et que le type n'est pas string alors faire un remove !
					}
				}
			}
			updateConfiguration(props);
			String result = messages.get("console.saved", language, (String) null); //$NON-NLS-1$
			if (result != null) {
				ArrayList<ConsoleField> form = new ArrayList<ConsoleField>();
				form.add(new ConsoleText(result, ConsoleText.ICON_NONE, null)); //$NON-NLS-1$
				return form;
			}
		}
		return null;
	}

	/**
	 * Get the configuration properties associated with this Section.
	 * @return
	 */
	protected Dictionary<String, Object> getConfiguration() {
		return activator.getCurrentConfiguration();
	}

	/**
	 * Save the configuration properties associated with this section.
	 *  
	 * @param props
	 */
	protected void updateConfiguration(Dictionary<String, Object> props) {
		activator.updateConfiguration(props);
	}

	/**
	 * 
	 * @return The list of properties to be edited.
	 * @see Property
	 */
	protected abstract List<Property> getProperties();

	/**
	 * @return The Message translator object.
	 */
	public final MultiLanguageMessages getMessages() {
		return messages;
	}
	
	/**
	 * Translate.
	 * 
	 * @param key
	 * @param language
	 * @return
	 */
	public final String getMessage(String key, Language language) {
		return messages.get(key, language);
	}

	/**
	 * @return The bundle Activator.
	 */
	public final AbstractConfiguredActivator getActivator() {
		return activator;
	}

	/**
	 * Set the Console section ID.
	 * @param id
	 */
	public final void setId(String id) {
		this.id = id;
	}

	/**
	 * Create a simple error information dialog.
	 * 
	 * @param title
	 * @param text
	 * @return
	 */
	protected final List<ConsoleField> createErrorDialog(String title,String text) {
		ArrayList<ConsoleField> form = new ArrayList<ConsoleField>();
		form.add(new ConsoleSet(title));
		form.add(new ConsoleText(text, ConsoleField.ICON_ERROR, null));
		return form;
	}

	/**
	 * Create a simple information dialog.
	 * 
	 * @param title
	 * @param text
	 * @return
	 */
	protected final List<ConsoleField> createInfoDialog(String title,String text) {
		ArrayList<ConsoleField> form = new ArrayList<ConsoleField>();
		form.add(new ConsoleSet(title));
		form.add(new ConsoleText(text, ConsoleField.ICON_INFO, null));
		return form;
	}

	/**
	 * Create a simple error information dialog.
	 * 
	 * @param title
	 * @param text
	 * @param language
	 * @return
	 */
	protected final List<ConsoleField> createErrorDialog(String titleKey,String textKey, Language language) {
		return createErrorDialog(messages.get(titleKey, language), messages.get(textKey, language));
	}

	/**
	 * Create a simple information dialog.
	 * 
	 * @param title
	 * @param text
	 * @param language
	 * @return
	 */
	protected final List<ConsoleField> createInfoDialog(String titleKey,String textKey, Language language) {
		return createInfoDialog(messages.get(titleKey, language), messages.get(textKey, language));
	}
}
