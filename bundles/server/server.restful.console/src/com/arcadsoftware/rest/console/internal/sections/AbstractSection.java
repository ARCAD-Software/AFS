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
package com.arcadsoftware.rest.console.internal.sections;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Properties;

import org.osgi.framework.Bundle;
import org.osgi.framework.ServiceReference;
import org.restlet.data.Language;

import com.arcadsoftware.osgi.IProperties;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.IRestConsoleSection;
import com.arcadsoftware.rest.console.internal.Activator;

public abstract class AbstractSection implements ISection {

	protected static final String NULLID = "nullid"; //$NON-NLS-1$
	
	private transient Properties defaultLabels;
	private transient HashMap<String,Properties> labels;
	private transient Activator activator;
	private transient Bundle bundle;
	private String messages;
	private String id;
	private String title;
	private String label;
	private String category;
	private String keywords;
	private String help;
	private int order;
	private int icon;

	public String getLabel(Language language) {
		if (label == null) {
			return localize("console.label", language, id); //$NON-NLS-1$ 
		}
		return localize(label, language);
	}

	public String getHelp(Language language) {
		if (help == null) {
			return localize("console.help", language, ""); //$NON-NLS-1$ //$NON-NLS-2$ 
		}
		return localize(help, language);
	}

	public String getTitle(Language language) {
		if (title == null) {
			String s = localize("console.title", language, ""); //$NON-NLS-1$ //$NON-NLS-2$
			if ((s != null) && (s.length() > 0)) {
				return s;
			}
			return getLabel(language);
		}
		return localize(title, language);
	}

	public String getLabel() {
		return label;
	}

	public String getCategory() {
		return category;
	}

	public String getCategory(Language language) {
		if (category == null) {
			return localize("console.category", language, null); //$NON-NLS-1$ 
		}
		return localize(category, language);
	}

	public int getOrder() {
		return order;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getRealId() {
		return id;
	}
	
	public void setTitle(String title) {
		this.title = title;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	public void setOrder(int order) {
		this.order = order;
	}

	/**
	 * @return The node section id.
	 */
	public String getId() {
		if (id == null) {
			return NULLID;
		}
		return id.replace('.', '_');
	}
	
	/**
	 * @param icon the icon to set
	 */
	public void setIcon(int icon) {
		this.icon = icon;
	}

	/**
	 * @return the icon
	 */
	public int getIcon() {
		return icon;
	}
	
	@Override
	public boolean equals(Object obj) {
		return (obj instanceof IRestConsoleSection) && (getId() != null) && getId().equals(((IRestConsoleSection)obj).getId());
	}

	/**
	 * @param activator the activator to set
	 */
	public void setActivator(Activator activator) {
		this.activator = activator;
	}

	/**
	 * @return the activator
	 */
	public final Activator getActivator() {
		return activator;
	}

	/**
	 * Translate the field.
	 * 
	 * <p>
	 * Translation rules are:
	 * <ul>
	 * <li>Use the "label" key, if none, for ConsoleProperty, use the "id" as label key. If the label is not a key, then it is used as the text.
	 * <li>If any, use the "help" key, if none, for ConsoleProperty, use the "id"+".help" as help key. If these key do not correspond to any text then the Help message il lieft empty.
	 * </ul>
	 * @param c
	 * @param language
	 * @return
	 */
	protected ConsoleField toClientView(ConsoleField c, Language language) {
		try {
			c = (ConsoleField)c.clone();
			if (c.getLabel() == null) {
				if (c instanceof ConsoleProperty) {
					c.setLabel(localize(((ConsoleProperty)c).getId(), language));
				}
			} else {
				c.setLabel(localize(c.getLabel(), language));
			}
			if (c.getHelp() != null) {
				c.setHelp(localize(c.getHelp(), language));
			} else if (c instanceof ConsoleProperty) {
				c.setHelp(localize(((ConsoleProperty)c).getId() + ".help", language, null)); //$NON-NLS-1$
			}
			if (c instanceof ConsoleAction) {
				((ConsoleAction) c).setCode(null);
			}
			return c;
		} catch (Throwable t) {
			return null;
		}
	}

	protected String localize(String label, Language language) {
		return localize(label, language, label);
	}
	
	protected String localize(String label, Language language, String defaultValue) {
		if (label == null) {
			return defaultValue;
		}
		if (PROPERTIES.equals(getMessages())) {
			return serviceLocalize(label, language);
		}
		String lang = language.getName();
		if ((lang == null) || (lang.length() < 2)) {
			lang = ""; //$NON-NLS-1$
		} else if (lang.length() > 2) {
			lang = '_' + lang.substring(0, 2);
		} else {
			lang = '_' + lang;
		}
		if (labels == null) {
			labels = new HashMap<String, Properties>();
		}
		if (lang.length() > 0) {
			lang = lang.toLowerCase();
			Properties p = labels.get(lang);
			if (p == null) {
				p = loadProperties(lang); //$NON-NLS-1$
				if (p != null) {
					labels.put(lang, p);
				}
			}
			if (p != null) {
				String result = p.getProperty(label);
				if (result != null) {
					return result;
				}
			}
		}
		if (defaultLabels == null) {
			defaultLabels = loadProperties(""); //$NON-NLS-1$
		}
		if (defaultLabels != null) {
			String result = defaultLabels.getProperty(label);
			if (result != null) {
				return result;
			}
		}
		return defaultValue;
	}

	public void setdefaultMessageFile(String path) {
		if ((messages == null) || (messages.length() == 0)) {
			messages = path + "/messages"; //$NON-NLS-1$
		}
	}

	public void setMessages(String messages) {
		this.messages = messages;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected String serviceLocalize(String label, Language language) {
		ServiceReference ref = getActivator().getContext().getServiceReference(IProperties.clazz);
		if (ref == null) {
			return label;
		}
		Object service = getActivator().getContext().getService(ref);
		if (service instanceof IProperties) {
			return ((IProperties)service).get("console", label, MultiLanguageMessages.toLocale(language)); //$NON-NLS-1$
		}
		return label;
	}

	private Properties loadProperties(String lang) {
		if (getMessages().endsWith(".properties")) { //$NON-NLS-1$
			messages = getMessages().substring(0, getMessages().length() - 11);
		}
		File file = getBundleFile(messages + lang + ".properties"); //$NON-NLS-1$
		Properties p = new Properties();
		if ((file != null) && file.isFile()) {
			FileInputStream fis = null;
			try {
				fis = new FileInputStream(file);
				p.load(fis);
			} catch (Throwable e) {
				activator.debug(e);
			} finally {
				if (fis != null) {
					try {
						fis.close();
					} catch (IOException e) {
						activator.debug(e);
					}
				}
			}
		}
		return p;
	}
	
	public String getMessages() {
		return messages;
	}

	/**
	 * @param bundle the bundle to set
	 */
	public void setBundle(Bundle bundle) {
		this.bundle = bundle;
	}

	/**
	 * @return the bundle
	 */
	public final Bundle getBundle() {
		return bundle;
	}
	
	protected File getBundleFile(String filename) {
		// Do not use bundle classpath.
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
		}
		return activator.toFile(url);
	}

	public String getKeywords() {
		return keywords;
	}

	public void setKeywords(String keywords) {
		this.keywords = keywords;
	}
	
}
