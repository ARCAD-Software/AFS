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
package com.arcadsoftware.server.properties.internal;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Dictionary;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Locale;
import java.util.Properties;
import java.util.ResourceBundle;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.restlet.data.Language;

import com.arcadsoftware.osgi.AbstractFileRepositoryActivator;
import com.arcadsoftware.osgi.FileSystemTracker;
import com.arcadsoftware.osgi.IProperties;
import com.arcadsoftware.rest.ITranslationService;
import com.arcadsoftware.rest.MultiLanguageMessages;

public class Activator extends AbstractFileRepositoryActivator implements ITranslationService, IProperties, BundleListener, CommandProvider {

	private static final String DEFAULTBUNDLEFOLDER = "files/properties"; //$NON-NLS-1$
	private static final String ROOTDEFAULTBUNDLEFOLDER = '/' + DEFAULTBUNDLEFOLDER;
	private static final String SECTIONSHEADER = "Arcad-PropertiesDomain"; //$NON-NLS-1$
	private static final String GROOVY = "groovy"; //$NON-NLS-1$
	private static final String ORG_CODEHAUS_GROOVY = "org.codehaus.groovy"; //$NON-NLS-1$
	private static final String DYNAMICIMPORT = "DynamicImport-Package"; //$NON-NLS-1$
	private static final String PROPERTIES_EXT = ".properties"; //$NON-NLS-1$
	
	public class PropertiesDated {
		public Properties properties;
		public Date date;
	}
	
	private static Activator instance;
	
	public static Activator getInstance() {
		return instance;
	}

	private final HashMap<String, HashMap<Language,PropertiesDated>> cache = new HashMap<String, HashMap<Language,PropertiesDated>>();
	private final HashMap<Bundle, ArrayList<String>> usedBundles = new HashMap<Bundle, ArrayList<String>>();
	
	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
		registerService(Branch.clazz, new Branch(), Branch.properties(Branch.ROOTBRANCH));
		registerService(BranchSecured.clazz, new BranchSecured(), BranchSecured.properties(BranchSecured.SECUREDBRANCH));
		registerService(ITranslationService.clazz, this);
		registerService(IProperties.clazz, this);
		registerService(CommandProvider.class.getName(), this);
		context.addBundleListener(this);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		instance = null;
	}

	@Override
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		if (properties != null) {
			if (properties.get(PROP_WATCHPERIOD) == null) {
				properties.put(PROP_WATCHPERIOD, 120);
			}
		}
		return false;
	}

	@Override
	protected String getDefaultDirName() {
		return DEFAULTBUNDLEFOLDER;
	}

	@Override
	protected String getFileExtension() {
		return PROPERTIES_EXT;
	}

	@Override
	public String translate(String domainName, String code, Language language) {
		PropertiesDated p = getTranslations(domainName, language);
		if ((p == null) || (p.properties == null)) {
			return code;
		}
		Object result = p.properties.getProperty(code);
		if (result == null) {
			return code;
		}
		return result.toString();
	}
	
	public PropertiesDated getTranslations(String domainName, Language language) {
		HashMap<Language, PropertiesDated> props = cache.get(domainName);
		if (props != null) {
			PropertiesDated p = props.get(language);
			if (p != null) {
				return p;
			}
		} else {
			props = new HashMap<Language, PropertiesDated>();
			cache.put(domainName,props);
		}
		PropertiesDated p = loadFromFile(domainName, language);
		props.put(language, p);
		return p;
	}
	
	/*
	 * 9 phases :
	 * A. loading file.properties:
	 *  1. from internal files.
	 *  2. from bundles.
	 *  3. from external files.
	 * B. loading file_lg.properties:
	 *  1. from internal files.
	 *  2. from bundles.
	 *  3. from external files.
	 * C. loading file_lg_cn.properties:
	 *  1. from internal files.
	 *  2. from bundles.
	 *  3. from external files.
	 */
	private PropertiesDated loadFromFile(String domainName, Language language) {
		Calendar lm = new GregorianCalendar();
		lm.setTime(new Date(0)); // very old date...
		Properties props = null;
		// A. Load reference properties.
		props = loadFromSources(domainName, '/' + domainName + PROPERTIES_EXT, lm, props);
		// B. Add Language version.
		String lang = MultiLanguageMessages.getLangCode(language);
		props = loadFromSources(domainName, '/' + domainName + '_' + lang + PROPERTIES_EXT, lm, props);
		// Add Country version.
		String country = MultiLanguageMessages.getCountryCode(language);
		if (country != null) {
			props = loadFromSources(domainName, '/' + domainName + '_' + lang  + '_' + country + PROPERTIES_EXT, lm, props);
		}
		PropertiesDated pd = new PropertiesDated();
		pd.properties = props;
		pd.date = lm.getTime();
		return pd;
	}

	private Properties loadFromSources(String domainName, String filename, Calendar lm, Properties props) {
		// Following loadings will override the previous loaded properties:
		// First: from internal repository
		File file = Activator.getInstance().getInternalFile(filename, lm); //$NON-NLS-1$
		if (file != null) {
			props = loadProperties(file, props);
		}
		Calendar cal = new GregorianCalendar();
		// Second: from other bundles
		for(Bundle b:getContext().getBundles()) {
			if (!isIgnored(b)) {
				for(String d:getBundleDomains(b)) {
					if (domainName.equals(d.trim())) {
						file = getBundleFile(b, ROOTDEFAULTBUNDLEFOLDER + filename);
						if (file != null) {
							ArrayList<String> bdl = usedBundles.get(b);
							if (bdl == null) {
								bdl = new ArrayList<String>();
								usedBundles.put(b, bdl);
							}
							bdl.add(domainName);
							props = loadProperties(file, props);
							cal.setTime(new Date(b.getLastModified()));
							if (cal.after(lm)) {
								lm.setTime(cal.getTime());
							}
						}
					}
				}
			}
		}
		// Third: from external repository.
		file = Activator.getInstance().getExternalFile(filename); //$NON-NLS-1$
		if ((file != null) && file.isFile()) {
			props = loadProperties(file, props);
			cal.setTime(new Date(file.lastModified()));
			if (cal.after(lm)) {
				lm.setTime(cal.getTime());
			}
		}
		return props;
	}

	private Properties loadProperties(File file, Properties props) {
		Properties result = new Properties(props);
		try (FileInputStream fis = new FileInputStream(file)) {
			result.load(fis);
		} catch (Exception e) {
			error("Error while loading properties in file \"" + file.getAbsolutePath() + "\": " + e.getLocalizedMessage(), e);
		}
		return result;
	}

	public void fileChanged(String filename) {
		String dm = filename;
		if (dm.charAt(0) == '/') {
			dm = dm.substring(1);
		}
		if (dm.endsWith(PROPERTIES_EXT)) {
			dm = dm.substring(0, dm.length() - 11);
		}
		if (dm.charAt(dm.length() - 3) == '_') {
			dm = dm.substring(0,dm.length() - 3);
		}
		if (dm.charAt(dm.length() - 3) == '_') {
			dm = dm.substring(0,dm.length() - 3);
		}
		cache.remove(dm);
	}

	@Override
	public boolean addFile(FileSystemTracker tracker, String name, File file) {
		fileChanged(name);
		return true;
	}

	@Override
	public void updateFile(FileSystemTracker tracker, String name, File file) {
		fileChanged(name);
	}

	@Override
	public void removeFile(FileSystemTracker tracker, String name, File file) {
		fileChanged(name);
	}

	public String get(String properties, String key) {
		return translate(properties, key, Language.ENGLISH_US);
	}

	public ResourceBundle get(String properties) {
		return null;
	}

	public String get(String properties, String key, Locale locale) {
		return translate(properties, key, MultiLanguageMessages.toLanguage(locale));
	}

	public ResourceBundle get(String properties, Locale locale) {
		return null;
	}

	public boolean containsKey(String properties, String key) {
		PropertiesDated p = getTranslations(properties, Language.ENGLISH_US);
		if ((p == null) || (p.properties == null)) {
			return false;
		}
		return p.properties.get(key) != null;
	}
	
	protected boolean isIgnored(Bundle bundle) {
		if (ORG_CODEHAUS_GROOVY.equals(bundle.getSymbolicName()) ||
			GROOVY.equals(bundle.getSymbolicName())) {
			return true;
		}
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(DYNAMICIMPORT);
			if ((o != null) && (o.toString().length() > 0)) {
				return "*".equals(o.toString().trim()); //$NON-NLS-1$
			}
		}
		return false;
	}

	public void bundleChanged(BundleEvent event) {
		if (!isIgnored(event.getBundle())) {
			switch(event.getType()) {
			case BundleEvent.RESOLVED:
				for(String domain:getBundleDomains(event.getBundle())) {
					cache.remove(domain.trim());
				}
				break;
			case BundleEvent.UNRESOLVED:
				ArrayList<String> dl = usedBundles.get(event.getBundle());
				if (dl != null) {
					for(String domain:dl) {
						cache.remove(domain);
					}
					usedBundles.remove(event.getBundle());
				}
				break;
			}
		}
	}

	private String[] getBundleDomains(Bundle bundle) {
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(SECTIONSHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				return o.toString().split(","); //$NON-NLS-1$
			}
		}
		return new String[0];
	}

	public String getHelp() {
		return Messages.Activator_Help;
	}

	public void _purgeP(CommandInterpreter ci) throws Exception {
		_purgeProperties(ci);
	}

	public void _purgeProps(CommandInterpreter ci) throws Exception {
		_purgeProperties(ci);
	}

	public void _purgeProperties(CommandInterpreter ci) throws Exception {
		String s = ci.nextArgument();
		if (s == null) {
			ci.println(String.format(Messages.Activator_Command_PurgeAll, cache.size()));
			cache.clear();
		} else if (cache.remove(s) == null) {
			ci.println(String.format(Messages.Activator_Command_PurgeNone, cache.size()));
		} else {
			ci.println(String.format(Messages.Activator_Command_Purge, s));
		}
	}
}
