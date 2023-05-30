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
package com.arcadsoftware.rest.console.internal;

import java.io.File;
import java.io.FileInputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;
import org.restlet.Context;
import org.restlet.data.Language;
import org.restlet.routing.Router;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;
import com.arcadsoftware.rest.XStreamCompact;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleMessage;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.ConsoleSet;
import com.arcadsoftware.rest.console.ConsoleText;
import com.arcadsoftware.rest.console.IConsoleActionService;
import com.arcadsoftware.rest.console.IRestConsoleSection;
import com.arcadsoftware.rest.console.internal.sections.AbstractSection;
import com.arcadsoftware.rest.console.internal.sections.ConfigSection;
import com.arcadsoftware.rest.console.internal.sections.ISection;
import com.arcadsoftware.rest.console.internal.sections.MultiConfigSection;
import com.arcadsoftware.rest.console.internal.sections.ScriptSection;
import com.arcadsoftware.rest.console.internal.sections.Section;

@SuppressWarnings("rawtypes")
public class Activator extends AbstractConfiguredActivator implements BundleListener, ServiceTrackerCustomizer {

	private static final String DEFAULTSECTIONSFILENAME = "/META-INF/consoles.xml"; //$NON-NLS-1$
	private static final String SECTIONSHEADER = "Arcad-ConsoleSections"; //$NON-NLS-1$
	private static final String EQUINOX_COMMON = "org.eclipse.equinox.common"; //$NON-NLS-1$
	private static final String PROP_ADMINCONFENABLED = "admin.config.enabled"; //$NON-NLS-1$

	private static Activator instance;

	static Activator getInstance() {
		return instance;
	}

	private XStreamCompact xs;
	private HashMap<String, IRestConsoleSection> sections;
	private ServiceTracker tracker;
	private ServiceTracker consoleActionTracker;
	private MultiLanguageMessages messages;
	private HashMap<Long, ArrayList<ServiceRegistration>> foreignSections;
	private volatile boolean adminConfigEnabled = true;

	@SuppressWarnings("unchecked")
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		instance = this;
		super.start(bundleContext);
		foreignSections = new HashMap<Long, ArrayList<ServiceRegistration>>();
		sections = new HashMap<String, IRestConsoleSection>();
		messages = new MultiLanguageMessages(
				Activator.class.getPackage().getName() + ".clientmessages", Activator.class.getClassLoader()); //$NON-NLS-1$
		xs = new XStreamCompact(Activator.class.getClassLoader());
		xs.alias("sections", ArrayList.class); //$NON-NLS-1$
		xs.alias("list", ArrayList.class); //$NON-NLS-1$
		xs.addImplicitCollection(Section.class, "list"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "id"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "title"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "label"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "category"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "order"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "messages"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "help"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "icon"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractSection.class, "keywords"); //$NON-NLS-1$
		xs.alias("configuration", ConfigSection.class); //$NON-NLS-1$
		xs.useAttributeFor(ConfigSection.class, "pid"); //$NON-NLS-1$
		xs.useAttributeFor(ConfigSection.class, "bid"); //$NON-NLS-1$
		xs.alias("section", ScriptSection.class); //$NON-NLS-1$
		xs.alias("multiconfiguration", MultiConfigSection.class); //$NON-NLS-1$
		xs.addImplicitCollection(MultiConfigSection.class, "sections"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleField.class, "label"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleField.class, "icon"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleField.class, "help"); //$NON-NLS-1$
		xs.alias("action", ConsoleAction.class); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleAction.class, "id"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleAction.class, "hidden"); //$NON-NLS-1$
		xs.registerConverter(new ActionConverter()); //$NON-NLS-1$
		xs.alias("set", ConsoleSet.class); //$NON-NLS-1$
		xs.alias("text", ConsoleText.class); //$NON-NLS-1$
		xs.alias("property", ConsoleProperty.class); //$NON-NLS-1$
		xs.addImplicitCollection(ConsoleProperty.class, "list"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleProperty.class, "id"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleProperty.class, "password"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleProperty.class, "defaultvalue"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleProperty.class, "readonly"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleProperty.class, "hidden"); //$NON-NLS-1$
		xs.useAttributeFor(ConsoleProperty.class, "type"); //$NON-NLS-1$
		xs.aliasAttribute("default", "defaultvalue"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.useAttributeFor(ConsoleProperty.class, "id"); //$NON-NLS-1$
		xs.aliasAttribute("sid", "id"); //$NON-NLS-1$ //$NON-NLS-2$
		registerService(SimpleBranch.clazz, new SimpleBranch() {
			@Override
			protected RouteList createAttachedResources(Context context, Router router) {
				RouteList list = new RouteList();
				list.add(router.attach("/remote/console/", SectionsListResource.class)); //$NON-NLS-1$
				list.add(router.attach("/remote/console/{section}", SectionResource.class)); //$NON-NLS-1$
				list.add(router.attach("/remote/console/{section}/{action}", ActionResource.class)); //$NON-NLS-1$
				list.add(router.attach("/admin/config/", ConfigListResource.class)); //$NON-NLS-1$
				list.add(router.attach("/admin/config/{pid}", ConfigResource.class)); //$NON-NLS-1$
				list.add(router.attach("/config/", ConfigPublicListResource.class)); //$NON-NLS-1$
				list.add(router.attach("/config/{pid}", ConfigPublicResource.class)); //$NON-NLS-1$
				list.add(router.attach("/config/{pid}/{prop}", ConfigPublicResource.class)); //$NON-NLS-1$
				return list;
			}
		}, SimpleBranch.properties(SimpleBranch.SECUREDBRANCH));
		tracker = new ServiceTracker(bundleContext, IRestConsoleSection.clazz, this);
		tracker.open();
		consoleActionTracker = new ServiceTracker(bundleContext, IConsoleActionService.clazz, null);
		consoleActionTracker.open();
		if (isCommonStarted()) {
			proceedDelayedBundleScan();
		}
		bundleContext.addBundleListener(this);
	}

	private void proceedDelayedBundleScan() {
		for (Bundle bundle : getContext().getBundles()) {
			if (bundle.getState() == Bundle.ACTIVE) {
				addBundle(bundle);
			}
		}
	}

	private boolean isCommonStarted() {
		for (Bundle b : getContext().getBundles()) {
			if (EQUINOX_COMMON.equals(b.getSymbolicName()) && (b.getState() == Bundle.ACTIVE)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		bundleContext.removeBundleListener(this);
		foreignSections.clear();
		tracker.close();
		consoleActionTracker.close();
		super.stop(bundleContext);
		foreignSections = null;
		instance = null;
	}

	@Override
	public void updatedConfiguration(Dictionary<String,Object> properties) {
		if (properties != null) {
			if (properties.get(PROP_ADMINCONFENABLED) != null) {
				adminConfigEnabled = "true".equalsIgnoreCase(properties.get(PROP_ADMINCONFENABLED).toString()); //$NON-NLS-1$
			} else {
				adminConfigEnabled = true;
			}
		}
	}

	public XStreamCompact getXStream() {
		return xs;
	}

	public void bundleChanged(BundleEvent event) {
		switch (event.getType()) {
		case BundleEvent.STARTED:
			if (EQUINOX_COMMON.equals(event.getBundle().getSymbolicName())) {
				proceedDelayedBundleScan();
			} else if (isCommonStarted()) {
				addBundle(event.getBundle());
			}
			break;
		case BundleEvent.STOPPED:
			removeBundle(event.getBundle());
			break;
		}
	}

	private String getSectionsParent(Bundle bundle) {
		String filename = DEFAULTSECTIONSFILENAME;
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(SECTIONSHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				filename = o.toString();
			}
		}
		if (filename.indexOf('/') > 0) {
			return filename.substring(0, filename.lastIndexOf('/'));
		}
		return ""; //$NON-NLS-1$
	}

	private File getSectionsFile(Bundle bundle) {
		String filename = DEFAULTSECTIONSFILENAME;
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(SECTIONSHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				filename = o.toString();
			}
		}
		// Do not use bundle classpath.
		return getBundleFile(bundle, filename);
	}

	public File getBundleFile(String filename) {
		return getBundleFile(getContext().getBundle(), filename);
	}

	public File getBundleFile(Bundle bundle, String filename) {
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
		}
		return toFile(url);
	}

	public void removeBundle(Bundle bundle) {
		if (foreignSections != null) {
			ArrayList<ServiceRegistration> regs = foreignSections.remove(bundle.getBundleId());
			if (regs != null) {
				for (ServiceRegistration reg : regs) {
					try {
						unregister(reg);
					} catch (IllegalStateException e) {
						// The service has been already unregistered.
						debug(e);
					}
				}
			}
		}
	}

	public void addBundle(Bundle bundle) {
		// Ignorer tout les bundles qui n'auront jamais de console !
		if (!(bundle.getSymbolicName().startsWith("com.arcadsoftware.") || //$NON-NLS-1$
				bundle.getSymbolicName().startsWith("com.dropssoftware."))) { //$NON-NLS-1$
			// include "com.ibm.icu"
			// "org.codehaus.groovy"
			// "groovy"
			return;
		}
		File file = getSectionsFile(bundle);
		if ((file != null) && file.isFile()) { //$NON-NLS-1$
			Object o = null;
			try (FileInputStream fis = new FileInputStream(file)) {
				o = xs.fromXML(fis);
			} catch (Exception e) {
				error(e.getLocalizedMessage(), e);
			}
			ArrayList<ServiceRegistration> regs = foreignSections.get(bundle.getBundleId());
			if (regs == null) {
				regs = new ArrayList<ServiceRegistration>();
				foreignSections.put(bundle.getBundleId(), regs);
			}
			if (o instanceof ISection) {
				((ISection) o).setBundle(bundle);
				((ISection) o).setdefaultMessageFile(getSectionsParent(bundle));
				((ISection) o).setActivator(this);
				regs.add(registerService(IRestConsoleSection.clazz, o));
			} else if (o instanceof ArrayList<?>) {
				for (Object x : (ArrayList<?>) o) {
					if (x instanceof ISection) {
						((ISection) x).setBundle(bundle);
						((ISection) x).setdefaultMessageFile(getSectionsParent(bundle));
						((ISection) x).setActivator(this);
						regs.add(registerService(IRestConsoleSection.clazz, x));
					}
				}
			}
		}
	}

	public Object addingService(ServiceReference reference) {
		@SuppressWarnings("unchecked")
		Object section = getContext().getService(reference);
		if (section instanceof IRestConsoleSection) {
			add((IRestConsoleSection) section);
			return section;
		}
		return null;
	}

	public void modifiedService(ServiceReference reference, Object service) {
	}

	public void removedService(ServiceReference reference, Object service) {
		if (service instanceof IRestConsoleSection) {
			remove((IRestConsoleSection) service);
		}
	}

	public void add(IRestConsoleSection section) {
		debug(Messages.Section_NewSectionInstalled + section.getId());
		sections.put(section.getId(), section);
	}

	public void remove(IRestConsoleSection service) {
		sections.remove(service.getId());
	}

	public String localize(String label, Language language) {
		return messages.get(label, language);
	}

	public IRestConsoleSection getSection(String id) {
		return sections.get(id);
	}

	public Collection<IRestConsoleSection> getSections() {
		return sections.values();
	}

	public ConfigurationAdmin getConfigurationAdmin() {
		return (ConfigurationAdmin) getConfigurationTracker().getService();
	}

	public List<ConsoleField> infoForm(String label, String message, String help, int icon, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(1);
		result.add(new ConsoleMessage(localize(label, language), localize(message, language), ConsoleField.ICON_NONE,
				localize(help, language)));
		return result;
	}

	public List<ConsoleField> errorForm(String label, String message, String help, int icon, Language language) {
		ArrayList<ConsoleField> result = new ArrayList<ConsoleField>(1);
		ConsoleMessage msg = new ConsoleMessage(localize(label, language), localize(message, language), ConsoleField.ICON_ERROR, localize(help, language));
		msg.setMessageType(ConsoleMessage.ERROR);
		result.add(msg);
		return result;
	}

	public boolean isAdminConfigEnabled() {
		return adminConfigEnabled;
	}

	@SuppressWarnings("unchecked")
	public IConsoleActionService[] getConsoleActionServices(String pid) {
		if (pid == null) {
			return new IConsoleActionService[0];
		}
		ArrayList<IConsoleActionService> result = new ArrayList<IConsoleActionService>();
		ServiceReference[] srs = consoleActionTracker.getServiceReferences();
		if (srs != null) {
			for (ServiceReference sr: srs) {
				Object secid = sr.getProperty(IConsoleActionService.PROP_SECTIONIDS);
				if (secid != null) {
					String[] sids = secid.toString().split(" "); //$NON-NLS-1$
					if (sids != null) {
						for(String sid: sids) {
							if (pid.equalsIgnoreCase(sid)) {
								result.add((IConsoleActionService) getContext().getService(sr));
								break;
							}
						}
					}
				}
			}
		}
		return result.toArray(new IConsoleActionService[result.size()]);
	}

}
