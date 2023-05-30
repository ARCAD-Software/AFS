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
package com.arcadsoftware.cm.simple.internal;

import java.io.File;
import java.io.IOException;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.osgi.framework.Bundle;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceFactory;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.cm.ConfigurationEvent;
import org.osgi.service.cm.ConfigurationListener;
import org.osgi.service.cm.ConfigurationPermission;
import org.osgi.service.cm.ConfigurationPlugin;
import org.osgi.service.cm.ManagedService;
import org.osgi.service.cm.ManagedServiceFactory;
import org.osgi.service.cm.SynchronousConfigurationListener;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;

public class ConfigurationAdminFactory implements ServiceFactory<ConfigurationAdmin> {

	private final Activator activator;
	private final ConfigurationStorage storage;
	private final ServiceTracker<ManagedService, ManagedService> msTracker;
	private final ServiceTracker<ManagedServiceFactory, ManagedServiceFactory> msfTracker;
	private final ServiceTracker<ConfigurationPlugin, ConfigurationPlugin> cpTracker;
	private final ServiceTracker<ConfigurationListener, ConfigurationListener> listenerTracker;
	private final ServiceTracker<SynchronousConfigurationListener, SynchronousConfigurationListener> syncListenerTracker;
	
	public ConfigurationAdminFactory(Activator activator, File store) {
		super();
		this.activator = activator;
		storage = new ConfigurationStorage(store, this);
		msTracker = new ServiceTracker<ManagedService, ManagedService>(activator.getContext(), ManagedService.class, 
				new ServiceTrackerCustomizer<ManagedService, ManagedService>() {
					@Override
					public ManagedService addingService(ServiceReference<ManagedService> reference) {
						ManagedService service = ConfigurationAdminFactory.this.getActivator().getContext().getService(reference);
						ConfigurationAdminFactory.this.initManagedService(reference, service);
						return service;
					}
					@Override
					public void modifiedService(ServiceReference<ManagedService> reference, ManagedService service) {
						//ConfigurationAdminFactory.this.initManagedService(reference, service);
					}
					@Override
					public void removedService(ServiceReference<ManagedService> reference, ManagedService service) {}
		});
		msfTracker = new ServiceTracker<ManagedServiceFactory, ManagedServiceFactory>(activator.getContext(), ManagedServiceFactory.class,
				new ServiceTrackerCustomizer<ManagedServiceFactory, ManagedServiceFactory>() {
					@Override
					public ManagedServiceFactory addingService(ServiceReference<ManagedServiceFactory> reference) {
						ManagedServiceFactory service = ConfigurationAdminFactory.this.getActivator().getContext().getService(reference);
						ConfigurationAdminFactory.this.initManagedServiceFactory(reference, service);
						return service;
					}
					@Override
					public void modifiedService(ServiceReference<ManagedServiceFactory> reference, ManagedServiceFactory service) {
						//ConfigurationAdminFactory.this.initManagedServiceFactory(reference, service);
					}
					@Override
					public void removedService(ServiceReference<ManagedServiceFactory> reference, ManagedServiceFactory service) {}
				});
		cpTracker = new ServiceTracker<ConfigurationPlugin, ConfigurationPlugin>(activator.getContext(), ConfigurationPlugin.class, null);
		listenerTracker = new ServiceTracker<ConfigurationListener, ConfigurationListener>(activator.getContext(), ConfigurationListener.class, null);
		syncListenerTracker = new ServiceTracker<SynchronousConfigurationListener, SynchronousConfigurationListener>(activator.getContext(), SynchronousConfigurationListener.class, null);
	}

	public void open() {
		if (Activator.doPrivileged(new PrivilegedAction<Boolean>() {
			@Override
			public Boolean run() {
				try {
					storage.load();
					return Boolean.TRUE;
				} catch (IOException e) {
					activator.error(e);
					return Boolean.FALSE;
				}
			}
		})) {
			// There is a great chance that at this point the Logger is not configured,
			// so logging here is equivalent to print to system.out or do nothing !
			activator.debug("Configuration started and loaded. Configuration objects count: " + storage.count());
		}
		listenerTracker.open();
		syncListenerTracker.open();
		activator.registerService(ConfigurationAdmin.class, this);
		cpTracker.open();
		msTracker.open();
		msfTracker.open();
	}
	
	public void close() {
		msfTracker.close();
		msTracker.close();
		cpTracker.close();
		listenerTracker.close();
		syncListenerTracker.close();
		if (storage.isDelayed()) {
			Activator.doPrivileged(new PrivilegedAction<Object>() {
				@Override
				public Object run() {
					try {
						storage.save();
						// There is a great chance that at this point the Logger is not configured,
						// so logging here is equivalent to print to system.out or do nothing !
						activator.debug("Configuration stoped and saved.");
					} catch (IOException e) {
						activator.error(e);
					}
					return null;
				}
			});
		}
	}

	private String[] getPids(ServiceReference<?> service, String pn) {
		Object pids = service.getProperty(pn);
		if (pids instanceof String) {
			return new String[] {(String) pids};
		}
		if (pids instanceof String[]) {
			return (String[]) pids;
		}
		if (pids instanceof Collection<?>) {
			return ((Collection<?>) pids).toArray(new String[((Collection<?>) pids).size()]);
		}
		return new String[0];
	}

	private boolean hasPermission(ServiceReference<?> ref, String location) {
		if ((location != null) && (System.getSecurityManager() != null)) {
			final Bundle b = ref.getBundle();
			if (b != null) {
				String bLocation = AccessController.doPrivileged(new PrivilegedAction<String>() {
						public String run() {
							return b.getLocation();
						}
					});
				if (!location.equals(bLocation)) {
					return b.hasPermission(new ConfigurationPermission(location, ConfigurationPermission.TARGET));
				}
			}
		}
		return true;
	}

	protected void initManagedServiceFactory(ServiceReference<ManagedServiceFactory> reference, ManagedServiceFactory service) {
		final HashMap<String, Dictionary<String, Object>> confs = new HashMap<>();
		for(String pid: getPids(reference, Constants.SERVICE_PID)) {
			if (pid != null) {
				for (ConfigurationContainer c: storage.getFactoryConfigurations(pid)) {
					if (hasPermission(reference, c.getLocation())) {
						confs.put(c.getPid(), modifyConfiguration(reference, c.getFactoryPid(), c.getLocation(), c.getProperties()));
					}
				}
			}
		}
	if (confs.isEmpty()) {
		new Thread(new Runnable() {
			@Override
			public void run() {
					for(Entry<String, Dictionary<String, Object>> e: confs.entrySet()) {
						try {
							service.updated(e.getKey(), e.getValue());
						} catch (Exception x) {
							ConfigurationAdminFactory.this.activator.error("Error during configuration factory update.", x);
						}
					}
				}
			}, "ManagedServiceFactory Initilialization").start();
		}		
	}

	protected void initManagedService(final ServiceReference<ManagedService> reference, final ManagedService service) {
		final ArrayList<Dictionary<String, Object>> confs = new ArrayList<>();
		for (String pid: getPids(reference, Constants.SERVICE_PID)) {
			if (pid != null) {
				ConfigurationContainer c = storage.getConfiguration(pid);
				if ((c != null) && (c.getFactoryPid() == null)) {
					if (hasPermission(reference, c.getLocation())) {
						confs.add(modifyConfiguration(reference, c.getPid(), c.getLocation(), c.getProperties()));
					}
				}
			}
		}
		new Thread(new Runnable() {
			@Override
			public void run() {
				if (confs.isEmpty()) {
					try {
						service.updated(null);
					} catch (Exception e) {
						ConfigurationAdminFactory.this.activator.error("Error during configuration update.", e);
					}
				} else {
					for(Dictionary<String, Object> p: confs) {
						try {
							service.updated(p);
						} catch (Exception e) {
							ConfigurationAdminFactory.this.activator.error("Error during configuration update.", e);
						}
					}
				}
			}
		}, "ManagedService Initialization").start();
	}

	private Dictionary<String, Object> modifyConfiguration(ServiceReference<?> reference, String pid, String location, Dictionary<String, Object> properties) {
		ServiceReference<ConfigurationPlugin>[] srs = cpTracker.getServiceReferences();
		if (srs != null) {
			ArrayList<RankedElement<ConfigurationPlugin>> services = new ArrayList<RankedElement<ConfigurationPlugin>>(srs.length);
			for(ServiceReference<ConfigurationPlugin> sr: srs) {
				String[] pids = getPids(sr, ConfigurationPlugin.CM_TARGET);
				for (String p: pids) {
					if (pid.equals(p) && hasPermission(sr, location)) {
						Object rnk = sr.getProperty(ConfigurationPlugin.CM_RANKING);
						int r = 0;
						if (rnk instanceof Integer) {
							r = (Integer) rnk;
						}
						if (rnk instanceof String) {
							try {
								r = Integer.parseInt((String) rnk);
							} catch (NumberFormatException e) {}
						}
						ConfigurationPlugin s = cpTracker.getService(sr);
						if (s != null) {
							services.add(new RankedElement<ConfigurationPlugin>(r, s));
						}
						break;
					}
				}
			}
			if (!services.isEmpty()) {
				Collections.sort(services);
				for(RankedElement<ConfigurationPlugin> rc: services) {
					try {
						rc.getValue().modifyConfiguration(reference, properties);
					} catch (Exception e) {
						activator.error("Error during Configuration modification.", e);
					}
				}
			}
		}
		return properties;
	}

	private List<ServiceReference<ManagedService>> getManagedServices(String pid, String location) {
		ArrayList<ServiceReference<ManagedService>> result = new ArrayList<ServiceReference<ManagedService>>();
		ServiceReference<ManagedService>[] srs = msTracker.getServiceReferences();
		if (srs != null) {
			for (ServiceReference<ManagedService> sr: srs) {
				for (String p: getPids(sr, Constants.SERVICE_PID)) {
					if (pid.equals(p) && hasPermission(sr, location)) {
						result.add(sr);
						break;
					}
				}
			}
		}
		return result;
	}

	private List<ServiceReference<ManagedServiceFactory>> getManagedServiceFactories(String factorypid, String location) {
		ArrayList<ServiceReference<ManagedServiceFactory>> result = new ArrayList<ServiceReference<ManagedServiceFactory>>();
		ServiceReference<ManagedServiceFactory>[] srs = msfTracker.getServiceReferences();
		if (srs != null) {
			for (ServiceReference<ManagedServiceFactory> sr: srs) {
				for (String p: getPids(sr, Constants.SERVICE_PID)) {
					if (factorypid.equals(p) && hasPermission(sr, location)) {
						result.add(sr);
						break;
					}
				}
			}
		}
		return result;
	}

	private List<ServiceReference<ManagedService>> getManagedServices(String pid) {
		ArrayList<ServiceReference<ManagedService>> result = new ArrayList<ServiceReference<ManagedService>>();
		ServiceReference<ManagedService>[] srs = msTracker.getServiceReferences();
		if (srs != null) {
			for (ServiceReference<ManagedService> sr: srs) {
				for (String p: getPids(sr, Constants.SERVICE_PID)) {
					if (pid.equals(p)) {
						result.add(sr);
						break;
					}
				}
			}
		}
		return result;
	}

	private List<ServiceReference<ManagedServiceFactory>> getManagedServiceFactories(String factorypid) {
		ArrayList<ServiceReference<ManagedServiceFactory>> result = new ArrayList<ServiceReference<ManagedServiceFactory>>();
		ServiceReference<ManagedServiceFactory>[] srs = msfTracker.getServiceReferences();
		if (srs != null) {
			for (ServiceReference<ManagedServiceFactory> sr: srs) {
				for (String p: getPids(sr, Constants.SERVICE_PID)) {
					if (factorypid.equals(p)) {
						result.add(sr);
						break;
					}
				}
			}
		}
		return result;
	}
	
	private void callListeners(final ConfigurationEvent event) {
		ServiceReference<SynchronousConfigurationListener>[] srss = syncListenerTracker.getServiceReferences();
		if (srss != null) {
			for (ServiceReference<SynchronousConfigurationListener> sr: srss) {
				SynchronousConfigurationListener s = syncListenerTracker.getService(sr);
				if (s != null) {
					try {
						s.configurationEvent(event);
					} catch (Exception e) {
						activator.error("Error during Configuration Event processing", e);
					}
				}
			}
		}
		final ServiceReference<ConfigurationListener>[] srs = listenerTracker.getServiceReferences();
		if ((srs != null) && (srs.length > 0)) {
			new Thread(new Runnable() {
				@Override
				public void run() {
					for (ServiceReference<ConfigurationListener> sr: srs) {
						ConfigurationListener s = ConfigurationAdminFactory.this.listenerTracker.getService(sr);
						if (s != null) {
							try {
								s.configurationEvent(event);
							} catch (Exception e) {
								ConfigurationAdminFactory.this.activator.error("Error during Configuration Event processing", e);
							}
						}
					}
				}
			}, "Configuration Event Dispatcher").start();
		}
	}

	public void configurationDeletion(ServiceReference<ConfigurationAdmin> reference, ConfigurationContainer conf) {
		if (conf.getFactoryPid() != null) {
			final List<ServiceReference<ManagedServiceFactory>> services = getManagedServiceFactories(conf.getFactoryPid(), conf.getLocation());
			if (!services.isEmpty()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						for(ServiceReference<ManagedServiceFactory> sr: services) {
							ManagedServiceFactory service = ConfigurationAdminFactory.this.msfTracker.getService(sr);
							if (service != null) {
								try {
									service.deleted(conf.getPid());
								} catch (Exception e) {
									ConfigurationAdminFactory.this.activator.error("Error during configuration factory delete.", e);
								}
							}
						}
					}
				}, "Configuration deletion from factory").start();
			}
		} else {
			final List<ServiceReference<ManagedService>> services = getManagedServices(conf.getPid(), conf.getLocation());
			if (!services.isEmpty()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						for(ServiceReference<ManagedService> sr: services) {
							ManagedService service = ConfigurationAdminFactory.this.msTracker.getService(sr);
							if (service != null) {
								try {
									service.updated(null);
								} catch (Exception e) {
									ConfigurationAdminFactory.this.activator.error("Error during configuration delete update.", e);
								}
							}
						}
					}
				}, "Configuration deletion").start();
			}
		}
		if (reference != null) {
			callListeners(new ConfigurationEvent(reference, ConfigurationEvent.CM_DELETED, conf.getFactoryPid(), conf.getPid()));
		}
	}
	
	public void configurationUpdateLocation(ServiceReference<ConfigurationAdmin> reference, ConfigurationContainer conf, String oldLocation) {
		if (conf.getFactoryPid() != null) {
			final List<ServiceReference<ManagedServiceFactory>> services = getManagedServiceFactories(conf.getFactoryPid());
			if (!services.isEmpty()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						for(ServiceReference<ManagedServiceFactory> sr: services) {
							boolean deletion = !hasPermission(sr, conf.getLocation());
							if (!deletion || hasPermission(sr, oldLocation)) {
								ManagedServiceFactory service = ConfigurationAdminFactory.this.msfTracker.getService(sr);
								if (service != null) {
									try {
										if (deletion) {
											service.deleted(conf.getPid());
										} else {
											service.updated(conf.getPid(), modifyConfiguration(sr, conf.getPid(), conf.getLocation(), conf.getProperties()));
										}
									} catch (Exception e) {
										ConfigurationAdminFactory.this.activator.error("Error during configuration factory update.", e);
									}
								}
							}
						}
					}
				}, "Configuration update from factory").start();
			}
		} else {
			final List<ServiceReference<ManagedService>> services = getManagedServices(conf.getPid());
			if (!services.isEmpty()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						for(ServiceReference<ManagedService> sr: services) {
							boolean deletion = !hasPermission(sr, conf.getLocation());
							if (!deletion || hasPermission(sr, oldLocation)) {
								ManagedService service = ConfigurationAdminFactory.this.msTracker.getService(sr);
								if (service != null) {
									try {
										if (deletion) {
											service.updated(null);
										} else {
											service.updated(modifyConfiguration(sr, conf.getPid(), conf.getLocation(), conf.getProperties()));
										}
									} catch (Exception e) {
										ConfigurationAdminFactory.this.activator.error("Error during configuration update.", e);
									}
								}
							}
						}
					}
				}, "Configuration update").start();
			}
		}
		if (reference != null) {
			callListeners(new ConfigurationEvent(reference, ConfigurationEvent.CM_LOCATION_CHANGED, conf.getFactoryPid(), conf.getPid()));
		}
	}
	
	public void configurationUpdate(ServiceReference<ConfigurationAdmin> reference, ConfigurationContainer conf) {
		if (conf.getFactoryPid() != null) {
			final List<ServiceReference<ManagedServiceFactory>> services = getManagedServiceFactories(conf.getFactoryPid(), conf.getLocation());
			if (!services.isEmpty()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						for(ServiceReference<ManagedServiceFactory> sr: services) {
							ManagedServiceFactory service = ConfigurationAdminFactory.this.msfTracker.getService(sr);
							if (service != null) {
								Dictionary<String, Object> props = modifyConfiguration(sr, conf.getPid(), conf.getLocation(), conf.getProperties());
								try {
									service.updated(conf.getPid(), props);
								} catch (Exception e) {
									ConfigurationAdminFactory.this.activator.error("Error during configuration factory update.", e);
								}
							}
						}
					}
				}, "Configuration update from factory").start();
			}
		} else {
			final List<ServiceReference<ManagedService>> services = getManagedServices(conf.getPid(), conf.getLocation());
			if (!services.isEmpty()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						for(ServiceReference<ManagedService> sr: services) {
							ManagedService service = ConfigurationAdminFactory.this.msTracker.getService(sr);
							if (service != null) {
								try {
									service.updated(modifyConfiguration(sr, conf.getPid(), conf.getLocation(), conf.getProperties()));
								} catch (Exception e) {
									ConfigurationAdminFactory.this.activator.error("Error during configuration update.", e);
								}
							}
						}
					}
				}, "Configuration update").start();
			}
		}
		if (reference != null) {
			callListeners(new ConfigurationEvent(reference, ConfigurationEvent.CM_UPDATED, conf.getFactoryPid(), conf.getPid()));
		}
	}
	
	public Activator getActivator() {
		return activator;
	}

	public ConfigurationStorage getStorage() {
		return storage;
	}

	@Override
	public ConfigurationAdmin getService(Bundle bundle, ServiceRegistration<ConfigurationAdmin> registration) {
		return new ConfigurationAdminImpl(bundle, registration, this);
	}

	@Override
	public void ungetService(Bundle bundle, ServiceRegistration<ConfigurationAdmin> registration, ConfigurationAdmin service) {}
}