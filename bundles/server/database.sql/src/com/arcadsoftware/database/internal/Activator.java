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
package com.arcadsoftware.database.internal;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import javax.sql.DataSource;

import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.database.DatabaseTracker;
import com.arcadsoftware.database.IDataSourceEvent;
import com.arcadsoftware.database.IDataSourceInformations;
import com.arcadsoftware.database.sql.DataSourceFactory;
import com.arcadsoftware.database.sql.DataSourceParameters;
import com.arcadsoftware.database.sql.IDataSourceProvider;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;

/**
 * This Activator ensure to role :
 * 
 * - register the query repository
 * - register a generic data source 
 * 
 */
public class Activator extends AbstractConfiguredActivator {

	protected static final String KEY_DATABASEID = ".dbid"; //$NON-NLS-1$
	protected static final String KEY_DATABASETYPE = ".type"; //$NON-NLS-1$
	protected static final String KEY_DATABASEURL = ".url"; //$NON-NLS-1$
	protected static final String KEY_DATABASELOGIN = ".login"; //$NON-NLS-1$
	protected static final String KEY_DATABASEPWD = ".password"; //$NON-NLS-1$
	protected static final String KEY_DATABASEPOOLMIN = ".poolmin"; //$NON-NLS-1$
	protected static final String KEY_DATABASEPOOLMAX = ".poolmax"; //$NON-NLS-1$
	protected static final String KEY_DATABASETIMEOUT = ".timeout"; //$NON-NLS-1$
	protected static final String KEY_DATABASEDESC = ".desc"; //$NON-NLS-1$
	protected static final String KEY_DATABASEDIALECT = ".dialect"; //$NON-NLS-1$
	
	protected static final int DEFAULT_TIMEOUT = 30000;
	protected static final int DEFAULT_POOLMIN = 0;
	protected static final int DEFAULT_POOLMAX = 20;
	
	private static Activator instance;
	
	public static Activator getInstance() {
		// la classe DataSourceFactory peut être employé en dehors d'OSGi.
		if (instance == null) {
			return new Activator();
		}
		return instance;
	}
	
	private final ArrayList<ServiceRegistration<DataSource>> currentDataSources = new ArrayList<ServiceRegistration<DataSource>>();
	private final ArrayList<DataSourceParameters> postponeDataSources = new ArrayList<DataSourceParameters>();
	private volatile ServiceTracker<DataSource, DataSource> eventtracker;
	private DataSourceInformations infos;
	private ServiceTracker<IDataSourceProvider, IDataSourceProvider> dstracker;

	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
		eventtracker = new ServiceTracker<>(context, EventAdmin.class.getName(), null);
		eventtracker.open();
		registerService(CommandProvider.class.getName(), new DatabaseCommandProvider(this));
		infos = new DataSourceInformations(this);
		registerService(IDataSourceInformations.clazz, infos);
		dstracker = new ServiceTracker<IDataSourceProvider, IDataSourceProvider>(context, IDataSourceProvider.class, new ServiceTrackerCustomizer<IDataSourceProvider, IDataSourceProvider>() {
			@Override
			public IDataSourceProvider addingService(ServiceReference<IDataSourceProvider> reference) {
				IDataSourceProvider dsp = getContext().getService(reference);
				synchronized (postponeDataSources) {
					Iterator<DataSourceParameters> itt = postponeDataSources.iterator();
					while (itt.hasNext()) {
						DataSourceParameters p = itt.next();
						if (dsp.acceptDatabaseType(p.getType())) {
							itt.remove();
							doAddDataSource(p);
						}
					}
				}
				return dsp;
			}
			@Override
			public void modifiedService(ServiceReference<IDataSourceProvider> reference, IDataSourceProvider service) {
				// Nothing to do.
			}
			@Override
			public void removedService(ServiceReference<IDataSourceProvider> reference, IDataSourceProvider service) {
				// TODO Remove the Datasource created by this service ?
			}
		});
		dstracker.open();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		dstracker.close();
		ArrayList<DataSource> dslist = new ArrayList<DataSource>();
		for (ServiceRegistration<DataSource> serviceRegistration: currentDataSources) {
			ServiceReference<DataSource> r = serviceRegistration.getReference();
			DataSource ds = context.getService(r);
			fireSyncDSChangeEvent(IDataSourceEvent.TOPIC_REMOVE,
					(String) r.getProperty(DatabaseTracker.DatabaseID),
					(String) r.getProperty(DatabaseTracker.DatabaseType),
					(String) r.getProperty(DatabaseTracker.DatabaseURL),
					(String) r.getProperty(DatabaseTracker.DatabaseDialect),
					ds); //$NON-NLS-1$
			dslist.add(ds);
		}
		currentDataSources.clear();
		postponeDataSources.clear();
		super.stop(context);
		eventtracker.close();
		eventtracker = null;
		infos = null;
		instance = null;
	}

	@Override
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		boolean changed = false;
		if (properties != null) {
			Enumeration<String> e = properties.keys();
			while (e.hasMoreElements()) {
				String key = e.nextElement();
				if (key.endsWith(KEY_DATABASEPWD)) {
					Object o = properties.get(key);
					if ((o != null) && !Crypto.isCryptSecure(o.toString())) {
						char[] pwd = Crypto.decrypt(o.toString());
						try {
							properties.put(key, Crypto.encrypt(pwd));
							changed = true;
						} finally {
							Crypto.clear(pwd);
						}
					}
				} else if (key.endsWith(KEY_DATABASEID)) {
					properties.remove(key);
					changed = true;
				}
			}
		}
		return changed;
	}

	@Override
	public void updatedConfiguration(Dictionary<String, Object> properties) {
		if (properties == null) {
			return;
		}
		for(ServiceRegistration<DataSource> serviceRegistration:currentDataSources) {
			ServiceReference<DataSource> r = serviceRegistration.getReference();
			fireDSChangeEvent(IDataSourceEvent.TOPIC_REMOVE,
					(String) r.getProperty(DatabaseTracker.DatabaseID),
					(String) r.getProperty(DatabaseTracker.DatabaseType),
					(String) r.getProperty(DatabaseTracker.DatabaseURL),
					(String) r.getProperty(DatabaseTracker.DatabaseDialect),
					getContext().getService(r)); //$NON-NLS-1$
			unregister(serviceRegistration);
		}
		currentDataSources.clear();
		synchronized (postponeDataSources) {
			postponeDataSources.clear();
			Enumeration<String> keys = properties.keys();
			while (keys.hasMoreElements()) {
				String key = keys.nextElement();
				if (key.endsWith(KEY_DATABASEID)) {
					addConnection(key.substring(0, key.length() - KEY_DATABASEID.length()), null, properties);
				} else if (key.endsWith(KEY_DATABASETYPE)) {
					String id = key.substring(0, key.length() - KEY_DATABASETYPE.length());
					if (properties.get(id + KEY_DATABASEID) == null) {
						addConnection(id, null, properties);
					}
				} else if (key.endsWith(KEY_DATABASEURL)) {
					String id = key.substring(0, key.length() - KEY_DATABASEURL.length());
					if ((properties.get(id + KEY_DATABASEID) == null) && (properties.get(id + KEY_DATABASETYPE) == null)) {
						String url = properties.get(key).toString();
						if (url.startsWith("jdbc:h2:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_H2pooled, properties);
						} else if (url.startsWith("jdbc:as400:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_DB400, properties);
						} else if (url.startsWith("jdbc:oracle:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_Oracle, properties);
						} else if (url.startsWith("jdbc:sqlserver:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_MSSqlServer, properties);
						} else if (url.startsWith("jdbc:jtds:sqlserver:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_MSSqlServerjtds, properties);
						} else if (url.startsWith("jdbc:mysql:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_MySql, properties);
						} else if (url.startsWith("jdbc:postgresql:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_PostgreSQL, properties);
						} else if (url.startsWith("jdbc:hsqldb:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_HSQLDB, properties);
						} else if (url.startsWith("jdbc:odbc:")) { //$NON-NLS-1$
							addConnection(id, IDataSourceInformations.DBTYPE_ODBC, properties);
						}
					}
				}
			}
		}
	}

	private void addConnection(final String id, String databaseType, final Dictionary<String, Object> properties) {
		String databaseID = id;
		String databaseURL = null;
		String databaseLogin = ""; //$NON-NLS-1$
		char[] databasePwd = new char[0];
		int databasePoolmin = DEFAULT_POOLMIN;
		int databasePoolmax = DEFAULT_POOLMAX;
		int databaseTimeOut = DEFAULT_TIMEOUT;
		if (properties.get(id + KEY_DATABASEID) != null) {
			databaseID = properties.get(id + KEY_DATABASEID).toString().trim();
			// id.dbid = id is not optional !
			if (databaseID.isEmpty()) {
				databaseID = id;
			}
		}
		if (properties.get(id + KEY_DATABASETYPE) != null) {
			databaseType = properties.get(id + KEY_DATABASETYPE).toString().trim();
		}
		if (properties.get(id + KEY_DATABASEURL) != null) {
			databaseURL = properties.get(id + KEY_DATABASEURL).toString().trim();
		}
		if (properties.get(id + KEY_DATABASELOGIN) != null) {
			databaseLogin = properties.get(id + KEY_DATABASELOGIN).toString();
		}
		if (properties.get(id + KEY_DATABASEPWD) != null) {
			databasePwd = Crypto.decrypt(properties.get(id + KEY_DATABASEPWD).toString());
		}
		if (properties.get(id + KEY_DATABASEPOOLMIN) != null) {
			databasePoolmin = Integer.valueOf(properties.get(id + KEY_DATABASEPOOLMIN).toString().trim());
		}
		if (properties.get(id + KEY_DATABASEPOOLMAX) != null) {
			databasePoolmax = Integer.valueOf(properties.get(id + KEY_DATABASEPOOLMAX).toString().trim());
		}
		if (properties.get(id + KEY_DATABASETIMEOUT) != null) {
			databaseTimeOut = Integer.valueOf(properties.get(id + KEY_DATABASETIMEOUT).toString().trim());
		}
		final String dialect;
		if (properties.get(id + KEY_DATABASEDIALECT) != null) {
			String s = properties.get(id + KEY_DATABASEDIALECT).toString().trim();
			if (s.length() == 0) {
				dialect = null;
			} else {
				dialect = s;
			}
		} else {
			dialect = IDataSourceInformations.BDDIALECTS_SHORT[DataSourceInformations.getDialectFromType(databaseType)];
		}
		// update the managed DataSource.
		if ((databaseID != null) && 
				(databaseID.length() > 0) &&
				(databaseType != null) && 
				(databaseType.length() > 0)) {
			final DataSourceParameters parameters = new DataSourceParameters(databaseID, databaseType, databaseURL, databaseLogin, databasePwd, databasePoolmin, databasePoolmax, databaseTimeOut, dialect);
			Enumeration<String> e = properties.keys();
			String iddot = id + '.';
			while (e.hasMoreElements()) {
				String key = e.nextElement();
				if (key.startsWith(iddot) && !(key.equals(id + KEY_DATABASEID) || key.equals(id + KEY_DATABASETYPE) || //
						key.equals(id + KEY_DATABASELOGIN) || key.equals(id + KEY_DATABASEURL) || //
						key.equals(id + KEY_DATABASEPWD) || key.equals(id + KEY_DATABASEPOOLMIN) || //
						key.equals(id + KEY_DATABASEPOOLMAX) || key.equals(id + KEY_DATABASETIMEOUT) || key.equals(id + KEY_DATABASEDIALECT))) {
					parameters.addParameter(key.substring(iddot.length()), properties.get(key));
				}
			}
			// Create a new DataSource... into another Thread, to ensure that the platform
			// will continue to process other configuration without hang.
			final Timer timer = new Timer("Delayed DataSource Creation"); //$NON-NLS-1$
			timer.schedule(new TimerTask() {
				public void run() {
					doAddDataSource(parameters);
					timer.cancel();
				}
			}, 500);
		}
	}

	private void doAddDataSource(final DataSourceParameters parameters) {
		DataSource ds = DataSourceFactory.createDataSource(this, parameters);
		if (ds != null) {
			Dictionary<String, Object> props = new Hashtable<String, Object>();
			props.put(DatabaseTracker.DatabaseID, parameters.getId());
			props.put(DatabaseTracker.DatabaseType, parameters.getType());
			if (parameters.getUrl() != null) {
				props.put(DatabaseTracker.DatabaseURL, parameters.getUrl());
			}
			if (parameters.getDialect() != null) {
				props.put(DatabaseTracker.DatabaseDialect, parameters.getDialect());
			}
			currentDataSources.add(registerService(DataSource.class, ds, props));
			info(parameters.getType() + Messages.Activator_DataSource_Declared + parameters.getId());
			fireDSChangeEvent(IDataSourceEvent.TOPIC_ADD, parameters.getId(), parameters.getType(), parameters.getUrl(), parameters.getDialect(), ds);
		} else {
			info(parameters.getId() + ": " + Messages.Activator_ConnectionWith + parameters.getType() + Messages.Activator_NotImplemented); //$NON-NLS-1$
			postponeDataSources.add(parameters);
		}
	}

	private void fireDSChangeEvent(String topic, String databaseID, String databaseType, String databaseURL, String dialect, DataSource ds) {
		if (eventtracker != null) {
			EventAdmin ea = (EventAdmin) eventtracker.getService();
			if (ea != null) {
				Dictionary<String, Object> properties = new Hashtable<String, Object>();
				if (ds != null) {
					properties.put(IDataSourceEvent.PROP_DS, ds);
				}
				if (databaseID != null) {
					properties.put(IDataSourceEvent.PROP_DSID, databaseID);
				}
				if (databaseType != null) {
					properties.put(IDataSourceEvent.PROP_DSTYPE, databaseType);
				}
				if (databaseURL != null) {
					properties.put(IDataSourceEvent.PROP_DSURL, databaseURL);
				}
				if (dialect != null) {
					properties.put(IDataSourceEvent.PROP_DSDIALECT, dialect);
				} else {
					properties.put(IDataSourceEvent.PROP_DSDIALECT, DataSourceInformations.BDDIALECTS_SHORT[DataSourceInformations.getDialectFromType(databaseType)]);
				}
				ea.postEvent(new Event(topic, properties));
			} else {
				warn(Messages.Activator_NoEventServiceAvailable);
			}
		}
	}

	private void fireSyncDSChangeEvent(String topic, String databaseID, String databaseType, String databaseURL, String dialect, DataSource ds) {
		if (eventtracker != null) {
			EventAdmin ea = (EventAdmin) eventtracker.getService();
			if (ea != null) {
				Dictionary<String, Object> properties = new Hashtable<String, Object>();
				if (ds != null) {
					properties.put(IDataSourceEvent.PROP_DS, ds);
				}
				if (databaseID != null) {
					properties.put(IDataSourceEvent.PROP_DSID, databaseID);
				}
				if (databaseType != null) {
					properties.put(IDataSourceEvent.PROP_DSTYPE, databaseType);
				}
				if (databaseURL != null) {
					properties.put(IDataSourceEvent.PROP_DSURL, databaseURL);
				}
				if (dialect != null) {
					properties.put(IDataSourceEvent.PROP_DSDIALECT, dialect);
				} else {
					properties.put(IDataSourceEvent.PROP_DSDIALECT, DataSourceInformations.BDDIALECTS_SHORT[DataSourceInformations.getDialectFromType(databaseType)]);
				}
				ea.sendEvent(new Event(topic, properties));
			} else {
				warn(Messages.Activator_NoEventServiceAvailable);
			}
		}
	}

	protected String getDatabaseProperty(String key) {
		Dictionary<String,Object> properties = getCurrentConfiguration();
		if (properties == null) {
			return null;
		}
		Object o = properties.get(key);
		if (o == null) {
			return null;
		}
		return o.toString();
	}
	
	protected String[] getDataSourcesId() {
		Dictionary<String,Object> properties = getCurrentConfiguration();
		ArrayList<String> result = new ArrayList<>(); 
		if (properties != null) {
			Enumeration<?> keys = properties.keys();
			while (keys.hasMoreElements()) {
				try {
					String key = (String)keys.nextElement();
					if (key.endsWith(KEY_DATABASEID) && (properties.get(key) != null)) {
						result.add(properties.get(key).toString());
					}
				} catch (ClassCastException e) {}
			}
		}
		return result.toArray(new String[result.size()]);
	}
	
	protected void removeDataSource(String id) {
		Dictionary<String, Object> properties = getCurrentConfiguration();
		if (properties == null) {
			return;
		}
		id = id + '.';
		ArrayList<String> kr = new ArrayList<String>();
		for(Enumeration<?> keys = properties.keys();keys.hasMoreElements();) {
			Object k = keys.nextElement();
			if (k.toString().startsWith(id)) {
				kr.add(k.toString());
			}
		}
		for(String k:kr) {
			properties.remove(k);
		}
		ConfigurationAdmin ca = (ConfigurationAdmin) getConfigurationTracker().getService();
		if (ca != null) {
			try {
				Configuration c = ca.getConfiguration(getContext().getBundle().getSymbolicName(), null);
				if (c != null) {
					c.update(properties);
				}
			} catch (IOException e) {
				error(e.getLocalizedMessage(), e);
			}
		} else {
			updateConfiguration(properties);
		}
	}

	protected void setDataSource(String id, String dbtype, String dialect, String url, String login, String pwd,
			int minpool, int maxpool, int timeout) {
		Dictionary<String, Object> props = getCurrentConfiguration();
		if (props == null) {
			props = new Hashtable<String, Object>();
			props.put(Constants.SERVICE_PID, getContext().getBundle().getSymbolicName());
		}
		props.put(id + Activator.KEY_DATABASEID, id);
		props.put(id + Activator.KEY_DATABASETYPE, dbtype);
		props.put(id + Activator.KEY_DATABASEURL, url);
		if (dialect != null) {
			props.put(id + Activator.KEY_DATABASEDIALECT, dialect);
		}
		if (login != null) {
			props.put(id + Activator.KEY_DATABASELOGIN, login);
		}
		if (pwd != null) {
			props.put(id + Activator.KEY_DATABASEPWD, pwd);
		}
		if (minpool > 0) {
			props.put(id + Activator.KEY_DATABASEPOOLMIN, Integer.toString(minpool));
		}
		if (maxpool > 0) {
			props.put(id + Activator.KEY_DATABASEPOOLMAX, Integer.toString(maxpool));
		}
		if (timeout > 0) {
			props.put(id + Activator.KEY_DATABASETIMEOUT, Integer.toString(timeout));
		}
		updateConfiguration(props);
	}

	public DataSource getDataSource(String name) {
		if (name != null) {
			for (ServiceRegistration<DataSource> serviceRegistration: currentDataSources) {
				final ServiceReference<DataSource> r = serviceRegistration.getReference();
				if (name.equalsIgnoreCase((String) r.getProperty(DatabaseTracker.DatabaseID))) {
					return getContext().getService(r);
				}
			}
		}
		return null;
	}

	public Map<String, DataSource> getDataSources() {
		HashMap<String, DataSource> result = new HashMap<>(currentDataSources.size());
		for (ServiceRegistration<DataSource> serviceRegistration: currentDataSources) {
			final ServiceReference<DataSource> r = serviceRegistration.getReference();
			DataSource ds = getContext().getService(r);
			if (ds != null) {
				result.put((String) r.getProperty(DatabaseTracker.DatabaseID), ds);
			}
		}
		return result;
	}
}
