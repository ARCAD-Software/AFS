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
package com.arcadsoftware.metadata.sql.internal;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;

import javax.sql.DataSource;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;

import com.arcadsoftware.database.DatabaseTracker;
import com.arcadsoftware.database.IDataSourceEvent;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataEventHandler;
import com.arcadsoftware.metadata.sql.MapperSQLService;
import com.arcadsoftware.osgi.AbstractActivator;

@SuppressWarnings("rawtypes")
public class Activator extends AbstractActivator implements EventHandler, ServiceTrackerCustomizer, CommandProvider {
	
	private static final String DOMAIN_JDBCPREFIX = "jdbc:"; //$NON-NLS-1$
	private static final File sqltrace;
	private static final boolean sqlTiming = Boolean.getBoolean("com.arcadsoftware.trace.sql.requests.timing"); //$NON-NLS-1$
	
	static {
		String s = System.getProperty("com.arcadsoftware.trace.sql.requests"); //$NON-NLS-1$
		if ((s != null) && !s.trim().isEmpty()) {
			File f = new File(s);
			if (!f.isFile()) {
				f.getParentFile().mkdirs();
				try {
					f.createNewFile();
				} catch (IOException e) {
					f = null;
				}
			}
			sqltrace = f;
		} else {
			sqltrace = null;
		}
	}
	
	private static Activator instance;
	
	public static Activator getInstance() {
		return instance;
	}

	private volatile PrintWriter trace;
	private HashMap<String, ServiceRegistration> map;
	private ServiceTracker tracker;
	
	@SuppressWarnings("unchecked")
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		instance = this;
		super.start(bundleContext);
		if (sqltrace != null) {
			try {
				trace = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(sqltrace, true))), true);
				trace.println("-- " + new Date().toString()); //$NON-NLS-1$
			} catch (Exception e) {}
		}
		map = new HashMap<String, ServiceRegistration>();
		tracker = new ServiceTracker(bundleContext, DataSource.class.getName(), this);
		tracker.open();
		registerService(EventHandler.class, this, EventConstants.EVENT_TOPIC, //
				new String[] {IDataSourceEvent.TOPIC_ALL, MetaDataEventHandler.TOPIC_ENTITY_ALLEVENTS});
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		map.clear();
		if (tracker != null) {
			try {
				tracker.close();
			} catch (Exception e) {}
		}
		if (trace != null) {
			try {
				trace.flush();
				trace.close();
			} catch (Exception e) {}
			trace = null;
		}
		instance = null;
	}

	@Override
	public void handleEvent(Event event) {
		if (event.getTopic().startsWith(MetaDataEventHandler.PREFIX_TOPIC_ENTITY)) {
			// Traitement nécessaire suite à redéclaration d'une entité.
			MetaDataEntity entity = (MetaDataEntity) event.getProperty(MetaDataEventHandler.EVENT_PROP_ENTITY);
			if (entity != null) {
				IMapperService mapper = entity.getMapper(); 
				// provoque l'association du mapper.
				if (mapper instanceof MapperSQLService) {
					// Purge du cache SQL !
					((MapperSQLService) mapper).purgeCache(entity);
					// L'information sera automatiquement reconstruite à la demande.
				}
			}
		}
	}

	private void createMapper(final String dsid, final DataSource ds, final String dialect) {
		// Création du mapper dans un autre thread...
		new Thread(new Runnable() {
			public void run() {
				String d = null;
				if (dialect != null) {
					d = dialect.toLowerCase();
				}
				String domain = DOMAIN_JDBCPREFIX + dsid;
				MapperSQLService mapper = new MapperSQLService(domain, ds, d);
				Dictionary<String, Object> props = new Hashtable<String, Object>();
				props.put(MapperSQLService.PROP_DOMAINNAME, domain);
				props.put(MapperSQLService.PROP_SUPPORT_EXTRAREFERENCES, true);
				props.put(MapperSQLService.PROP_SUPPORT_GROUPSENTITY, false);
				props.put(MapperSQLService.PROP_SUPPORT_MULTILINKREFERENCES, true);
				props.put(MapperSQLService.PROP_SUPPORT_PAGINATION, true);
				props.put(MapperSQLService.PROP_SUPPORT_SOFTDELETION, true);
				ServiceRegistration mr = registerService(MapperSQLService.clazz, //
						mapper, //
						props);
				map.put(domain, mr);
				debug(Messages.Activator_MapperRegistered + domain);
			}
		},"Creation of Mapper " + dsid).start(); //$NON-NLS-1$
	}

	public Object addingService(ServiceReference reference) {
		// Ajout de la source de données.
		String dsid = (String) reference.getProperty(DatabaseTracker.DatabaseID);
		if (dsid != null) {
			String dialect = (String) reference.getProperty(DatabaseTracker.DatabaseDialect);
			if (dialect == null) {
				dialect = (String) reference.getProperty(DatabaseTracker.DatabaseType);
			}
			@SuppressWarnings("unchecked")
			DataSource ds = (DataSource) getContext().getService(reference);
			createMapper(dsid, ds, dialect);
			return ds;
		}
		return null;
	}

	public void modifiedService(ServiceReference reference, Object service) {}

	public void removedService(ServiceReference reference, Object service) {
		String dsid = (String)reference.getProperty(DatabaseTracker.DatabaseID);
		if (dsid != null) {
			 ServiceRegistration mr = map.get(DOMAIN_JDBCPREFIX + dsid);
			if (mr != null) {
				unregister(mr);
			}
		}
	}

	@Override
	public String getHelp() {
		return "\tpurgeSQLMapper <entity> - Purge the SQL mapper cache of the given entity.\n";
	}

	public void _purgeSQLMapper(CommandInterpreter ci) throws Exception {
		String en = ci.nextArgument();
		if (en == null) {
			ci.println("Usage:");
			ci.println("\tpurgeSQLMapper <entity> - Purge the SQL mapper cache of the given entity.\n");
			return;
		}
		MetaDataEntity entity = MetaDataEntity.loadEntity(en);
		if (entity == null) {
			ci.println(String.format("No Entity \"%s\" found.", en));
			return;
		}
		IMapperService mapper = entity.getMapper(); 
		// provoque l'association du mapper.
		if (mapper instanceof MapperSQLService) {
			// Purge du cache SQL !
			((MapperSQLService) mapper).purgeCache(entity);
			// L'information sera automatiquement reconstruite à la demande.
			ci.println("SQL mapper cache pruged.");
		} else {
			ci.println("This entity is not associated to an SQL Mapper.");
		}
	}
	
	public void trace(String query, long t) {
		if (trace != null) {
			if (sqlTiming) {
				t = System.currentTimeMillis() - t;
				trace.println(query + " -- " + t + "ms");
			} else {
				trace.println(query);
			}
		}
	}
}
