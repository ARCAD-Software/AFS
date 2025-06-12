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
package com.arcadsoftware.databse.sql.h2.internal;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Properties;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.h2.tools.Server;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;

import com.arcadsoftware.database.sql.IDataSourceProvider;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;

public class Activator extends AbstractConfiguredActivator implements CommandProvider {

	private static final String H2SERVERSTOP = "com/arcadsoftware/h2/server/stop"; //$NON-NLS-1$
	private static final String H2SERVERSTART = "com/arcadsoftware/h2/server/start"; //$NON-NLS-1$
	private static final String PROP_AUTOSTART = "autostart"; //$NON-NLS-1$
	private static final String PROP_DISTANT = "remote"; //$NON-NLS-1$
	private static final String PROP_SSL = "ssl"; //$NON-NLS-1$
	private static final String PROP_PORT = "port"; //$NON-NLS-1$
	private static final String PROP_TRACE = "trace"; //$NON-NLS-1$
	private static final String PROP_CREATE = "create"; //$NON-NLS-1$
	private static final String PROP_EVENT_URL = "url"; //$NON-NLS-1$
	private static final int DEFAULT_PORT = 9092;

	static {
		// Allow to use JDBC URL this local path, just like in H2 version 1.3
		System.setProperty("h2.implicitRelativePath", "true");
		// See http://www.h2database.com/javadoc/org/h2/api/ErrorCode.html#c90011
		//xxx ajouter alias !!!
		
	}
	
	private Server webServer = null;
	private Server tcpServer = null;
	@SuppressWarnings("rawtypes")
	private ServiceRegistration tcpServerReg;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		// Console command provider...
		registerService(CommandProvider.class, this);
		registerService(IDataSourceProvider.class, new DataSourceProvider(this));
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		if (webServer != null) {
			webServer.stop();
			info(Messages.Activator_WebServerStop);
			webServer = null;
		}
		if (tcpServer != null) {
			unregister(tcpServerReg);
			tcpServer.stop();
			info(Messages.Activator_DBStop);
			fireEvent(H2SERVERSTOP, null);
			tcpServer = null;
		}
		super.stop(context);
	}

	@Override
	protected String getConfigurationID() {
		return "com.arcadsoftware.h2"; //$NON-NLS-1$
	}

	private boolean isTrue(Object o) {
		return (o != null) &&
			(((o instanceof Boolean) && ((Boolean)o).booleanValue()) ||
			 ((o instanceof String) && "true".equalsIgnoreCase((String)o)) || //$NON-NLS-1$
			 ((o instanceof Integer) && (((Integer)o).intValue() == 1)));
	}
	
	@Override
	public void updatedConfiguration(@SuppressWarnings("rawtypes") Dictionary properties) {
		//Eventually stop any started server.
		if (tcpServer != null) {
			unregister(tcpServerReg);
			tcpServer.stop();
			fireEvent(H2SERVERSTOP, null);
			tcpServer = null;
			info(Messages.Activator_DBStop);
		}
		if (properties == null) {
			return;
		}
		if (isTrue(properties.get(PROP_AUTOSTART))) {
			Properties props = new Properties();
			// Start the database server...
			ArrayList<String> params = new ArrayList<String>();
			if (isTrue(properties.get(PROP_DISTANT))) {
				params.add("-tcpAllowOthers"); //$NON-NLS-1$
				//params.add("true"); //$NON-NLS-1$
				props.put(PROP_DISTANT, true);
			} else {
				props.put(PROP_DISTANT, false);
			}
			if (isTrue(properties.get(PROP_SSL))) {
				params.add("-tcpSSL"); //$NON-NLS-1$
				//params.add("true"); //$NON-NLS-1$
				props.put(PROP_SSL, true);
			} else {
				props.put(PROP_SSL, true);
			}
			if (isTrue(properties.get(PROP_TRACE))) {
				params.add("-trace"); //$NON-NLS-1$
				//params.add("true"); //$NON-NLS-1$
				props.put(PROP_TRACE, true);
			}
			if (!isTrue(properties.get(PROP_CREATE))) {
				params.add("-ifExists"); //$NON-NLS-1$
				//params.add("true"); //$NON-NLS-1$
				props.put(PROP_CREATE, false);
			} else {
				props.put(PROP_CREATE, true);
			}
			int port = DEFAULT_PORT;
			if (properties.get(PROP_PORT) != null) {
				if (properties.get(PROP_PORT) instanceof Integer) {
					port = (Integer)properties.get(PROP_PORT);
				} else if (properties.get(PROP_PORT) instanceof String) {
					try {
						port = Integer.parseInt((String) properties.get(PROP_PORT));
					} catch (NumberFormatException e) {
						port = DEFAULT_PORT;
					}
				}
				if (port < 256) {
					port = DEFAULT_PORT;
				}
			}
			params.add("-tcpPort"); //$NON-NLS-1$
			params.add(String.valueOf(port));
			props.put(PROP_PORT, port);
			props.put(PROP_EVENT_URL, "jdbc:h2:tcp://localhost:"+port+'/'); //$NON-NLS-1$
			synchronized (this) {
				try {
					tcpServer = Server.createTcpServer(params.toArray(new String[params.size()]));
					tcpServer.start();
					tcpServerReg = registerService(Server.class.getName(), tcpServer);
					info(Messages.Activator_DBStart + port);
					fireEvent(H2SERVERSTART, props);
				} catch (SQLException e) {
					tcpServer = null;
					error(Messages.Activator_DBServerError, e);
				}
			}
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void fireEvent(String topic, Properties props) {
		ServiceReference sr = getContext().getServiceReference(EventAdmin.class.getName());
		if (sr == null) {
			info(Messages.Activator_NoEvent + topic);
			return;
		}
		Object ea = getContext().getService(sr);
		if (ea instanceof EventAdmin) {
			((EventAdmin) ea).postEvent(new Event(topic, (Dictionary) props));
		} else {
			info(Messages.Activator_NoEvent + topic);
		}
	}
	
	public String getHelp() {
		StringBuilder sb = new StringBuilder();
		sb.append(Messages.Activator_HelpH2);
		sb.append(Messages.Activator_HelpH2Server);
		sb.append(Messages.Activator_HelpH2Manual);
		return sb.toString();
	}
	
	/**
	 * OSGi Console command.
	 * @param ci
	 */
	public void _h2Server(CommandInterpreter ci) {
		Dictionary<String, Object> properties = new Hashtable<>();
		properties.put(PROP_AUTOSTART,true);
		String param = ci.nextArgument();
		while (param != null) {
			if (param.equalsIgnoreCase("distant") || //$NON-NLS-1$
					param.equalsIgnoreCase("d")) { //$NON-NLS-1$
				properties.put(PROP_DISTANT,true);
			} else if (param.equalsIgnoreCase("ssl") || //$NON-NLS-1$
					param.equalsIgnoreCase("s")) { //$NON-NLS-1$
				properties.put(PROP_SSL,true);
			} else if (param.equalsIgnoreCase("trace") || //$NON-NLS-1$
					param.equalsIgnoreCase("t")) { //$NON-NLS-1$
				properties.put(PROP_TRACE,true);
			} else if (param.equalsIgnoreCase("create") || //$NON-NLS-1$
					param.equalsIgnoreCase("c")) { //$NON-NLS-1$
				properties.put(PROP_CREATE,true);
			} else {
				try {
					if (Integer.parseInt(param) > 0) {
						properties.put(PROP_PORT,Integer.decode(param));
					}
				} catch(NumberFormatException e) {}
			}
			param = ci.nextArgument();
		}
		updateConfiguration(properties);
	}
	
	public void _h2Manual(CommandInterpreter ci) {
		try {
			tcpServer = Server.createTcpServer(new String[]{});
			tcpServer.start();
			info(Messages.Activator_DBStart + DEFAULT_PORT);
		} catch (SQLException e) {
			tcpServer = null;
			error(Messages.Activator_DBServerError, e);
		}
	}
}
