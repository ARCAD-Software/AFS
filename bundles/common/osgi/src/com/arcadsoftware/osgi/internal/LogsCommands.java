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
package com.arcadsoftware.osgi.internal;

import java.io.IOException;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Logger;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;

import com.arcadsoftware.osgi.AbstractActivator;

public class LogsCommands implements CommandProvider {

	private static final String PAXLOGGIN_SYMBOLICNAME = "org.ops4j.pax.logging.pax-logging-log4j2"; //$NON-NLS-1$;
	private static final String PAXLOGGIN_PID = "org.ops4j.pax.logging"; //$NON-NLS-1$;
	private static final String PAXLOGGIN_THRESHOLD = "log4j2.rootLogger.level"; //$NON-NLS-1$;
	private static final String PAXLOGGIN_DEFAULTLEVEL = "warn"; //$NON-NLS-1$;
	private static final String[] PAXLOGLEVELS = new String[]{"trace", "debug", "info", //$NON-NLS-1$; //$NON-NLS-2$; //$NON-NLS-3$;
		"warn", "error", "fatal"}; //$NON-NLS-1$; //$NON-NLS-2$; //$NON-NLS-3$;
	private static final String EQUINOXLOG_SYMBOLICNAME = "org.eclipse.equinox.log"; //$NON-NLS-1$;
	private static final String EQUINOXLOG_PID = "org.eclipse.equinox.log.Log"; //$NON-NLS-1$;
	private static final String EQUINOXLOG_THRESHOLD = "log.threshold"; //$NON-NLS-1$;
	private static final String EQUINOXLOG_DEFAULTLEVEL = "2"; //$NON-NLS-1$;

	private final Activator activator;
	private final BundleContext context;
	private ServiceReference<CommandProvider> serviceRef;

	public LogsCommands(Activator activator,BundleContext context) {
		super();
		this.activator = activator;
		this.context = context;
	}

	public void setServiceRef(ServiceReference<CommandProvider> serviceRef) {
		this.serviceRef = serviceRef;
	}

	@Override
	public String getHelp() {
		StringBuilder buffer = new StringBuilder();
		// Adding some help...
		buffer.append(Messages.getString("osgi.LogCommandsHelp")); //$NON-NLS-1$
		buffer.append(Messages.getString("osgi.LogInfoHelpText")); //$NON-NLS-1$
		buffer.append(Messages.getString("osgi.LogLevelHelpText")); //$NON-NLS-1$
		buffer.append(Messages.getString("osgi.LogConfigHelp")); //$NON-NLS-1$
		return buffer.toString();
	}

	/**
	 * Add a log message, console command. 
	 * 
	 * @param ci
	 * @throws Exception
	 */
	public void _logInfo(CommandInterpreter ci) throws Exception {
		StringBuilder message = new StringBuilder();
		String s = ci.nextArgument();
		boolean java = false;
		if ((s != null) && s.equalsIgnoreCase("-java")) {
			java = true;
			s = ci.nextArgument();
		}
		while (s != null) {
			message.append(' ');
			message.append(s);
			s = ci.nextArgument();
		}
		if (java) {
			try {
				Logger.getLogger("osgi").info(s);
			} catch (Exception e) {
				ci.println(e.getLocalizedMessage());
			}
		} else {
			configLog(AbstractActivator.LOG_INFO, message.toString(), null);
		}
		ci.println(Messages.getString("osgi.MessageLogged")); //$NON-NLS-1$
	}
	
	public void _logDebug(CommandInterpreter ci) throws Exception {
		StringBuilder message = new StringBuilder(ci.nextArgument());
			String s = ci.nextArgument();
			while (s != null) {
				message.append(' ');
				message.append(s);
				s = ci.nextArgument();
			}
			configLog(AbstractActivator.LOG_DEBUG, message.toString(), null);
			ci.println(Messages.getString("osgi.MessageLogged")); //$NON-NLS-1$
		
	}

	private void configLog(int level, String message, Throwable exception) {
		activator.log(serviceRef, level, message, exception);
	}

	public void _loglevel(CommandInterpreter ci) throws Exception {
		_logLevel(ci);
	}
	
	public void _logLevel(CommandInterpreter ci) throws Exception {
		if (context == null) {
			ci.println(Messages.getString("osgi.NoContextAvailable")); //$NON-NLS-1$
			return;
		}
		Bundle[] bundles = context.getBundles();
		ServiceReference<ConfigurationAdmin> ref = context.getServiceReference(ConfigurationAdmin.class);
		if (ref == null) {
			ci.println(Messages.getString("osgi.NoConfigAdminService")); //$NON-NLS-1$
			return;
		}
		ConfigurationAdmin configAdmin = context.getService(ref);
		String level = ci.nextArgument();
		if ((level == null)) {
			// Print out the current log level.
			if (findBundle(bundles,PAXLOGGIN_SYMBOLICNAME)) {
				ci.println(Messages.getString("osgi.PaxLogInstalled")); //$NON-NLS-1$
				ci.println(Messages.getString("osgi.CurrentLogLevel") + getPropValue(configAdmin, PAXLOGGIN_PID, PAXLOGGIN_THRESHOLD, PAXLOGGIN_DEFAULTLEVEL)); //$NON-NLS-1$
				ci.println(Messages.getString("osgi.PaxLogThresholds")); //$NON-NLS-1$
			} else if (findBundle(bundles,EQUINOXLOG_SYMBOLICNAME)) {
				ci.println(Messages.getString("osgi.EquinoxLogInstalled")); //$NON-NLS-1$
				ci.println(Messages.getString("osgi.CurrentLogLevel") + IntLevelToText(getPropValue(configAdmin, EQUINOXLOG_PID, EQUINOXLOG_THRESHOLD, EQUINOXLOG_DEFAULTLEVEL))); //$NON-NLS-1$
				ci.println(Messages.getString("osgi.OSGiLogLevels")); //$NON-NLS-1$
			} else {
				ci.println(Messages.getString("osgi.NoKnownLogImplementation")); //$NON-NLS-1$
			}
		} else {
			// Set the current log level.
			if (findBundle(bundles,PAXLOGGIN_SYMBOLICNAME)) {
				ci.println(Messages.getString("osgi.PaxLogInstalled")); //$NON-NLS-1$
				if (testLogLevelValue(level,PAXLOGLEVELS)) {
					String loc = getLocation(bundles,PAXLOGGIN_SYMBOLICNAME);
					Configuration c = null;
					try {
						c = configAdmin.getConfiguration(PAXLOGGIN_PID, loc);
					} catch (Exception e) {
						return;
					}
					if (c == null) {
						return;
					}
					Dictionary<String, Object> props = c.getProperties();
					if (props == null) {
						props = new Hashtable<>();
						props.put(Constants.SERVICE_PID, PAXLOGGIN_PID);
					}
					props.put(PAXLOGGIN_THRESHOLD, level);
					// By default initialize a Console logger.
					if (props.get("log4j2.appender.console.name") == null) { //$NON-NLS-1$
						props.put("log4j2.appender.console.type", "Console"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.console.name", "console"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.console.layout.type", "PatternLayout"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.console.layout.pattern", "%d %p %t %c - %m{nolookups}%n"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.rootLogger.appenderRef.console.ref", "console"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					try {
						c.update(props);
						ci.println(Messages.getString("osgi.LogThresholdUpdated")); //$NON-NLS-1$
					} catch (IOException e) {
						ci.println(Messages.getString("osgi.ErrorDuringLogThresholdUpdate")); //$NON-NLS-1$
					}
				} else {
					ci.println(Messages.getString("osgi.InvalidThreshold") + level + ")");  //$NON-NLS-1$//$NON-NLS-2$;
					ci.println(Messages.getString("osgi.PaxLogThresholds")); //$NON-NLS-1$
				}
			} else if (findBundle(bundles,EQUINOXLOG_SYMBOLICNAME)) {
				ci.println(Messages.getString("osgi.EquinoxLogInstalled")); //$NON-NLS-1$
				Integer levelValue = TextLevelToInt(level);
				if (!levelValue.equals(0)) {
					if (setPropValue(configAdmin, getLocation(bundles,EQUINOXLOG_SYMBOLICNAME), EQUINOXLOG_PID, EQUINOXLOG_THRESHOLD, levelValue)) {
						ci.println(Messages.getString("osgi.LogThresholdUpdated")); //$NON-NLS-1$
					} else {
						ci.println(Messages.getString("osgi.ErrorDuringLogThresholdUpdate")); //$NON-NLS-1$
					}
				} else {
					ci.println(Messages.getString("osgi.InvalidThreshold") + level + ")");  //$NON-NLS-1$//$NON-NLS-2$;
					ci.println(Messages.getString("osgi.OSGiLogLevels")); //$NON-NLS-1$
				}
			} else {
				ci.println(Messages.getString("osgi.NoKnownLogImplementation")); //$NON-NLS-1$
			}
		}
	}
	
	public void _logconfig(CommandInterpreter ci) throws Exception {
		_logConfig(ci);
	}
	
	public void _logConfig(CommandInterpreter ci) throws Exception {
		if (context == null) {
			ci.println(Messages.getString("osgi.NoContextAvailable")); //$NON-NLS-1$
			return;
		}
		Bundle[] bundles = context.getBundles();
		ServiceReference<ConfigurationAdmin> ref = context.getServiceReference(ConfigurationAdmin.class);
		if (ref == null) {
			ci.println(Messages.getString("osgi.NoConfigAdminService")); //$NON-NLS-1$
			return;
		}
		ConfigurationAdmin configAdmin = context.getService(ref);
		String filename = ci.nextArgument();
		if ((filename == null)) {// Print out the current log level.
			if (findBundle(bundles,PAXLOGGIN_SYMBOLICNAME)) {
				ci.println(Messages.getString("osgi.PaxLogInstalled")); //$NON-NLS-1$
			} else {
				ci.println(Messages.getString("osgi.NoKnownLogImplementation")); //$NON-NLS-1$
			}
		} else {// Set the config file location.
			if (findBundle(bundles,PAXLOGGIN_SYMBOLICNAME)) {
				ci.println(Messages.getString("osgi.PaxLogInstalled")); //$NON-NLS-1$
				String loc = getLocation(bundles, PAXLOGGIN_SYMBOLICNAME);
				Configuration c = null;
				try {
					c = configAdmin.getConfiguration(PAXLOGGIN_PID, loc);
				} catch (Exception e) {
					return;
				}
				if (c == null) {
					return;
				}
				Dictionary<String, Object> props = c.getProperties();
				if (props == null) {
					props = new Hashtable<>();
					props.put(Constants.SERVICE_PID, PAXLOGGIN_PID);
				}
				String level = (String)props.get(PAXLOGGIN_THRESHOLD);
				if ((level == null) || (level.length() == 0)) {
					level = PAXLOGGIN_DEFAULTLEVEL;
				}
				props.put(PAXLOGGIN_THRESHOLD, level);
				if ("console".equalsIgnoreCase(filename)) { //$NON-NLS-1$
					props.put("log4j2.rootLogger.appenderRef.console.ref", "console"); //$NON-NLS-1$ //$NON-NLS-2$
					props.remove("log4j2.rootLogger.appenderRef.file.ref"); //$NON-NLS-1$
					if (props.get("log4j2.appender.console.name") == null) { //$NON-NLS-1$
						props.put("log4j2.appender.console.type", "Console"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.console.name", "console"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.console.layout.type", "PatternLayout"); //$NON-NLS-1$ //$NON-NLS-2$
					}
				} else {
					props.put("log4j2.rootLogger.appenderRef.file.ref", "file"); //$NON-NLS-1$ //$NON-NLS-2$
					props.remove("log4j2.rootLogger.appenderRef.console.ref"); //$NON-NLS-1$
					if (props.get("log4j2.appender.file.name") == null) { //$NON-NLS-1$
						props.put("log4j2.appender.file.type", "RollingFile"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.file.name", "file"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.file.layout.type", "PatternLayout"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.file.policies.type", "Policies"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.file.policies.size.type", "SizeBasedTriggeringPolicy"); //$NON-NLS-1$ //$NON-NLS-2$
						props.put("log4j2.appender.file.strategy.type", "DefaultRolloverStrategy"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					props.put("log4j2.appender.file.fileName", filename); //$NON-NLS-1$
					if (filename.toLowerCase().endsWith(".log")) {
						filename = filename.substring(0, filename.length() - 4);
					}
					props.put("log4j2.appender.file.filePattern", filename + "_%d{yyyy-MM-dd}_%i.log");
					String filesize = ci.nextArgument();
					if (filesize == null) {
						filesize = "500KB"; //$NON-NLS-1$
					}
					props.put("log4j2.appender.file.policies.size.size", filesize); //$NON-NLS-1$
					String maxfiles = ci.nextArgument();
					if (maxfiles == null) {
						maxfiles = "12"; //$NON-NLS-1$
					}
					props.put("log4j2.appender.file.strategy.max", maxfiles); //$NON-NLS-1$
				}
				StringBuilder pattern = new StringBuilder();
				String s = ci.nextArgument();
				while (s != null) {
					pattern.append(' ');
					pattern.append(s);
					s = ci.nextArgument();
				}
				if (pattern.length() == 0) {
					pattern.append("%d %p %t %c - %m{nolookups}%n"); //$NON-NLS-1$
				}
				if (props.get("log4j2.appender.console.name") != null) { //$NON-NLS-1$
					props.put("log4j2.appender.console.layout.pattern", pattern.toString()); //$NON-NLS-1$
				}
				if (props.get("log4j2.appender.file.name") != null) { //$NON-NLS-1$
					props.put("log4j2.appender.file.layout.pattern", pattern.toString()); //$NON-NLS-1$
				}
				try {
					c.update(props);
				} catch (IOException e) {
					//carry on
				}
			} else {
				ci.println(Messages.getString("osgi.NoKnownLogImplementation")); //$NON-NLS-1$
			}
		}
	}

	private boolean findBundle(Bundle[] bundles, String symbolicName) {
		for (int i = 0; i < bundles.length; i++) {
			if (bundles[i].getSymbolicName().equalsIgnoreCase(symbolicName)) {
				return true;
			}
		}
		return false;
	}

	/* Get the location of the specified bundle.
	 * @return The location string
	 */
	private String getLocation(Bundle[] bundles, String symbolicName) {
		for (int i = 0; i < bundles.length; i++) {
			if (bundles[i].getSymbolicName().equalsIgnoreCase(symbolicName)) {
				return bundles[i].getLocation();
			}
		}
		return null;
	}

	private String getPropValue(ConfigurationAdmin configAdmin, String pid, String key, String defaultValue) {
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(pid, null);
		} catch (IOException e) {
			return defaultValue;
		}
		if (c != null) {
			Dictionary<String, Object> props = c.getProperties();
			if (props != null) {
				Object value = props.get(key);
				if (value != null) {
					return value.toString();
				}
			}
		}
		return defaultValue;
	}

	/* Convert internal threshold representation into text.
	 * (Work only for OSGi logging service.)
	 */
	private String IntLevelToText(String logLevel) {
		if (logLevel.equals("4")) { //$NON-NLS-1$;
			return "DEBUG"; //$NON-NLS-1$;
		} else if (logLevel.equals("3")) { //$NON-NLS-1$;
			return "INFO"; //$NON-NLS-1$;
		} else if (logLevel.equals("2")) { //$NON-NLS-1$;
			return "WARNING"; //$NON-NLS-1$;
		} else if (logLevel.equals("1")) { //$NON-NLS-1$;
			return "ERROR"; //$NON-NLS-1$;
		}
		return "UNKNOWN"; //$NON-NLS-1$;
	}

	/* Convert textual threshold representation into Integer.
	 * (Work only for OSGi logging service.)
	 */
	private Integer TextLevelToInt(String logLevel) {
		if (logLevel.equalsIgnoreCase("DEBUG")) { //$NON-NLS-1$;
			return 4;
		} else if (logLevel.equalsIgnoreCase("INFO")) { //$NON-NLS-1$;
			return 3;
		} else if (logLevel.equalsIgnoreCase("WARNING")) { //$NON-NLS-1$;
			return 2;
		} else if (logLevel.equalsIgnoreCase("ERROR")) { //$NON-NLS-1$;
			return 1;
		}
		return 0;
	}

	private boolean testLogLevelValue(String level, String[] logLevels) {
		for (int i = 0; i < logLevels.length; i++) {
			if (logLevels[i].equalsIgnoreCase(level)) {
				return true;
			}
		}
		return false;
	}

	private boolean setPropValue(ConfigurationAdmin configAdmin, String location, String pid, String key, Object value) {
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(pid, location);
		} catch (Exception e) {
			return false;
		}
		if (c == null) {
			return false;
		}
		Dictionary<String, Object> props = c.getProperties();
		if (props == null) {
			props = new Hashtable<>();
			props.put(Constants.SERVICE_PID, pid);
		}
		if (value == null) {
			props.remove(key);
		} else {
			props.put(key, value);
		}
		try {
			c.update(props);
		} catch (IOException e) {
			return false;
		}
		return true;
	}
}
