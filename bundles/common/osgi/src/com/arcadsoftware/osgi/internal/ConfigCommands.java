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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Properties;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;

public class ConfigCommands implements CommandProvider {

	private final BundleContext context;

	public ConfigCommands(BundleContext context) {
		super();
		this.context = context;
	}
	
	public String getHelp() {
		StringBuilder buffer = new StringBuilder();
		// Adding some help...
		buffer.append(Messages.getString("osgi.ConfigCommandsHelp")); //$NON-NLS-1$
		buffer.append(Messages.getString("osgi.ConfigHelpText")); //$NON-NLS-1$
		return buffer.toString();
	}

	/**
	 * Print or clear the Configuration Administration Properties, console command.
	 * @param ci
	 * @throws Exception
	 */
	public void _conf(CommandInterpreter ci) throws Exception {
		String action = ci.nextArgument();
		if (action == null) {
			ci.println(Messages.getString("osgi.ConfigUsage")); //$NON-NLS-1$
			ci.println();
			action = "list"; //$NON-NLS-1$
		}
		// Get the configuration administration service.
		ServiceReference<?> ref = context.getServiceReference(ConfigurationAdmin.class.getName());
		if (ref == null) {
			ci.println(Messages.getString("osgi.NoConfigAdminService")); //$NON-NLS-1$
			return;
		}
		ConfigurationAdmin configAdmin = (ConfigurationAdmin) context.getService(ref);
		if (action.equalsIgnoreCase("list") || action.equalsIgnoreCase("l")) { //$NON-NLS-1$ //$NON-NLS-2$
			listConfigs(ci, configAdmin);
			return;
		}
		if (action.equalsIgnoreCase("export") || action.equalsIgnoreCase("x")) { //$NON-NLS-1$ //$NON-NLS-2$
			exportConfig(ci, configAdmin);
			return;
		}
		String selection = ci.nextArgument();
		if (selection == null) {
			if (action.equalsIgnoreCase("realloc") || action.equalsIgnoreCase("r")) { //$NON-NLS-1$ //$NON-NLS-2$
				reallocAllConfiguration(ci, configAdmin);
			} else {
				ci.println(Messages.getString("osgi.ConfigUsage")); //$NON-NLS-1$
			}
			return;
		}
		String pid = null;
		Configuration[] configs = configAdmin.listConfigurations(null);
		for (int i = 0; i < configs.length; i++) {
			Configuration conf = configs[i];
			if (conf.getPid().equalsIgnoreCase(selection)) {
				pid = conf.getPid();
			} else if (conf.getPid().endsWith(selection) && (pid == null)) {
				pid = conf.getPid();
			}
		}
		if (pid == null) {
			ci.println(Messages.getString("osgi.noPIDfound")); //$NON-NLS-1$
			return;
		}
		ci.println(Messages.getString("osgi.PIDfound") + pid); //$NON-NLS-1$
		String key = ci.nextArgument();
		if (action.equalsIgnoreCase("show") || action.equalsIgnoreCase("s")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (key == null) {
				ci.printDictionary(getPropValues(configAdmin, pid), Messages.getString("osgi.PropertiesOf") + pid); //$NON-NLS-1$
			} else {
				ci.println(key + " = " + getPropValue(configAdmin, pid, key, "")); //$NON-NLS-1$ //$NON-NLS-2$
			}
		} else if (action.equalsIgnoreCase("realloc") || action.equalsIgnoreCase("r")) { //$NON-NLS-1$ //$NON-NLS-2$
			reallocConfiguration(ci, configAdmin, pid);
		} else if (action.equalsIgnoreCase("clear") || action.equalsIgnoreCase("c")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (key != null) {
				setPropValue(configAdmin, null, pid, key, null);
			} else if (clearPropValues(configAdmin, pid)) {
				ci.println(Messages.getString("osgi.PropertiesCleared")); //$NON-NLS-1$
			} else {
				ci.println(Messages.getString("osgi.PropertiesNotCleared")); //$NON-NLS-1$
			}
		} else if (action.equalsIgnoreCase("edit") || action.equalsIgnoreCase("e") || //$NON-NLS-1$ //$NON-NLS-2$
				action.equalsIgnoreCase("add") || action.equalsIgnoreCase("a")) { //$NON-NLS-1$ //$NON-NLS-2$
			String value = ci.nextArgument();
			if (value != null) {
				StringBuilder sv = new StringBuilder(value);
				value = ci.nextArgument();
				while (value != null) {
					sv.append(' ');
					sv.append(value);
					value = ci.nextArgument();
				}
				value = sv.toString();
			}
			// Support de chaine vide si la commande est "add" !
			if ((value == null) && (action.equalsIgnoreCase("add") || action.equalsIgnoreCase("a"))) { //$NON-NLS-1$ //$NON-NLS-2$
				value = ""; //$NON-NLS-1$
			}
			if (key == null) {
				ci.println(Messages.getString("osgi.ConfigUsage")); //$NON-NLS-1$
			} else {
				Integer i = null;
				try {
					i = Integer.parseInt(value);
					setPropValue(configAdmin, null, pid, key, i);
				} catch (NumberFormatException e) {
					setPropValue(configAdmin, null, pid, key, value);
				}
			}
		} else {
			ci.println(Messages.getString("osgi.ConfigUsage")); //$NON-NLS-1$
		}		
	}
	
	private void exportConfig(CommandInterpreter ci, ConfigurationAdmin configAdmin)
			throws IOException, InvalidSyntaxException, FileNotFoundException {
		String folderName = ci.nextArgument();
		if (folderName == null) {
			return;
		}
		File folder = new File(folderName);
		if (!folder.exists()) {
			folder.mkdirs();
		}
		Configuration[] configs = configAdmin.listConfigurations(null);
		if (configs != null) {
			for (Configuration conf: configs) {
				ci.print(Messages.getString("osgi.ExportConf")); //$NON-NLS-1$
				ci.println(conf.getPid());
				Properties p = new Properties();
				Dictionary<String, Object> c = conf.getProperties();
				Enumeration<String> ks = c.keys();
				while (ks.hasMoreElements()) {
					String k = ks.nextElement();
					p.put(k, c.get(k).toString());
				}
				FileOutputStream fos = new FileOutputStream(new File(folder, conf.getPid() + ".cfg")); //$NON-NLS-1$
				try {
					p.store(fos, "Exported from platform"); //$NON-NLS-1$
				} finally {
					fos.close();
				}
			}
		}
	}


	private void listConfigs(CommandInterpreter ci, ConfigurationAdmin configAdmin) throws IOException, InvalidSyntaxException {
		String filter = ci.nextArgument();
		Configuration[] configs = configAdmin.listConfigurations(filter);
		if (configs != null) {
			if (filter == null) {
				ArrayList<String> pids = new ArrayList<>();
				int pidMaxLength = 0;
				for (Configuration c: configs) {
					String pid = c.getPid();
					pids.add(pid);
					if (pidMaxLength < pid.length()) {
						pidMaxLength = pid.length();
					}
				}
				pidMaxLength += 20;
				for (Configuration c: configs) {
					String pid = c.getPid();
					int x = pid.length() - 1;
					ci.print(pid);
					if (x > 1) {
						String shurt = pid.substring(x);
						ArrayList<String> candidates = pids;
						while (true) {
							ArrayList<String> nc = new ArrayList<>();
							for(String s: candidates) {
								if (s.endsWith(shurt)) {
									nc.add(s);
								}
							}
							if (nc.size() > 1) {
								x--;
								if (x == 0) {
									break;
								}
								shurt = pid.substring(x);
								candidates = nc;
							} else {
								break;
							}
						}
						if (x > 5) { // we ignore shortcut that remove only 6 char !
							ci.print(" ("); //$NON-NLS-1$
							ci.print(shurt);
							ci.print(')');
							x = 4 + shurt.length();
						} else {
							x = 0;
						}
					}
					String s = Long.toString(c.getChangeCount());
					ci.print(" {"); //$NON-NLS-1$
					ci.print(s);
					ci.print('}');
					x += 3 + s.length();
					for (int i = pid.length() + x + 1; i < pidMaxLength; i++) {
						ci.print(' ');
					}
					ci.print(" at ");
					if (c.getBundleLocation() != null) {
						ci.println(c.getBundleLocation());
					} else {
						ci.println("null");
					}
				}
			} else {
				for (int i = 0; i < configs.length; i++) {
					ci.println(configs[i].getPid());
				}
			}
		}
	}

	private void reallocAllConfiguration(CommandInterpreter ci, ConfigurationAdmin configAdmin) throws IOException, InvalidSyntaxException {
		Configuration[] configs = configAdmin.listConfigurations(null);
		if (configs != null) {
			for (Configuration c: configs) {
				reallocConfiguration(ci, configAdmin, c);
			}
		}		
	}
	
	private void reallocConfiguration(final CommandInterpreter ci, final ConfigurationAdmin configAdmin, String pid) {
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(pid, null);
		} catch (IOException e) {
			ci.printStackTrace(e);
			return;
		}
		if (c != null) {
			reallocConfiguration(ci, configAdmin, c);
		} else {
			ci.println(Messages.getString("ConfigCommands.NoConfiguration")); //$NON-NLS-1$
		}
	}

	private void reallocConfiguration(CommandInterpreter ci, ConfigurationAdmin configAdmin, Configuration c) {
		final String oldLocation = c.getBundleLocation();
		ci.println(Messages.getString("ConfigCommands.oldLocation") + oldLocation); //$NON-NLS-1$
		c.setBundleLocation(null);
		ci.println(Messages.getString("ConfigCommands.locationReset")); //$NON-NLS-1$
		try {
			c.update();
			c = configAdmin.getConfiguration(c.getPid());
			if (c != null) {
				c.getProperties().put(ConfigurationAdmin.SERVICE_BUNDLELOCATION + ".old", oldLocation); //$NON-NLS-1$
				c.update();
				c = configAdmin.getConfiguration(c.getPid());
				if (c != null) {
					c.getProperties().remove(ConfigurationAdmin.SERVICE_BUNDLELOCATION + ".old"); //$NON-NLS-1$
					c.update();
				}
			}
			ci.println(Messages.getString("ConfigCommands.configurationUpdated")); //$NON-NLS-1$
		} catch (IOException e) {
			ci.printStackTrace(e);
		}
	}

	/**
	 * 
	 * @param configAdmin
	 * @param pid
	 * @param key
	 * @param defaultValue
	 * @return
	 */
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

	private Dictionary<String, Object> getPropValues(ConfigurationAdmin configAdmin, String pid) {
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(pid, null);
			if (c != null) {
				Dictionary<String, Object> props = c.getProperties();
				if (props != null) {
					return props;
				}
			}
		} catch (IOException e) {}
		return new Hashtable<>();
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

	/**
	 * Suppress any configuration properties from the stored properties.
	 * This will force the corresponding bundle or service to use default
	 * properties. 
	 * 
	 * @param configAdmin
	 * @param pid
	 * @return
	 */
	private boolean clearPropValues(ConfigurationAdmin configAdmin, String pid) {
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(pid, null);
		} catch (IOException e) {
			return false;
		}
		if (c == null) {
			return false;
		}
		Dictionary<String, String> props = new Hashtable<>();
		props.put(Constants.SERVICE_PID, pid);
		try {
			c.update(props);
		} catch (IOException e) {
			return false;
		}
		return true;
	}
}
