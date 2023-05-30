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
package com.arcadsoftware.osgi;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * This class implement a simple Facade to the Java native logger interface;
 * 
 * @author ARCAD Software
 */
public class JavaLogged implements ILoggedPlugin {

	/**
	 * Initialize Java logging facilities from a ./log.properties file.
	 * @see LogManager
	 */
	public static void init() {
		init(new File("./log.properties")); //$NON-NLS-1$
	}

	/**
	 * Initialize the logging facilities from the given file.
	 * @param confFile
	 */
	public static void init(File confFile) {
		if (confFile.isFile()) {
			try {
	            FileInputStream inputStream = new FileInputStream(confFile);
	            try {
	            	LogManager.getLogManager().readConfiguration(inputStream);
	            } finally {
	            	inputStream.close();
	            }
	        } catch (IOException exception) {
	            exception.printStackTrace();
	        }
		}
	}

	/**
	 * Initialize the logging facilities from the given set of properties.
	 * 
	 * @param properties
	 */
	public static void init(Properties properties) {
		if (properties != null) {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			try {
				properties.store(bos, ""); //$NON-NLS-1$
            	LogManager.getLogManager().readConfiguration(new ByteArrayInputStream(bos.toByteArray()));
			} catch (IOException e) {
				e.printStackTrace();
			} catch (ClassCastException e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Initialize the logging facilities to a default file logging, with rolling, et the specified level. 
	 * 
	 * @param logFileName the default Log file name pattern, for instance: <code>%h/server%u.log</code>
	 * @param level The logging level.
	 * @param size Maximal size of the log files.
	 * @param count number of rolling log files
	 * @see LogManager
	 * @see Level
	 */
	public static void init(String logFileName, Level level, int size, int count) {
		Properties p = new Properties();
		p.put(".level", level.toString()); //$NON-NLS-1$
		p.put("java.util.logging.ConsoleHandler.level", level.toString()); //$NON-NLS-1$
		p.put("java.util.logging.ConsoleHandler.formatter", "java.util.logging.SimpleFormatter"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("confLogger.level", level.toString()); //$NON-NLS-1$
		p.put("java.util.logging.FileHandler.pattern", logFileName); //$NON-NLS-1$
		if (size < 10000) {
			size = 10000;
		}
		p.put("java.util.logging.FileHandler.limit", Integer.toString(size)); //$NON-NLS-1$ //$NON-NLS-2$
		if (count < 1) {
			count = 1;
		}
		p.put("java.util.logging.FileHandler.count", Integer.toString(count)); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("java.util.logging.FileHandler.formatter", "java.util.logging.SimpleFormatter"); //$NON-NLS-1$ //$NON-NLS-2$
		p.put("handlers", "java.util.logging.ConsoleHandler,java.util.logging.FileHandler"); //$NON-NLS-1$ //$NON-NLS-2$
		init(p);
	}
	
	private final Logger logger;
	
	/**
	 * Create a Logger with the given name. Using default log.properties file for configuration.
	 * 
	 * @param name
	 */
	public JavaLogged(String name) {
		super();
		logger = Logger.getLogger(name);
	}

	/**
	 * Create a Logger with the current class name.
	 */
	public JavaLogged() {
		super();
		logger = Logger.getLogger(this.getClass().getName());
	}

	/**
	 * Create a Logger with the specified bundle symbolic name.
	 * @param activator
	 */
	public JavaLogged(AbstractActivator activator) {
		super();
		if (activator != null) {
			logger = Logger.getLogger(activator.getContext().getBundle().getSymbolicName());
		} else {
			logger = Logger.getLogger(this.getClass().getPackage().getName());
		}
	}
	
	@Override
	public void log(String message) {
		logger.info(message);
	}

	@Override
	public void log(String message, Throwable e) {
		logger.log(Level.INFO, message, e);
	}

	@Override
	public void log(Throwable e) {
		logger.log(Level.INFO, e.getLocalizedMessage(), e);
	}

	@Override
	public void error(String message, Throwable e) {
		logger.log(Level.SEVERE, message, e);
	}

	@Override
	public void warn(String message) {
		logger.warning(message);
	}

	@Override
	public void warn(String message, Throwable e) {
		logger.log(Level.WARNING, message, e);
	}

	@Override
	public void debug(String message) {
		logger.fine(message);
	}

	@Override
	public void debug(String message, Throwable e) {
		logger.log(Level.FINE, message, e);
	}

	@Override
	public void debug(Throwable e) {
		logger.log(Level.FINE, e.getLocalizedMessage(), e);
	}

}
