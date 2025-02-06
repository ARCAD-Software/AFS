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
package com.arcadsoftware.cli;

import java.io.File;

import com.arcadsoftware.cli.core.ExecutionResult;
import com.arcadsoftware.cli.core.services.AbstractService;
import com.arcadsoftware.cli.core.services.ServiceFactory;
import com.arcadsoftware.cli.logger.IMessageLogger;
import com.arcadsoftware.cli.logger.ISimpleLogger;
import com.arcadsoftware.cli.model.LoggedObject;

public class CLI extends LoggedObject {

	public static void main(String[] args) {
		final CLI m = new CLI();
		if (args.length == 0) {
			displayUsage();
			return;
		}
		m.initProperties();
		final ExecutionResult result = m.execute(args);
		System.exit(result.getExitCode());
	}

	private void initProperties() {
		initDefaultTrustoreForTcpipSSl();
	}

	private void initDefaultTrustoreForTcpipSSl() {
		if (System.getProperty("trustStore") == null) {
			System.setProperty("trustStore", "certificate.jks");
		}
		if (System.getProperty("trustStorePassword") == null) {
			System.setProperty("trustStorePassword", "anonymizer");
		}
	}

	public static void displayUsage() {
		System.out.println("Usage: <service.identifier> [-<flag>=<value> | ...]");

	}

	private void listService() {
		System.out.println("Service List:");
		System.out.println(ServiceFactory.getInstance().getServiceList());
	}

	private ExecutionResult executeService(String[] args, AbstractService service) {
		if ((args.length >= 2) && (args[1].equalsIgnoreCase("help"))) {
			System.out.println(service.getHelp());
			return new ExecutionResult(true, AbstractService.EXITCODE_SUCCEED);
		} else {
			return service.execute();
		}
	}

	private boolean loadFromFolder() throws Throwable {

		final File serviceFolder = new File("services");
		log("Search Service File Name in " + serviceFolder.getAbsolutePath(), IMessageLogger.LOGLVL_INFO);
		if (serviceFolder.exists()) {
			final File[] files = serviceFolder.listFiles(pathname -> pathname.getName().toLowerCase().endsWith("xml"));
			if (files.length > 0) {
				for (final File f : files) {
					log("-> Scanning services in " + f.getAbsolutePath(), IMessageLogger.LOGLVL_INFO);
					ServiceFactory.getInstance().initialize(f.getAbsolutePath());
				}
				return true;
			} else {
				log("No service File found in " + serviceFolder.getAbsolutePath() + " directory",
						IMessageLogger.LOGLVL_FATAL);
				return false;
			}
		} else {
			log("File Service Directory " + serviceFolder.getAbsolutePath() + " not found",
					IMessageLogger.LOGLVL_FATAL);
		}
		return false;
	}

	public ExecutionResult execute(String[] args) {
		final ISimpleLogger logger = createLogger();
		setLogger(logger);
		ServiceFactory.getInstance().setLogger(logger);

		AbstractService service;
		try {
			final File f = new File(getServiceFilename());
			if (!f.exists()) {
				log("Service File Name: " + getServiceFilename() + " not found", IMessageLogger.LOGLVL_WARNING);
				final boolean searchInFolder = loadFromFolder();
				if (!searchInFolder) {
					return new ExecutionResult(false, AbstractService.EXITCODE_SERVICEFILE_NOTFOUND);
				}
			} else {
				log("Service File Name: " + getServiceFilename(), IMessageLogger.LOGLVL_INFO);
				ServiceFactory.getInstance().initialize(f.getAbsolutePath());
			}
			// Service List Request
			if (args[0].equalsIgnoreCase("list")) {
				listService();
				return new ExecutionResult(true, AbstractService.EXITCODE_SUCCEED);
			}
			// Service List Request
			service = ServiceFactory.getInstance().createService(args);
			if (service != null) {
				log("Service: " + args[0] + " (" + service.getClass().getName() + ")", IMessageLogger.LOGLVL_INFO);
				// service.setLogger(logger);
				final ExecutionResult result = executeService(args, service);
				log("Status    : " + (result.isSucceed() ? "Succeed" : "Failed"), IMessageLogger.LOGLVL_INFO);
				log("Exit Code : " + (result.getExitCode()), IMessageLogger.LOGLVL_INFO);
				return result;
			} else {
				log("Service: " + args[0] + " not found", IMessageLogger.LOGLVL_FATAL);
				return new ExecutionResult(false, AbstractService.EXITCODE_SERVICENOTFOUND);
			}
		} catch (final Throwable e) {
			log(e, IMessageLogger.LOGLVL_FATAL);
			return new ExecutionResult(false, AbstractService.EXITCODE_SERVICECREATIONERROR);
		}

	}

	public String getServiceFilename() {
		return "services.xml";
	}

	public ISimpleLogger createLogger() {
		return new ISimpleLogger() {

			@Override
			public void logError(String arg0) {
				System.out.println(arg0);

			}

			@Override
			public void logInfo(String arg0) {
				System.out.println(arg0);

			}

			@Override
			public void logVerbose(String arg0) {
				System.out.println(arg0);

			}

			@Override
			public void logWarning(String arg0) {
				System.out.println(arg0);

			}

		};
	}

}
