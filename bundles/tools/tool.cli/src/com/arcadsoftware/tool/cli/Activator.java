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
package com.arcadsoftware.tool.cli;

import java.util.Hashtable;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.cli.ICommandLineService;
import com.arcadsoftware.osgi.AbstractActivator;

import cli.ChangeDBPassword;
import cli.ConfMigration;
import cli.ConfUpdate;
import cli.ConfigAdminUser;
import cli.DBH2Backup;
import cli.DBH2Restore;
import cli.DBMigration;
import cli.DBUpdate;
import cli.EncryptPassword;
import cli.GenMasterKey;
import cli.HTTPSSelfCerts;
import cli.TestDB;
import cli.TestHTTP;
import cli.TestLDAP;
import cli.UpdateConfigIni;

public class Activator extends AbstractActivator {

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		registerCommand(new ChangeDBPassword());
		registerCommand(new ConfigAdminUser());
		registerCommand(new ConfMigration());
		registerCommand(new ConfUpdate());
		registerCommand(new DBH2Backup());
		registerCommand(new DBH2Restore());
		registerCommand(new DBMigration());
		registerCommand(new DBUpdate());
		registerCommand(new EncryptPassword());
		registerCommand(new GenMasterKey());
		registerCommand(new HTTPSSelfCerts());
		registerCommand(new TestDB());
		registerCommand(new TestHTTP());
		registerCommand(new TestLDAP());
		registerCommand(new UpdateConfigIni());
	}

	private void registerCommand(Command command) {
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		props.put(ICommandLineService.COMMANDNAME, command.getCommandFullName());
		props.put(ICommandLineService.COMMANDSHORTHELPMESSAGE, command.getCommandDescription());
		registerService(ICommandLineService.class, new CommandLineService(command), props);
	}
	
}
