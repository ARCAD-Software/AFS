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
package cli;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import com.arcadsoftware.cm.simple.Configuration;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.RandomGenerator;
import com.arcadsoftware.tool.cli.Command;

public class EncryptPassword extends Command {

	private static final HashMap<String, String[]> PASSWORDSMAP = new HashMap<String, String[]>();
	static {
		PASSWORDSMAP.put("", new String[] {});
		PASSWORDSMAP.put("", new String[] {});
		//PASSWORDSMAP.put("", new String[] {});
	}
	
	public static void main(String[] args) {
		new EncryptPassword(args).exec();
	}

	public EncryptPassword() {
		super();
	}

	public EncryptPassword(String[] args) {
		super(args);
	}
	
	// Required to be able to generate de masterkey before the Crypt class is loaded.
	private String fogFork(String string) {
		if (string == null) {
			return null;
		}
		string = RandomGenerator.randomString(32) + string;
		return new StringBuilder(byteArrayToHexString(string.getBytes())).reverse().toString();
	}
	
	private String byteArrayToHexString(byte[] b) {
		StringBuilder sb = new StringBuilder(b.length * 2);
		for (int i = 0; i < b.length; i++) {
			int v = b[i] & 0xff;
			if (v < 16) {
				sb.append('0');
			}
			sb.append(Integer.toHexString(v));
		}
		return sb.toString().toUpperCase();
	}

	@Override
	protected void initMasterKey() {
		if (isArgument("-genmaster", "-gmk", "-genmasterkey")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			println("Master Key generation...");
			if ((getConfigIniProperties().get("com.arcadsoftware.masterkey.fog") == null) && //$NON-NLS-1$
					(getConfigIniProperties().get("com.arcadsoftware.masterkey") == null) && //$NON-NLS-1$
					(getConfigIniProperties().get("com.arcadsoftware.masterkey.path") == null)) { //$NON-NLS-1$
				// We can not use Crypto here !!!
				String fkm = fogFork(RandomGenerator.randomStringSecure(256));
				getConfigIniProperties().put("com.arcadsoftware.masterkey.fog", fkm); //$NON-NLS-²$
				System.setProperty("com.arcadsoftware.masterkey.fog", fkm); //$NON-NLS-1$
				saveConfigIni();
				println("A new random Master key has been generated.");
				printWarn("Note that all password previously encrypted in the application server configuration must be re-encrypted.");
			} else {
				println("The master key was all ready defined into the config.ini file.");
			}
		}
		super.initMasterKey();
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "encrypt"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to secure the application installation and/or encrypt a password into the configuration files.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-gmk,-genmaster]", "If not already generated this option will generate a random master key used to encrypt all other secured datas.");
		result.put("[-k,-key [<pid>:]<property>]", "If used this parameter must match a property name of a configuration section (pid), the one in which the password must be changed.");
		result.put("[-p,-password <new password>]", "The password to encrypt. If not defined the password will be prompted.");
		result.put("[-f|-fog]", "Use the Fog encryption algorithm instead of the stronger one. Fog is not a secure algorithm but it is faster to decrypt and portable to other locations.");
		return result;
	}

	@Override
	protected int run() {
		char[] pwd = getArgumentValue(new String[] {"-p", "-password"}, (char[]) null); //$NON-NLS-1$ //$NON-NLS-2$
		if (pwd == null) {
			pwd = readSecret("Enter the password to encrypt: ");
			if ((pwd == null) || (pwd.length == 0)) {
				printError("ERROR: no password entered !");
				return ERROR_MISSING_PARAMETER;
			}
			char[] pwd2 = readSecret("Confirm this password: ");
			if (!Arrays.equals(pwd, pwd2)) {
				printError("The given passwords do not matches."); 
				return ERROR_WRONG_PARAMETER;
			}
		}
		String npwd;
		if (isArgument("-fog", "-f")) { //$NON-NLS-1$ //$NON-NLS-2$
			npwd = Crypto.fog(pwd);
		} else {
			npwd = Crypto.encrypt(pwd);
		}
		String key = getArgumentValue(new String[] {"-k", "-key"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
		if ((key != null) && !key.isEmpty()) {
			int i = key.indexOf(':');
			String pid = null;
			if (i > 0) {
				pid = key.substring(0, i).trim();
				key = key.substring(i + 1);
			}
			if (pid != null) {
				Hashtable<String, Object> props = getOSGiConfiguration(pid);
				if (props != null) {
					props.put(key, npwd);
					saveOSGiConfiguration();
					println("Application Server configuration updated.");
				}
			} else {
				boolean changed = false;
				for(Configuration c: listOSgiConfigurations()) {
					if (c.contains(key)) {
						changed = true;
						c.put(key, npwd);
					}
				}
				if (changed) {
					saveOSGiConfiguration();
					println("Application Server configuration updated.");
				} else {
					printError("The property \"" + key + "\" is not declared in the configuration.");
				}
			}
		} else {
			println("The encrypted password is:");
			println();
			println(npwd);
			println();
		}
		return 0;
	}

}
