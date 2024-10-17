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

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;

import com.arcadsoftware.crypt.RandomGenerator;
import com.arcadsoftware.tool.cli.Command;

public class GenMasterKey extends Command {
	
	public static void main(String[] args) {
		System.exit(new GenMasterKey(args).exec());
	}

	public GenMasterKey() {
		super();
	}

	public GenMasterKey(String[] args) {
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

	private static final String RANDOMSTRING3 = "=sµ"; //$NON-NLS-1$
	private static final String RANDOMSTRING7 = "ĂƇ©%"; //$NON-NLS-1$

	@Override
	protected void initMasterKey() {
		println("Master Key generation...");
		if ((getConfigIniProperties().get("com.arcadsoftware.masterkey.fog") == null) && //$NON-NLS-1$
				(getConfigIniProperties().get("com.arcadsoftware.masterkey") == null) && //$NON-NLS-1$
				(getConfigIniProperties().get("com.arcadsoftware.masterkey.path") == null)) { //$NON-NLS-1$
			// We can not use Crypto here !!!
			String k;
			if (isArgument("-d", "-def", "-defaut", "-default")) {
				String osn = getArgumentValue(new String[] {"-on", "-osname"}, System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				String jh = getArgumentValue(new String[] {"-jh", "-javahome"}, System.getProperty("java.home")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				String hn = getArgumentValue(new String[] {"-hn", "-hostname"}, ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				if (hn.isEmpty()) {
					hn = System.getProperty("user.name"); //$NON-NLS-1$
					try {
						String h = InetAddress.getLocalHost().getHostName();
						if (!h.equals(InetAddress.getLocalHost().getHostAddress())) {
							hn = h;
						}
					} catch (UnknownHostException e) {}
				}
				char[] dmkc = new char[osn.length() + RANDOMSTRING7.length() + hn.length() + RANDOMSTRING3.length() + jh.length()];
				System.arraycopy(osn.toCharArray(), 0, dmkc, 0, osn.length());
				System.arraycopy(RANDOMSTRING7.toCharArray(), 0, dmkc, osn.length(), RANDOMSTRING7.length());
				System.arraycopy(hn.toCharArray(), 0, dmkc, osn.length() + RANDOMSTRING7.length(), hn.length());
				System.arraycopy(RANDOMSTRING3.toCharArray(), 0, dmkc, osn.length() + RANDOMSTRING7.length() + hn.length(), RANDOMSTRING3.length());
				System.arraycopy(jh.toCharArray(), 0, dmkc, osn.length() + RANDOMSTRING7.length() + hn.length() + RANDOMSTRING3.length(), jh.length());
				k = new String(dmkc);
			} else {
				k = RandomGenerator.randomStringSecure(256);
			}
			String fkm = fogFork(k);
			getConfigIniProperties().put("com.arcadsoftware.masterkey.fog", fkm); //$NON-NLS-²$
			System.setProperty("com.arcadsoftware.masterkey.fog", fkm); //$NON-NLS-1$
			saveConfigIni();
			println("A new random Master key has been generated.");
			printWarn("Note that all password previously encrypted in the application server configuration must be re-encrypted before the next application start.");
		} else {
			println("The master key was all ready defined into the config.ini file.");
			printWarn("Remove it to regerate a new key, but note that changing the Master Key may compromise your installation.");
		}
		super.initMasterKey();
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "masterkey"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to generate a random Master Key for this application server. The master key is a security material used to encrypt and decrypt critical datas.\nChanging the master key will broke all data currently encrypted in the application configuration.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-d|-default]", "If present this command will generate a key with the application default process.");
		result.put("[-jh|-javahome <path>]", "Combined with -default this parameter will replace the current java home directory with the given one.");
		result.put("[-hn|-hostname <dns>]", "Combined with -default this parameter will replace the current host name with the given one.");
		result.put("[-on|-osname <dns>]", "Combined with -default this parameter will replace the current OS name with the given one.");
		return result;
	}

	@Override
	protected int run() {
		return 0;
	}

}
