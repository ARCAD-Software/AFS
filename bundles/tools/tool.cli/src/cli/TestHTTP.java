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

import java.io.File;
import java.io.IOException;
import java.net.DatagramSocket;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.InstallCertificates;
import com.arcadsoftware.tool.cli.Command;

public class TestHTTP extends Command {

	public static void main(String[] args) {
		new TestHTTP(args).exec();
	}
	
	public TestHTTP() {
		super();
	}
	
	public TestHTTP(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.server.restful"); //$NON-NLS-1$
		if ((props == null) || props.isEmpty()) {
			println("No HTTP/HTTPS server configuration found.");
			return 0;
		}
		// Test that the Ports are not used by another Socket.
		int p = getProperty(props, "port", 0); //$NON-NLS-1$
		if (p == 0) {
			println("HTTP Server is disabled.");
		} else if ((p < 0) || (p > 65535)) {
			printError("Invalid HTTP Port number: " + p);
		} else {
			println("HTTP Server is enabled...");
			if (p < 1025) {
				if (p == 80) {
					printWarn("The HTTP server use the default port number for HTTP protocol. This may expose this application server to remote attack and be blocked by firewall rules.");
				} else if (p == 443) {
					printError("The HTTP server use the port number generally reserved to HTTPS this may lead to connection problems.");
				} else {
					printWarn("The HTTP server use a reserver port number of another protocol. This may be blocked by firewall rules.");
				}
			}
			String err = available(p);
			if (err == null) {
				println("The HTTP Server port number is available.");
				printWarn("The HTTP protocol is not a secured protocol it may expose the server to remote attacks.");
			} else {
				printError("The HTTP server port number is not available: " + err);
			}
		}
		p = getProperty(props, "portssl", 0); //$NON-NLS-1$
		if (p == 0) {
			println("HTTPS Server is disabled.");
		} else if ((p < 0) || (p > 65535)) {
			printError("Invalid HTTPS Port number: " + p);
		} else {
			println("HTTPS Server is enabled...");
			if (p < 1025) {
				if (p == 443) {
					printWarn("The HTTPS server use the default port number for HTTPS protocol. This may expose this application server to remote attack and be blocked by firewall rules.");
				} else if (p == 80) {
					printError("The HTTPS server use the port number generally reserved to HTTP this may lead to connection problems.");
				} else {
					printWarn("The HTTPS server use a reserver port number of another protocol. This may be blocked by firewall rules.");
				}
			}
			String err = available(p);
			if (err == null) {
				println("The HTTPS Server port number is available.");
			} else {
				printError("The HTTPS server port number is not available: " + err);
			}
			println("Checking TLS configuration...");
			// Test the Keystore and TrustStores.
			String kStore = getProperty(props, "keystore", ""); //$NON-NLS-1$ //$NON-NLS-2$
			char[] kStorepwd = Crypto.decrypt(getProperty(props, "keystorepwd", "")); //$NON-NLS-1$ //$NON-NLS-2$
			String keyAlias = getProperty(props, "keyalias", ""); //$NON-NLS-1$ //$NON-NLS-2$
			if (kStore.isEmpty() && keyAlias.isEmpty()) {
				printWarn("This HTTPS server does not use its own private key to activate TLS encryption. This is not conforms to the classic configuration.");
			} else if (kStore.isEmpty()) {
				printError("The HTTPS Server require a \"keystore\" path to be able to activate the HTTPS protocol. The configuraiton is invalid.");
			} else if (keyAlias.isEmpty()) {
				printError("The HTTPS Server private key \"keyalias\" must be defined. The configuraiton is invalid.");
			} else if ((kStorepwd == null) || (kStorepwd.length == 0)) {
				printError("The Key Store must be protected by a password, an empty value for \"keystorepwd\" is not acceptable. The configuraiton is invalid.");
			} else {
				File ks = new File(kStore);
				if (ks.isFile()) {
					String kSType = getProperty(props, "keytype", (String) null); //$NON-NLS-1$
					char[] keypwd = Crypto.decrypt(getProperty(props, "keypwd", (String) null)); //$NON-NLS-1$
					String result = new InstallCertificates(kStore, kStorepwd, kSType).testKeyStore(keyAlias, keypwd);
					if (result != null) {
						printError("The given Key Store is invalid: " + result);
					} else {
						println("The HTTPS Server Key configuration is correct.");
					}
				} else {
					try {
						printError("The Key Store file does not exists, or is not readable: " + ks.getCanonicalPath());
					} catch (IOException e) {
						printError("The Key Store file does not exists, or is not readable: " + ks.getAbsolutePath());
					}
				}
			}
			boolean keyClient = getProperty(props, "clientauth", false); //$NON-NLS-1$
			String tStore = getProperty(props, "truststore", ""); //$NON-NLS-1$ //$NON-NLS-2$
			if (!keyClient && tStore.isEmpty()) {
				if (kStore.isEmpty() && keyAlias.isEmpty()) {
					printError("The HTTPS Server configuration is incomplete. It must define at least a Key Store or a Trust Store to provide a secured TLS connection.");
				}
			} else if (!keyClient) {
				printWarn("Has the \"clientauth\" parameter is not set to true, the definition of a Trust Store is useless.");
			} else if (tStore.isEmpty()) {
				printWarn("A Trust Store, \"truststore\", must be specified in order to activate the client authentication with TLS certificates.");
			} else {
				// Check that the SSL configuration is correct before to try to start it.
				File ts = new File(tStore);
				if (ts.isFile()) {
					String tsType = getProperty(props, "truststoretype", (String) null); //$NON-NLS-1$
					char[] tspwd = Crypto.decrypt(getProperty(props, "truststorepwd", (String) null)); //$NON-NLS-1$
					if ((tspwd == null) || (tspwd.length == 0)) {
						printWarn("No password are specified for the Trust Store, parameter \"truststorepwd\". Using an unprotected store is not recommended.");
					}
					String result = new InstallCertificates(tStore, tspwd, tsType).testKeyStore();
					if (result != null) {
						printError("The configured Trust Store is invalid: " + result);
					} else {
						println("The client TLS authentication is correct.");
					}
				} else {
					try {
						printError("The specified Trust Store file does not exists: " + ts.getCanonicalPath());
					} catch (IOException e) {
						printError("The specified Trust Store file does not exists: " + ts.getAbsolutePath());
					}
				}
			}
		}
		return 0;
	}
	
	private String available(int port) {
		try (ServerSocket ss = new ServerSocket(port)) {
			ss.setReuseAddress(true);
			try (DatagramSocket ds = new DatagramSocket(port)) {
				ds.setReuseAddress(true);
				return null;
			} catch (IOException e) {
				return e.getLocalizedMessage();
			}
		} catch (IOException e) {
			return e.getLocalizedMessage();
		}
	}
	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "testhttp"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to test the configuration ";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		return new HashMap<String, String>();
	}

}
