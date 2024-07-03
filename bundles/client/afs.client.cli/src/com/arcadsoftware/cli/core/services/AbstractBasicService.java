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
package com.arcadsoftware.cli.core.services;

import java.io.File;
import java.util.ArrayList;
import java.util.Base64;

import com.arcadsoftware.ae.core.logger.IMessageLogger;
import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.cli.core.ICoreModifiers;
import com.arcadsoftware.cli.core.help.Parameter;
import com.arcadsoftware.cli.model.ServerHandle;

public abstract class AbstractBasicService extends AbstractService {

	private static int ANY_EXITCODE_CONNECTION_FAILED = EXITCODE_MAX + 1;

	protected ServerHandle handle;

	private String createDefaultOutDirectory() {
		String path = System.getenv("ARCAD_HOME"); //$NON-NLS-1$
		if (path == null) {
			path = System.getenv("user.dir"); //$NON-NLS-1$
		}
		if (path != null) {
			final File out = new File(path, "tmp"); //$NON-NLS-1$
			if (!out.exists()) {
				out.mkdirs();
			}
			return out.getAbsolutePath();
		}
		return null;
	}

	@Override
	public boolean validateOptions() {
		final boolean result = super.validateOptions();
		if (result) {
			if (!checkRequiredOption(ICoreModifiers.CORE_URL, "URL is required!")) {
				return false;
			} else {
				final String url = getOptionValue(ICoreModifiers.CORE_URL);
				if (url.toLowerCase().startsWith("https")) { //$NON-NLS-1$
					if (!checkRequiredOption(ICoreModifiers.CORE_JKS_PATH, "The Certificat Path is required!")) {
						return false;
					} else {
						final File jskFile = new File(getOptionValue(ICoreModifiers.CORE_JKS_PATH));
						if (!jskFile.exists()) {
							log("Certificat " + jskFile.getAbsolutePath() + " not found.", IMessageLogger.LOGLVL_FATAL); //$NON-NLS-1$ //$NON-NLS-2$
						} else {
							log("Certificat " + jskFile.getAbsolutePath() + " found.", IMessageLogger.LOGLVL_INFO); //$NON-NLS-1$ //$NON-NLS-2$
						}
					}
					if (!checkRequiredOption(ICoreModifiers.CORE_JKS_CTP, "The Certificat Password is required!")) {
						return false;
					}
				}
			}
			if (!checkRequiredOption(ICoreModifiers.CORE_USER, "Username is required!") || 
					!checkRequiredOption(ICoreModifiers.CORE_PWD, "password is required!")) {
				return false;
			}

			if (getOptionValue(ICoreModifiers.CORE_OUTPUT_FILE) != null) {
				if (getOptionValue(ICoreModifiers.CORE_OUTPUT_DIR) == null) {
					final String defaultOutfileDirectory = createDefaultOutDirectory();
					if (defaultOutfileDirectory != null) {
						insertOption(ICoreModifiers.CORE_OUTPUT_DIR, defaultOutfileDirectory);
					} else {
						return checkRequiredOption(ICoreModifiers.CORE_OUTPUT_DIR,
								"The out file directory is required!");
					}
				}
			}
		}
		return result;
	}

	private ITrustStoreProvider getTrustStroreProvider() {
		final String jksPath = getOptionValue(ICoreModifiers.CORE_JKS_PATH);
		final String jksPassword = getOptionValue(ICoreModifiers.CORE_JKS_CTP);
		if ((jksPath != null) && (jksPath != jksPassword)) {
			return new ITrustStoreProvider() {
				
				@Override
				public char[] getTrustStorePassword() {
					final byte[] passwordByte = Base64.getDecoder().decode(jksPassword);
					String password = jksPassword;
					if (passwordByte != null) {
						System.out.println("TrustStore is not null ");
						password = new String(passwordByte);
					} else {
						System.out.println("Warning : The Trust Store password has not been crypted");
					}
					return password.toCharArray();
				}

				@Override
				public String getTrustStorePath() {
					return jksPath;
				}

				@Override
				public char[] getKeyStorePassword() {
					return getTrustStorePassword();
				}

				@Override
				public String getKeyStorePath() {
					return null;
				}

				@Override
				public void resetToDefault() {}

				@Override
				public boolean save() {
					return false;
				}

				@Override
				public void setKeyStorePassword(char[] arg0) {}

				@Override
				public void setKeyStorePath(String arg0) {}

				@Override
				public void setTrustStorePassword(char[] arg0) {}

				@Override
				public void setTrustStorePath(String arg0) {}

				@Override
				public char[] getKeyPassword() {
					return null;
				}

				@Override
				public void setKeyPassword(char[] password) {}

				@Override
				public String getKeyStoreType() {
					return null;
				}

				@Override
				public void setKeyStoreType(String type) {}

				@Override
				public String getTrustStoreType() {
					return null;
				}

				@Override
				public void setTrustStoreType(String type) {}

				@Override
				public String getKeyManagerAlgorithm() {
					return null;
				}

				@Override
				public void setKeyManagerAlgorithm(String keyAlgorithm) {}

				@Override
				public String getTrustManagerAlgorithm() {
					return null;
				}

				@Override
				public void setTrustManagerAlgorithm(String trustAlgorithm) {}

				@Override
				public String getDisabledCipherSuites() {
					return null;
				}

				@Override
				public void setDisabledCipherSuites(String disabledCiphers) {}

				@Override
				public String getDisabledProtocols() {
					return null;
				}

				@Override
				public void setDisabledProtocols(String disabledProtocols) {}

				@Override
				public String getEnabledCipherSuites() {
					return null;
				}

				@Override
				public void setEnabledCipherSuites(String enabledCiphers) {}

				@Override
				public String getEnabledProtocols() {
					return null;
				}

				@Override
				public void setEnabledProtocols(String enabledProtocols) {}

				@Override
				public String getProtocol() {
					return null;
				}

				@Override
				public void setProtocol(String protocol) {}

				@Override
				public String getSecureRandomAlgorithm() {
					return null;
				}

				@Override
				public void setSecureRandomAlgorithm(String randomAlgorithm) {}

			};
		}
		return null;
	}

	@Override
	protected boolean initialize() {
		final String url = getOptionValue(ICoreModifiers.CORE_URL);
		final String user = getOptionValue(ICoreModifiers.CORE_USER);
		final String cryptedPassword = getOptionValue(ICoreModifiers.CORE_PWD);
		final byte[] passwordByte = Base64.getDecoder().decode(cryptedPassword);
		String password = cryptedPassword;
		if (passwordByte != null) {
			password = new String(passwordByte);
		}
		handle = new ServerHandle(url, user, password);
		handle.setTrustStoreProvider(getTrustStroreProvider());
		final boolean connected = handle.connect();
		if (connected) {
			log("Connected to " + url, IMessageLogger.LOGLVL_VERBOSE);
			return doInitialize();
		}
		exitCode = ANY_EXITCODE_CONNECTION_FAILED;
		log("Connection failed!", IMessageLogger.LOGLVL_FATAL);
		return false;
	}

	public boolean doInitialize() {
		return true;
	}

	@Override
	public void getParameterDescription(ArrayList<Parameter> parameters) {
		parameters.add(new Parameter(ICoreModifiers.CORE_URL,
				"The URL of the AFS Server including the Port Number",
				true,
				"none", //$NON-NLS-1$
				"http://localhost:5252")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_USER,
				"A valid login that can be used to connect the server",
				true,
				"", //$NON-NLS-1$
				"")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_PWD,
				"The password related to the used login",
				true,
				"", //$NON-NLS-1$
				"")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_DIR,
				"The directotry where the output file will be stored",
				false,
				"", //$NON-NLS-1$
				"")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_TYPE,
				"The format of the output file",
				false,
				"j", //$NON-NLS-1$
				"j for a JSON output format or x for an xml output format"));
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_FILE,
				"The name of the file that will contain the execution result",
				false,
				"arcadlog.xml", //$NON-NLS-1$
				"result.xml")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_DIR,
				"The name of the directory where the result log file will be stored.\nIf not value is passed, the service will attempt to generate the <defaultdir>/tmp directory "
						+
						"where <defaultdir> will be the ARCAD_HOME value or user.dir value.\nNote that if neihter ARCAD_HOME nor user.dir is defined, an error is thrown.",
				false,
				"ARCAD_HOME value or user.dir value",
				"")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_JKS_PATH,
				"The path to a Trust Store File. This value is used if the URL starts with https.",
				false,
				"None", //$NON-NLS-1$
				"")); //$NON-NLS-1$
		parameters.add(new Parameter(ICoreModifiers.CORE_JKS_CTP,
				"The Encrypted Trust Store Password. This value is used if the URL starts with https.",
				false,
				"None", //$NON-NLS-1$
				"")); //$NON-NLS-1$
	}

	@Override
	public String getIntializationMessage() {
		return "Service is starting...";
	}

	@Override
	public String getFinalizationMessage() {
		return "Service execution completed.";
	}

}
