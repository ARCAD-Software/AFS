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
package com.arcadsoftware.cli.core.services;

import java.io.File;
import java.util.ArrayList;
import java.util.Base64;

import com.arcadsoftware.ae.core.logger.IMessageLogger;
import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.cli.core.ICoreModifiers;
import com.arcadsoftware.cli.core.help.Parameter;
import com.arcadsoftware.cli.model.ServerHandle;
 
public abstract class AbstractBasicService extends AbstractService{
	
	private static int ANY_EXITCODE_CONNECTION_FAILED = EXITCODE_MAX+1;
	
	protected ServerHandle handle;
	
	
	private String createDefaultOutDirectory(){
		String path = System.getenv("ARCAD_HOME");
		if (path==null) {
			path=System.getenv("user.dir");
		}
		if (path!=null) {
			File out = new File(path,"tmp");
			if (!out.exists()) {
				out.mkdirs();
			}
			return out.getAbsolutePath();
		}
		return null;
	}
	
	@Override
	public boolean validateOptions() {
		boolean result = super.validateOptions();
		if (result) {
			if (!checkRequiredOption(ICoreModifiers.CORE_URL, "URL is required!")) {
				return false;
			} else {
				String url = getOptionValue(ICoreModifiers.CORE_URL);
				if (url.toLowerCase().startsWith("https")) {
					if (!checkRequiredOption(ICoreModifiers.CORE_JKS_PATH, "The Certificat Path is required!")) {
						return false;
					} else {
						File jskFile = new File(getOptionValue(ICoreModifiers.CORE_JKS_PATH));
						if (!jskFile.exists()){
							log("Certificat "+jskFile.getAbsolutePath()+" not found.", IMessageLogger.LOGLVL_FATAL); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						} else {
							log("Certificat "+jskFile.getAbsolutePath()+" found.", IMessageLogger.LOGLVL_INFO); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						}
					}
					if (!checkRequiredOption(ICoreModifiers.CORE_JKS_CTP, "The Certificat Password is required!")) {
						return false;
					}					
				}
			}
			if (!checkRequiredOption(ICoreModifiers.CORE_USER, "Username is required!")) {
				return false;
			}
			if (!checkRequiredOption(ICoreModifiers.CORE_PWD, "password is required!")) {
				return false;
			}
			
			if (getOptionValue(ICoreModifiers.CORE_OUTPUT_FILE)!=null) {
				if (getOptionValue(ICoreModifiers.CORE_OUTPUT_DIR)==null) {
					String defaultOutfileDirectory = createDefaultOutDirectory();
					if (defaultOutfileDirectory!=null) {
						insertOption(ICoreModifiers.CORE_OUTPUT_DIR, defaultOutfileDirectory);	
					} else {
						return checkRequiredOption(ICoreModifiers.CORE_OUTPUT_DIR, "The out file directory is required!");
					}					
				}		
			}			
		}
		return result;
	}
	
	
	private ITrustStoreProvider getTrustStroreProvider(){
		final String jksPath = getOptionValue(ICoreModifiers.CORE_JKS_PATH);
		final String jksPassword = getOptionValue(ICoreModifiers.CORE_JKS_CTP);
		if ((jksPath!=null) && (jksPath!=jksPassword)) {
			return new ITrustStoreProvider(){
				public char[] getTrustStorePassword() {					
					byte[] passwordByte = Base64.getDecoder().decode(jksPassword);
					String password = jksPassword;
					if (passwordByte!=null) {
						System.out.println("TrustStore is not null ");
						password = new String(passwordByte);
					} else {
						System.out.println("Warning : The Trust Store password has not been crypted");
					}					
					return password.toCharArray();
				}

				public String getTrustStorePath() {					
					return jksPath ;
				}

				@Override
				public char[] getKeyStorePassword() {
					return getTrustStorePassword();
				}

				@Override
				public String getKeyStorePath() {
					return null;
				}


				public void resetToDefault() {
				}

				public boolean save() {
					return false;
				}


				public void setKeyStorePassword(char[] arg0) {
				}


				public void setKeyStorePath(String arg0) {
				}


				public void setTrustStorePassword(char[] arg0) {
				}


				public void setTrustStorePath(String arg0) {
				}

				@Override
				public char[] getKeyPassword() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setKeyPassword(char[] password) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getKeyStoreType() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setKeyStoreType(String type) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getTrustStoreType() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setTrustStoreType(String type) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getKeyManagerAlgorithm() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setKeyManagerAlgorithm(String keyAlgorithm) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getTrustManagerAlgorithm() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setTrustManagerAlgorithm(String trustAlgorithm) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getDisabledCipherSuites() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setDisabledCipherSuites(String disabledCiphers) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getDisabledProtocols() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setDisabledProtocols(String disabledProtocols) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getEnabledCipherSuites() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setEnabledCipherSuites(String enabledCiphers) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getEnabledProtocols() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setEnabledProtocols(String enabledProtocols) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getProtocol() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setProtocol(String protocol) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public String getSecureRandomAlgorithm() {
					// TODO Auto-generated method stub
					return null;
				}

				@Override
				public void setSecureRandomAlgorithm(String randomAlgorithm) {
					// TODO Auto-generated method stub
					
				}
				
			};	
		}
		return null;
	}
	
	@Override
	protected boolean initialize() {
		String url = getOptionValue(ICoreModifiers.CORE_URL);
		String user = getOptionValue(ICoreModifiers.CORE_USER);
		String cryptedPassword = getOptionValue(ICoreModifiers.CORE_PWD);
		byte[] passwordByte = Base64.getDecoder().decode(cryptedPassword);
		String password = cryptedPassword;
		if (passwordByte!=null) {
			password = new String(passwordByte);
		}
		handle = new ServerHandle(url, user, password);
		handle.setTrustStoreProvider(getTrustStroreProvider());
		boolean connected = handle.connect();
		if (connected) {
			log("Connected to "+url, IMessageLogger.LOGLVL_VERBOSE);
			return doInitialize();
		} else {
			exitCode = ANY_EXITCODE_CONNECTION_FAILED;
			log("Connection failed!", IMessageLogger.LOGLVL_FATAL);
		}		
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
									 "none",
									 "http://localhost:5252"));
		parameters.add(new Parameter(ICoreModifiers.CORE_USER, 
				 "A valid login that can be used to connect the server",
				 true, 
				 "",
				 ""));	
		parameters.add(new Parameter(ICoreModifiers.CORE_PWD, 
				 "The password related to the used login",
				 true, 
				 "",
				 ""));
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_DIR, 
				 "The directotry where the output file will be stored",
				 false, 
				 "",
				 ""));
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_TYPE, 
				 "The format of the output file",
				 false, 
				 "j",
				 "j for a JSON output format or x for an xml output format"));		
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_FILE, 
				 "The name of the file that will contain the execution result",
				 false, 
				 "arcadlog.xml",
				 "result.xml"));
		parameters.add(new Parameter(ICoreModifiers.CORE_OUTPUT_DIR, 
				 "The name of the directory where the result log file will be stored.\nIf not value is passed, the service will attempt to generate the <defaultdir>/tmp directory "+
		         "where <defaultdir> will be the ARCAD_HOME value or user.dir value.\nNote that if neihter ARCAD_HOME nor user.dir is defined, an error is thrown.",
				 false, 
				 "ARCAD_HOME value or user.dir value",
				 ""));
		parameters.add(new Parameter(ICoreModifiers.CORE_JKS_PATH, 
				 "The path to a Trust Store File. This value is used if the URL starts with https.",
				 false, 
				 "None",
				 ""));			
		parameters.add(new Parameter(ICoreModifiers.CORE_JKS_CTP, 
				 "The Encrypted Trust Store Password. This value is used if the URL starts with https.",
				 false, 
				 "None",
				 ""));			
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
