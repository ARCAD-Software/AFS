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
package com.arcadsoftware.restful.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.ManagedService;
import org.osgi.util.tracker.ServiceTracker;
import org.restlet.Component;
import org.restlet.Restlet;
import org.restlet.Server;
import org.restlet.data.Method;
import org.restlet.data.Parameter;
import org.restlet.data.Protocol;
import org.restlet.engine.Engine;
import org.restlet.ext.jetty.HttpServerHelper;
import org.restlet.ext.jetty.HttpsServerHelper;
import org.restlet.service.CorsService;
import org.restlet.util.Series;

import com.arcadsoftware.crypt.InstallCertificates;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.BranchTracker;
import com.arcadsoftware.rest.IBranch;
import com.arcadsoftware.rest.IRestServerMonitor;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.OSGiApplication;
import com.arcadsoftware.rest.XStreamCompact;

public class Activator extends AbstractActivator implements BundleListener, IRestServerMonitor {

	private static final String DEFAULTBRANCHSFILENAME = "/META-INF/resources.xml"; //$NON-NLS-1$
	private static final String BRANCHSHEADER = "Rest-Resources"; //$NON-NLS-1$
	private static final String EQUINOX_COMMON = "org.eclipse.equinox.common"; //$NON-NLS-1$
	
	static {
		// Register Jetty Connectors...
		// Attention un CNFE ici indique un mauvais packaging des bundles de Restlet.
        Engine.getInstance().getRegisteredServers().add(new HttpServerHelper(null));
        Engine.getInstance().getRegisteredServers().add(new HttpsServerHelper(null));
	}
	
	private final XStreamCompact xs = new XStreamCompact(Activator.class.getClassLoader());
	private final ArrayList<DeclarativeBranch> dbranches = new ArrayList<DeclarativeBranch>();
	private volatile Component component;
	private volatile BranchTracker rootTracker;
	private volatile ServiceTracker<Restlet, Restlet> restletTracker;
	private ServerConfiguration config;
	private ServiceRegistration<ManagedService> configReg;
	private ServiceRegistration<IBranch> rootReg;
	private ServiceRegistration<CommandProvider> consoleReg;
	private ServiceRegistration<IRestServerMonitor> monitorReg;
	private volatile HTTPServiceTracker httpTracker;
	private ServiceRegistration<OSGiApplication> appService;
	private MultiLanguageMessages messages;
	private volatile ServerProperties serverProps;
	private volatile Date startDate;

	public Activator() {
		super();
		xs.alias("branch", DeclarativeBranch.class); //$NON-NLS-1$
		xs.alias("resources", DeclarativeBranch.class); //$NON-NLS-1$
		xs.alias("branchs", ArrayList.class); //$NON-NLS-1$
		xs.alias("branches", ArrayList.class); //$NON-NLS-1$
		xs.alias("list", ArrayList.class); //$NON-NLS-1$
		xs.alias("resource", DeclarativeBranch.ResourceDeclaration.class); //$NON-NLS-1$
		xs.useAttributeFor(DeclarativeBranch.class, "path"); //$NON-NLS-1$
		xs.useAttributeFor(DeclarativeBranch.class, "uri"); //$NON-NLS-1$
		xs.useAttributeFor(DeclarativeBranch.class, "defaultResource"); //$NON-NLS-1$
		xs.useAttributeFor(DeclarativeBranch.class, "secured"); //$NON-NLS-1$
		xs.aliasAttribute(DeclarativeBranch.class, "defaultResource", "default"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.useAttributeFor(DeclarativeBranch.ResourceDeclaration.class, "path"); //$NON-NLS-1$
		xs.useAttributeFor(DeclarativeBranch.ResourceDeclaration.class, "className"); //$NON-NLS-1$
		xs.aliasAttribute(DeclarativeBranch.ResourceDeclaration.class, "className", "class"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.addImplicitCollection(DeclarativeBranch.class, "resources", DeclarativeBranch.ResourceDeclaration.class); //$NON-NLS-1$
		xs.addImplicitCollection(DeclarativeBranch.class, "subbranches", DeclarativeBranch.class); //$NON-NLS-1$
	}
	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		startDate = new Date();
		serverProps = new ServerProperties(context.getBundle());
		messages = new MultiLanguageMessages(Messages.BUNDLE_NAME, getClass().getClassLoader());
		// Add the specific OSGi client helper to load OSGi bundles resources. 
        OSGiClientHelper.setActivator(this);
		// Declared configuration datas...
		config = new ServerConfiguration(this);
		configReg = context.registerService(ManagedService.class, config, config.getProperties());
		consoleReg = context.registerService(CommandProvider.class, new ConsoleCommandProvider(this), new Hashtable<String, Object>());
	    // Prepare a Root branch to add the About Restlet.
		rootReg = context.registerService(IBranch.class, new RootBranch(this), RootBranch.properties(RootBranch.ROOTBRANCH));
		// 
		httpTracker = new HTTPServiceTracker(this);
		// Wait until "Equinox Common" is started.
		if (isCommonStarted()) {
			proceedDelayedBundleScan();
		}
		context.addBundleListener(this);
		// Initially start of the default HTTP server...
		final Timer timer = new Timer("Delayed REST Server Starting"); //$NON-NLS-1$
		timer.schedule(new TimerTask() {
			public void run() {
				// This is only useful if the configuration administration is not installed. 
				if (component == null) {
					startServer();
				}
				timer.cancel();
			}
		}, 5000);
		monitorReg = context.registerService(IRestServerMonitor.class, this, null);
	}

	private void proceedDelayedBundleScan() {
		for (Bundle bundle: getContext().getBundles()) {
			if (bundle.getState() == Bundle.ACTIVE) {
				addBundle(bundle);
			}
		}
	}

	private boolean isCommonStarted() {
		for (Bundle b: getContext().getBundles()) {
			if (EQUINOX_COMMON.equals(b.getSymbolicName()) && (b.getState() == Bundle.ACTIVE)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		if (monitorReg != null) {
			monitorReg.unregister();
			monitorReg = null;
		}
		if (rootReg != null) {
			rootReg.unregister();
			rootReg = null;
		}
		for (DeclarativeBranch b: dbranches) {
			if (b.getServiceRegistration() != null) {
				b.getServiceRegistration().unregister();
			}
		}
		dbranches.clear();
		if (configReg != null) {
			configReg.unregister();
			configReg = null;
		}
		if (consoleReg != null) {
			consoleReg.unregister();
			consoleReg = null;
		}
		if (component != null) {
			stopServer();
		}
		if (httpTracker != null) {
			httpTracker.removeservlet();
			httpTracker.close();
			httpTracker = null;
		}
        OSGiClientHelper.setActivator(null);
        super.stop(context);
		if (serverProps != null) {
			serverProps.close();
			serverProps = null;
		}
	}
	
	public OSGiApplication createApplication(boolean httpServer, boolean httpsServer) {
		if (appService != null) {
			appService.unregister();
			appService = null;
		}
		if (rootTracker != null) {
			rootTracker.close();
			rootTracker = null;
		}
		if (restletTracker != null) {
			restletTracker.close();
			restletTracker = null;
		}
		OSGiApplication application = new OSGiApplication(this, getContext());
		if (serverProps != null) {
			application.setAuthor(serverProps.getAuthor());
			application.setName(serverProps.getName());
			application.setVersion(serverProps.getVersion());
			application.setOwner(serverProps.getOwner());
			application.setDescription(serverProps.getDescription());
			application.setEmail(serverProps.getEmail());
			application.setTermsOfService(serverProps.getTermsOfService());
			application.setLicense(serverProps.getLicense());
			application.setLicenseURL(serverProps.getLicenseURL());
			application.setWebSite(serverProps.getWebSite());
		}
		application.setStatusService(new ServicesStatus(this));
		if (!Boolean.getBoolean("com.arcadsoftware.corsservice.disabled")) { //$NON-NLS-1$
			// Prevent CORS/preflight check errors when using AJAX requests
			CorsService cors = new CorsService(true);
			// TODO Check the default configuration of this service !
			Set<String> origins = getAllowedOrigins(httpServer, httpsServer);
			if (origins != null) {
				cors.setAllowedOrigins(origins);
			}
			// Remove PATCH form default accepted method, this method is currently not used.
			cors.getDefaultAllowedMethods().remove(Method.PATCH);
			// Adding HEAD which is used !
			cors.getDefaultAllowedMethods().add(Method.HEAD);
			cors.setAllowingAllRequestedHeaders(true);
			int i = Integer.getInteger("com.arcadsoftware.corsservice.maxage", -1); //$NON-NLS-1$
			if ((i > 0) && (i <= 86400)) {
				cors.setMaxAge(i);
			}
			// Skipping "OPTIONS" requests is not required to be able to add the CORS headers (this is how Restlet implement it !).
			// (This disable the usage of OPTIONS methods in the whole server.)
			cors.setSkippingResourceForCorsOptions(true);
			application.getServices().set(cors);
		} else {
			warn("CORS Service is disabled this may lead to a vulnerable server in production environments.");
		}
		// prepare the unique Root branches tracker.
		rootTracker = new BranchTracker(application, null);
		restletTracker = new ServiceTracker<Restlet, Restlet>(getContext(), Restlet.class, new RestletServiceTrackerCustomizer(application, this));
		// Register the Application service.
		appService = getContext().registerService(OSGiApplication.class, application, new Hashtable<String, Object>());
		return application;
	}

	private Set<String> getAllowedOrigins(boolean httpServer, boolean httpsServer) {
		if (!httpServer && !httpsServer) {
			return null;
		}
		HashSet<String> origins = new HashSet<String>();
		// Disable the "origins" list
		if (serverProps != null) {
			String[] ds = serverProps.getDomainNames();
			if ((ds != null) && (ds.length == 1) && "*".equals(ds[0])) {
				origins.add("*");
				return origins;
			}
		}
		try {
			// Define the default list. 
			InetAddress lh = InetAddress.getLocalHost();
			if (httpServer) {
				origins.add("http://localhost"); //$NON-NLS-1$
				origins.add("http://" + lh.getHostName()); //$NON-NLS-1$
				origins.add("http://" + lh.getCanonicalHostName()); //$NON-NLS-1$
			}
			if (httpsServer) {
				origins.add("https://localhost"); //$NON-NLS-1$
				origins.add("https://" + lh.getHostName()); //$NON-NLS-1$
				origins.add("https://" + lh.getCanonicalHostName()); //$NON-NLS-1$
			}
		} catch (UnknownHostException e) {}
		// Add the domain given by the configuration...
		if (serverProps != null) {
			for(String dn: serverProps.getDomainNames()) {
				if (!dn.isEmpty()) {
					if (httpServer) {
						origins.add("http://" + dn); //$NON-NLS-1$
					}
					if (httpsServer) {
						origins.add("https://" + dn); //$NON-NLS-1$
					}
				}
			}
		}
		return origins;
	}

	public synchronized void stopServer() {
		info(Messages.getString("Activator.Stoping")); //$NON-NLS-1$
		if (appService != null) {
			appService.unregister();
			appService = null;
		}
		// Inform the tracker that the application is about to be stoped.
		if (rootTracker != null) {
			rootTracker.close();
			rootTracker = null;
		}
		if (restletTracker != null) {
			restletTracker.close();
			restletTracker = null;
		}
		if (component != null) {
			try {
				component.stop();
			} catch (Throwable e) {
				error(Messages.getString("Activator.StopingError"),e); //$NON-NLS-1$
			} finally {
				component = null;
			}
		}
	}

	private synchronized void startServer() {
		if ((serverProps == null) || serverProps.isServerInactive()) {
			if (httpTracker != null) {
				httpTracker.addServlet();
			}
		} else {
			// Create a new Restlet component and add a HTTP server connector to it
			final StringBuilder httplogger = new StringBuilder();
			boolean https = false;
			try {
				if (component != null) {
					stopServer();
				}
				info(Messages.getString("Activator.Starting")); //$NON-NLS-1$
				// Create embedded Component.
				component = new Component();
				if (isRestletLogDisabled()) {
					component.getLogger().setUseParentHandlers(false);
				}
				if (useHTTPS()) {
					https = true;
					Server server = new Server(component.getServers().getContext().createChildContext(), //
							Arrays.asList(Protocol.HTTPS), null, serverProps.getPortssl(), component.getServers().getNext(), HttpsServerHelper.class.getName()); 
					if (!server.isAvailable()) {
						throw new ServerConfigurationException("The HTTPS Server connector is not available. Please check the server configuration, some bundles may be absent or not started, correct the error and refresh this bundle.");
					}
					component.getServers().add(server);
					Series<Parameter> parameters = server.getContext().getParameters();
					parameters.add("sslContextFactory", "org.restlet.engine.ssl.DefaultSslContextFactory"); //$NON-NLS-1$ //$NON-NLS-2$
					parameters.add("keyStorePath", serverProps.getkStore()); //$NON-NLS-1$
					parameters.add("keyStorePassword", new String(serverProps.getkSPwd())); //$NON-NLS-1$
					parameters.add("keyPassword", new String(serverProps.getKeyPwd())); //$NON-NLS-1$
					if ((serverProps.getkSType() != null) && !serverProps.getkSType().isEmpty()) {
						parameters.add("keyStoreType", serverProps.getkSType()); //$NON-NLS-1$
					}
					if ((serverProps.getDisabledCipherSuites() != null) && !serverProps.getDisabledCipherSuites().isEmpty()) {
						parameters.add("disabledCipherSuites", serverProps.getDisabledCipherSuites()); //$NON-NLS-1$
					}
					if ((serverProps.getDisabledProtocols() != null) && serverProps.getDisabledProtocols().isEmpty()) {
						parameters.add("disabledProtocols", serverProps.getDisabledProtocols()); //$NON-NLS-1$
					}
					if ((serverProps.getEnabledCipherSuites() != null) && serverProps.getEnabledCipherSuites().isEmpty()) {
						parameters.add("enabledCipherSuites", serverProps.getEnabledCipherSuites()); //$NON-NLS-1$
					}
					if ((serverProps.getEnabledProtocols() != null) && !serverProps.getEnabledProtocols().isEmpty()) {
						parameters.add("enabledProtocols", serverProps.getEnabledProtocols()); //$NON-NLS-1$
					}
					if ((serverProps.getKeyManagerAlgorithm() != null) && !serverProps.getKeyManagerAlgorithm().isEmpty()) {
						parameters.add("keyManagerAlgorithm", serverProps.getKeyManagerAlgorithm()); //$NON-NLS-1$
					}
					if ((serverProps.getProtocol() != null) && !serverProps.getProtocol().isEmpty()) {
						parameters.add("protocol", serverProps.getProtocol()); //$NON-NLS-1$
					}
					if ((serverProps.getSecureRandomAlgorithm() != null) && serverProps.getSecureRandomAlgorithm().isEmpty()) {
						parameters.add("secureRandomAlgorithm", serverProps.getSecureRandomAlgorithm()); //$NON-NLS-1$
					}
					if (serverProps.isKeyClient() && (serverProps.gettStore() != null) && !serverProps.gettStore().isEmpty()) {
						parameters.add("trustStorePath", serverProps.gettStore()); //$NON-NLS-1$
						if (serverProps.gettSPwd() != null) {
							parameters.add("trustStorePassword", new String(serverProps.gettSPwd())); //$NON-NLS-1$
						}
						if (serverProps.gettSType() != null) {
							parameters.add("trustStoreType", serverProps.gettSType()); //$NON-NLS-1$
						}
						if (serverProps.getTrustManagerAlgorithm() != null) {
							parameters.add("trustManagerAlgorithm", serverProps.getTrustManagerAlgorithm()); //$NON-NLS-1$
						}
						parameters.add("needClientAuthentication", "true"); //$NON-NLS-1$ //$NON-NLS-2$
						//parameters.add("wantClientAuthentication", "true"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					// Add other parameters specification
					parameters.add("http.requestHeaderSize", Integer.getInteger("com.arcadsoftware.httprequestheaderlimit", 16384).toString()); //$NON-NLS-1$ //$NON-NLS-2$
					parameters.add("http.responseHeaderSize", Integer.getInteger("com.arcadsoftware.httpresponseheaderlimit", 16384).toString()); //$NON-NLS-1$ //$NON-NLS-2$
				} else if (serverProps.getPortssl() > 0) {
					error("The HTTPS Server is not correctly configured and can not be started.");
				}
				if (serverProps.getPort() > 0) {
					Server server = new Server(component.getServers().getContext().createChildContext(), //
							Arrays.asList(Protocol.HTTP), null, serverProps.getPort(), component.getServers().getNext(), HttpServerHelper.class.getName());
					if (!server.isAvailable()) {
						throw new ServerConfigurationException("The HTTP Server connector is not available. Please check the server configuration, some bundles may be absent or not started, correct the error and refresh this bundle.");
					}
					component.getServers().add(server);
					// Add other parameters specification
					Series<Parameter> parameters = server.getContext().getParameters();
					parameters.add("http.requestHeaderSize", Integer.getInteger("com.arcadsoftware.httprequestheaderlimit", 16384).toString()); //$NON-NLS-1$ //$NON-NLS-2$
					parameters.add("http.responseHeaderSize", Integer.getInteger("com.arcadsoftware.httpresponseheaderlimit", 16384).toString()); //$NON-NLS-1$ //$NON-NLS-2$
					warn("Using an HTTP Server on production environment is a security breach. Change the Server configuration to use only an HTTPS server.");
				}
				component.getClients().add(Protocol.HTTP);
				component.getClients().add(Protocol.CLAP);
				component.getClients().add(Protocol.FILE);
				component.setAuthor(serverProps.getAuthor());
				component.setName(serverProps.getName());
				component.setOwner(serverProps.getOwner());
				component.setDescription(serverProps.getDescription());
				// Attach the application.
				OSGiApplication application = createApplication(serverProps.getPort() > 0, https);
				// Activate the HTTP Strict-Transport-Security header.
				application.setHttps(https);
		        component.getDefaultHost().attach(application);
				// Now, let's start the component!
		        // Note that the HTTP server connector is also automatically started.
				component.start();
				// TODO Test if the servers are available...
				
				// Start the root branches tracker.
				if (rootTracker != null) {
					rootTracker.open();
				}
				if (restletTracker != null) {
					restletTracker.open();
				}				
				if (serverProps.getPort() > 0) {
					final String portInfo = Messages.getString("Activator.Started") + serverProps.getPort(); //$NON-NLS-1$
					info(portInfo);
					httplogger.append(portInfo);
					httplogger.append("\n## WARNING: Running an HTTP Server on production is a security breach.\n\n");
				}
				if (https) {
					final String sslPortInfo = Messages.getString("Activator.Started") + serverProps.getPortssl() + " [HTTPS]"; //$NON-NLS-1$ //$NON-NLS-2$
					info(sslPortInfo);
					httplogger.append(sslPortInfo);
					httplogger.append('\n');
				}
			} catch (Exception e) {
				StringBuilder text = new StringBuilder(Messages.getString("Activator.StartingError")); //$NON-NLS-1$
				if (serverProps.getPort() > 0) {
					text.append(serverProps.getPort());
					if (https) {
						text.append(" and "); //$NON-NLS-1$
					}
				}
				if (https) {
					text.append(serverProps.getPortssl());
					text.append(" [HTTPS]"); //$NON-NLS-1$
				}
				text.append(Messages.getString("Activator.StartingErrorEnd")); //$NON-NLS-1$
				error(text.toString(), e);
				httplogger.append(text.toString());
				if ((component != null) && component.isStarted()) {
					// Blindage
					try {
						component.stop();
					} catch (Exception es) {
						debug(es);
					}
				}
				component = null;
				return;
			} finally {
				if (httplogger.length() > 0) {
					String fn = getContext().getProperty("restlog.file"); //$NON-NLS-1$
					if (fn == null) {
						fn = "./restfulports.log"; //$NON-NLS-1$
					}
					if (!fn.trim().isEmpty()) {
						final File portsLogFile = new File(fn);
						if (portsLogFile.exists()) {
							if (!portsLogFile.delete()) {
								debug("Unable to delete " + portsLogFile.getAbsolutePath()); //$NON-NLS-1$
							}
						}
						try {
							if (!portsLogFile.createNewFile()) {
								debug("Unable to create " + portsLogFile.getAbsolutePath()); //$NON-NLS-1$
							}
							try (FileWriter output = new FileWriter(portsLogFile, false)) {
								output.write(httplogger.toString());
							}
						} catch (IOException e) {
							error(e);
						}
					}
				}
			}
		}
	}

	protected boolean isRestletLogDisabled() {
		// TODO Auto-generated method stub
		return (serverProps != null) && (serverProps.isServerInactive() || serverProps.isRestletLogDisabled());
	}

	private boolean useHTTPS() {
		if ((serverProps != null) && serverProps.useHTTPS()) {
			// Check that the SSL configuration is correct before to try to start it.
			if (serverProps.getkStore() != null) {
				File ks = new File(serverProps.getkStore());
				if (ks.isFile()) {
					String result = new InstallCertificates(serverProps.getkStore(), //
							serverProps.getkSPwd(), //
							serverProps.getkSType()).testKeyStore(serverProps.getKeyAlias(), serverProps.getKeyPwd());
					if (result != null) {
						error("Invalid KeyStore: " + result, null);
						return false;
					}
				} else {
					error("Key Store file does not exists: " + ks.getAbsolutePath());
					return false;
				}
			}
			if (serverProps.isKeyClient() && (serverProps.gettStore() != null)) {
				File ts = new File(serverProps.gettStore());
				if (ts.isFile()) {
					String result = new InstallCertificates(serverProps.gettStore(), //
							serverProps.gettSPwd(), //
							serverProps.gettSType()).testKeyStore();
					if (result != null) {
						error("Invalid TrustStore: " + result, null);
						return false;
					}
				} else {
					error("Trust Store file does not exists: " + ts.getAbsolutePath());
					return false;
				}
			}
			return true;
		}
		return false;
	}

	public void propertiesChange(ServerProperties sp) {
		if ((sp != null) && !sp.equals(serverProps)) {
			if (sp.isNoMajorModification(serverProps)) {
				// Update standard properties without restarting the server.
				if (component != null) {
					if (serverProps != null) {
						serverProps.close();
					}
					serverProps = sp;
					if (useHTTPS()) {
						Server server = component.getServers().get(0);
						Series<Parameter> parameters = server.getContext().getParameters();
						parameters.add("keystorePath", serverProps.getkStore()); //$NON-NLS-1$
						parameters.add("keystorePassword", new String(serverProps.getkSPwd())); //$NON-NLS-1$
						parameters.add("keyPassword", new String(serverProps.getKeyPwd())); //$NON-NLS-1$
						parameters.add("keystoreType", serverProps.getkSType()); //$NON-NLS-1$
						parameters.add("sslServerAlias", serverProps.getKeyAlias()); //$NON-NLS-1$
						parameters.add("disabledCipherSuites", serverProps.getDisabledCipherSuites()); //$NON-NLS-1$
						parameters.add("disabledProtocols", serverProps.getDisabledProtocols()); //$NON-NLS-1$
						parameters.add("enabledCipherSuites", serverProps.getEnabledCipherSuites()); //$NON-NLS-1$
						parameters.add("enabledProtocols", serverProps.getEnabledProtocols()); //$NON-NLS-1$
						parameters.add("keyManagerAlgorithm", serverProps.getKeyManagerAlgorithm()); //$NON-NLS-1$
						parameters.add("protocol", serverProps.getProtocol()); //$NON-NLS-1$
						parameters.add("secureRandomAlgorithm", serverProps.getSecureRandomAlgorithm()); //$NON-NLS-1$
						if (serverProps.isKeyClient()) {
							parameters.add("truststorePath", serverProps.gettStore()); //$NON-NLS-1$
							parameters.add("truststorePassword", new String(serverProps.gettSPwd())); //$NON-NLS-1$
							parameters.add("trustManagerAlgorithm", serverProps.getTrustManagerAlgorithm()); //$NON-NLS-1$
							parameters.add("truststoreType", serverProps.gettSType()); //$NON-NLS-1$
							parameters.add("needClientAuthentication", "true"); //$NON-NLS-1$ //$NON-NLS-2$
						} else {
							parameters.add("needClientAuthentication", "false"); //$NON-NLS-1$ //$NON-NLS-2$
						}
					}
					component.setAuthor(serverProps.getAuthor());
					component.setName(serverProps.getName());
					component.setOwner(serverProps.getOwner());
					component.setDescription(serverProps.getDescription());
					ServiceReference<OSGiApplication> ref = getContext().getServiceReference(OSGiApplication.class);
					if (ref != null) {
						OSGiApplication app = getContext().getService(ref);
						if (app != null) {
							app.setAuthor(serverProps.getAuthor());
							app.setName(serverProps.getName());
							app.setVersion(serverProps.getVersion());
							app.setOwner(serverProps.getOwner());
							app.setDescription(serverProps.getDescription());
						}
					}
				}
			} else {
				// Stopping old server instance.
				if (serverProps.isServerInactive()) {
					if (httpTracker != null) {
						httpTracker.removeservlet();
					}
				} else {
					stopServer();
				}
				// Update parameters
				if (serverProps != null) {
					serverProps.close();
				}
				serverProps = sp;
				// Start new instance
				startServer();
			}
		}
	}

	public MultiLanguageMessages getMessages() {
		return messages;
	}
	
	public Date getStartDate() {
		return startDate;
	}

	public Component getComponent() {
		return component;
	}

	public int getPort() {
		return serverProps.getPort();
	}

	public int getSSLPort() {
		return serverProps.getPortssl();
	}

	public String getDomainName() {
		return serverProps.getDomainname();
	}
	
	public void bundleChanged(BundleEvent event) {
		// Le traitement est retardé tant que "Equinox Common" n'est pas démarré.
		if (event.getType() == BundleEvent.STARTED) {
			if (EQUINOX_COMMON.equals(event.getBundle().getSymbolicName())) {
				proceedDelayedBundleScan();
			} else if (isCommonStarted()) {
				addBundle(event.getBundle());
			}
		} else if (event.getType() == BundleEvent.STOPPED) {
			removeBundle(event.getBundle());
		}
	}

	private void removeBundle(Bundle bundle) {
		Iterator<DeclarativeBranch> itt = dbranches.iterator();
		while (itt.hasNext()) {
			DeclarativeBranch b = itt.next();
			if (bundle.equals(b.getBundle()) && (b.getServiceRegistration() != null)) {
				b.getServiceRegistration().unregister();
				itt.remove();
			}
		}
	}

	private void addBundle(Bundle bundle) {
		if ((!"org.codehaus.groovy".equals(bundle.getSymbolicName())) && //$NON-NLS-1$
				(!"groovy".equals(bundle.getSymbolicName()))) { //$NON-NLS-1$
			File file = getBranchsFile(bundle);
			if ((file != null) && file.isFile()) {
				try (FileInputStream fis = new FileInputStream(file)) {
					Object o = xs.fromXML(fis);
					if (o instanceof DeclarativeBranch) {
						((DeclarativeBranch) o).setActivator(this);
						registerBranch(bundle, (DeclarativeBranch) o);
					} else if (o instanceof List) {
						for(Object b: (List<?>) o) {
							if (b instanceof DeclarativeBranch) {
								((DeclarativeBranch) b).setActivator(this);
								registerBranch(bundle, (DeclarativeBranch) b);
							}
						}
					}
				} catch (Exception e) {
					error("Error during declarative branches loading process", e);
				}
			}
		}
	}
	
	private void registerBranch(Bundle bundle,DeclarativeBranch branch) {
		branch.setBundle(bundle);
		branch.setServiceRegistration(getContext().registerService(DeclarativeBranch.clazz, branch, DeclarativeBranch.properties(branch.getUri())));
		dbranches.add(branch);
	}

	protected File getBranchsFile(Bundle bundle) {
		String filename = DEFAULTBRANCHSFILENAME;
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(BRANCHSHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				filename = o.toString();
			}
		}
		return toFile(bundle.getEntry(filename));
	}

	@Override
	public boolean restart() {
		if (serverProps == null) {
			return false;
		}
		if (serverProps.isServerInactive()) {
			if (httpTracker != null) {
				httpTracker.removeservlet();
			}
		} else {
			stopServer();
		}
		startServer();
		return (component != null) || (serverProps.isServerInactive() && (httpTracker != null));
	}
}
