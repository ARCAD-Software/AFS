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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.Dictionary;
import java.util.Properties;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.restlet.Application;
import org.restlet.Component;
import org.restlet.Restlet;
import org.restlet.Server;
import org.restlet.engine.Engine;
import org.restlet.engine.connector.ConnectorHelper;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.resource.ClientResource;
import org.restlet.resource.ResourceException;
import org.restlet.routing.Filter;
import org.restlet.routing.Route;
import org.restlet.routing.Router;
import org.restlet.routing.TemplateRoute;
import org.restlet.security.Authenticator;

import com.thoughtworks.xstream.core.JVM;

public class ConsoleCommandProvider implements CommandProvider {

	private final Activator activator;
	
	public ConsoleCommandProvider(Activator activator) {
		this.activator = activator;
	}

	public String getHelp() {
		StringBuilder buffer = new StringBuilder();
		// Adding some help...
		buffer.append(Messages.getString("ConsoleCommandProvider.HelpTitle")); //$NON-NLS-1$
		buffer.append(Messages.getString("ConsoleCommandProvider.HelpCertificate")); //$NON-NLS-1$
		buffer.append(Messages.getString("ConsoleCommandProvider.HelpServerREST")); //$NON-NLS-1$
		buffer.append(Messages.getString("ConsoleCommandProvider.HelpRest")); //$NON-NLS-1$
		buffer.append(Messages.getString("ConsoleCommandProvider.HelpBranches")); //$NON-NLS-1$
		buffer.append(Messages.getString("ConsoleCommandProvider.HelpHTTPCommand")); //$NON-NLS-1$
		buffer.append(Messages.getString("ConsoleCommandProvider.xStreamCommand")); //$NON-NLS-1$
		return buffer.toString();
	}

	public void _http(CommandInterpreter ci) throws Exception {
		String method = ci.nextArgument();
		String url = ci.nextArgument();
		String fn = ci.nextArgument();
		if (method == null) {
			ci.println(Messages.getString("ConsoleCommandProvider.HelpHTTPCommand")); //$NON-NLS-1$
			return;
		}
		if ((url != null) && (url.length() > 0) && (url.charAt(0) == '/')) {
			url = "http://localhost:5252" + url; //$NON-NLS-1$
		}
		Representation rep = null;
		try {
			if ("post".equalsIgnoreCase(method)) { //$NON-NLS-1$
				rep = new ClientResource(url).post(new StringRepresentation("")); //$NON-NLS-1$
			} else if ("put".equalsIgnoreCase(method)) { //$NON-NLS-1$
				rep = new ClientResource(url).put(new StringRepresentation("")); //$NON-NLS-1$				
			} else if ("get".equalsIgnoreCase(method)) { //$NON-NLS-1$
				rep = new ClientResource(url).get(); //$NON-NLS-1$				
			} else if ("delete".equalsIgnoreCase(method)) { //$NON-NLS-1$
				rep = new ClientResource(url).delete();				
			} else if ("option".equalsIgnoreCase(method) || "options".equalsIgnoreCase(method)) { //$NON-NLS-1$ //$NON-NLS-2$
				rep = new ClientResource(url).options();				
			} else if ("head".equalsIgnoreCase(method)) { //$NON-NLS-1$
				rep = new ClientResource(url).head();				
			} else if (method.length() > 0) {
				if (url != null) {
					fn = url;
				}
				if ((method.charAt(0) == '/')) {
					method = "http://localhost:5252" + method; //$NON-NLS-1$
				}
				rep = new ClientResource(method).get();
				url = method;
				method = "GET"; //$NON-NLS-1$
			}
		} catch (ResourceException e) {
			ci.println(String.format(Messages.getString("ConsoleCommandProvider.HTTPCommand_Error"), method, url, e.toString())); //$NON-NLS-1$
			return;
		}
		if ((rep == null) || !rep.isAvailable()) {
			ci.println(Messages.getString("ConsoleCommandProvider.HTTPCommand_SuccessNoResult")); //$NON-NLS-1$
		} else {
			if (fn != null) {
				File file = new File(fn);
				if (file.isFile()) {
					if (!file.delete()) {
						ci.println("ERROR: Unable to delete old file: " + file.getAbsolutePath());
					}
				}
				FileOutputStream fos = new FileOutputStream(file);
				try {
					rep.write(fos);
					ci.println(Messages.getString("ConsoleCommandProvider.HTTPCommand_SuccessSaved") + fn); //$NON-NLS-1$
				} finally {
					fos.close();
				}
			} else {
				ci.println(rep.getText());
			}
			rep.exhaust();
			rep.release();
		}
	}
	
	@SuppressWarnings("unchecked")
	public void _rest(CommandInterpreter ci) throws Exception {
		String s = ci.nextArgument();
		if (s != null) { // Change server port
			int port = 0;
			try {
				port = Integer.parseInt(s);
			} catch (NumberFormatException e) {
				ci.println(Messages.getString("ConsoleCommandProvider.HelpRest")); //$NON-NLS-1$
				return;
			}
			@SuppressWarnings("rawtypes")
			ServiceReference ref = activator.getContext().getServiceReference(ConfigurationAdmin.class.getName());
			if (ref != null) {
				ConfigurationAdmin ca = (ConfigurationAdmin) activator.getContext().getService(ref);
				if (ca != null) {
					Configuration c = ca.getConfiguration(activator.getContext().getBundle().getSymbolicName(), null);
					if (c != null) {
						@SuppressWarnings("rawtypes")
						Dictionary p = c.getProperties();
						if (p == null) {
							p = new Properties();
							p.put(Constants.SERVICE_PID, activator.getContext().getBundle().getSymbolicName());
						}
						p.put(ServerProperties.PROP_PORTNUMBER, port);
						c.update(p);
					}
				}
			}
		} else { // print server info status.
			Component component = activator.getComponent();
			if ((component != null) && component.isStarted()) {
				ci.println(Messages.getString("ConsoleCommandProvider.ServerActive")); //$NON-NLS-1$
			} else {
				ci.println(Messages.getString("ConsoleCommandProvider.ServerInactive")); //$NON-NLS-1$
			}
			int port = activator.getPort();
			int ports = activator.getSSLPort();
			if ((port == 0) && (ports == 0)) {
				ci.println(Messages.getString("ConsoleCommandProvider.EmbeddedServer")); //$NON-NLS-1$
			} else {
				if (port > 0) {
					ci.println(String.format(Messages.getString("ConsoleCommandProvider.StandAloneServer"),port)); //$NON-NLS-1$
				}
				if (ports > 0) {
					ci.println(String.format(Messages.getString("ConsoleCommandProvider.StandAloneServerSSL"),ports)); //$NON-NLS-1$
				}
			}
			if (component != null) {
				ci.println(Messages.getString("ConsoleCommandProvider.Other")); //$NON-NLS-1$
				ci.println(Messages.getString("ConsoleCommandProvider.Name")+component.getName()); //$NON-NLS-1$
				ci.println(Messages.getString("ConsoleCommandProvider.Description")+component.getDescription()); //$NON-NLS-1$
				ci.println(Messages.getString("ConsoleCommandProvider.Author")+component.getAuthor()); //$NON-NLS-1$
				ci.println(Messages.getString("ConsoleCommandProvider.Owner")+component.getOwner()); //$NON-NLS-1$
			}
			ci.println("Registered servers:");
			for (ConnectorHelper<Server> server: Engine.getInstance().getRegisteredServers()) {
				ci.println(" " + server.getClass().getName());
			}
			if (component != null) {
				ci.println("Active Servers:");
				for(Server server: component.getServers()) {
					if (server.getContext() != null) {
						ci.println(" " + server.getContext().getAttributes().get("org.restlet.engine.helper"));
					}
				}
			}
			ci.println();
			Date date = activator.getStartDate();
			if (date != null) {
				ci.println(Messages.getString("ConsoleCommandProvider.ServerStart") + date.toString() + Messages.getString("ConsoleCommandProvider.ServerStartEnd")); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	public void _serverREST(CommandInterpreter ci) throws Exception {
		int port = 80;
		String s = ci.nextArgument();
		if (s != null) {
			try {
				port = Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		@SuppressWarnings("rawtypes")
		ServiceReference ref = activator.getContext().getServiceReference(ConfigurationAdmin.class.getName());
		if (ref != null) {
			ConfigurationAdmin ca = (ConfigurationAdmin) activator.getContext().getService(ref);
			if (ca != null) {
				Configuration c = ca.getConfiguration(activator.getContext().getBundle().getSymbolicName(), null);
				if (c != null) {
					@SuppressWarnings("rawtypes")
					Dictionary p = c.getProperties();
					if (p == null) {
						p = new Properties();
						p.put(Constants.SERVICE_PID, activator.getContext().getBundle().getSymbolicName());
					}
					p.put(ServerProperties.PROP_PORTNUMBER, port);
					c.update(p);
				}
			}
		}
	}
	
	public void _branches(CommandInterpreter ci) throws Exception {
		Component c = activator.getComponent();
		if ((c == null) || !c.isStarted()) {
			ci.println(Messages.getString("ConsoleCommandProvider.ServerNotStarted")); //$NON-NLS-1$
		}
		for(Route route : c.getDefaultHost().getRoutes()) {
			printRoute(ci, route, 0);
		}
	}
	
	private void printRoute(CommandInterpreter ci, Route route, int tab) {
		if (route == null) {
			return;
		}
		String p = null;
		if (route instanceof TemplateRoute) {
			if (((TemplateRoute) route).getTemplate() != null) {
				p = ((TemplateRoute) route).getTemplate().getPattern();
			}
		} else {
			p = route.getName();
		}
		if ((p != null) && (p.length() > 0)) {
			for (int i = 0; i < tab; i++) {
				ci.print("\t"); //$NON-NLS-1$
			}
			ci.println(p);
			tab++;
		}
		Restlet r = route.getNext();
		if (r instanceof Application) {
			r = ((Application) r).getInboundRoot();
		}
		if (r instanceof Filter) {
			if (r instanceof Authenticator) {
				ci.println(Messages.getString("ConsoleCommandProvider.BranchesSecurized")); //$NON-NLS-1$
			}
			r = ((Filter) r).getNext();
		}
		if (r instanceof Router) {
			for(Route rt : ((Router) r).getRoutes()) {
				printRoute(ci, rt, tab);
			}
			printRoute(ci, ((Router) r).getDefaultRoute(), tab);
		}
	}

	public void _certificate(CommandInterpreter ci) throws Exception {
		String host = ci.nextArgument();
		String ksf =  ci.nextArgument();
		String pwd = ci.nextArgument();
		int port = 443;
		if (host == null) {
			ci.println(Messages.getString("ConsoleCommandProvider.AddresseNeeded")); //$NON-NLS-1$
			return;
		}
		if (host.indexOf(':') >= 0) { 
			String[] c = host.split(":"); //$NON-NLS-1$
			host = c[0];
			try {
				port = Integer.parseInt(c[1]);
			} catch (NumberFormatException e) {
				ci.println(Messages.getString("ConsoleCommandProvider.PortNumberPositive")); //$NON-NLS-1$
				return;
			}
		}
		if (ksf == null) {
			ci.println("A Valid TrustStore is required (Second command argument) !");
			return;
		}
		if (pwd == null) {
			ci.println("A Password is required (Third command argument) !");
			return;
		}
		char[] passphrase = pwd.toCharArray(); //$NON-NLS-1$
		File file = new File(ksf); //$NON-NLS-1$
		ci.println(Messages.getString("ConsoleCommandProvider.LoadingKeystore") + file.getAbsolutePath() + //$NON-NLS-1$ 
				Messages.getString("ConsoleCommandProvider.points")); //$NON-NLS-1$
		KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
		InputStream in = new FileInputStream(file);
		try {
			ks.load(in, passphrase);
		} finally {
			in.close();
		}
		// Test connection (and get certificate from server).
		SSLContext context = SSLContext.getInstance("TLS"); //$NON-NLS-1$
		TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
		tmf.init(ks);
		X509TrustManager defaultTrustManager = (X509TrustManager) tmf.getTrustManagers()[0];
		SavingTrustManager tm = new SavingTrustManager(defaultTrustManager);
		context.init(null, new TrustManager[] { tm }, null);
		SSLSocketFactory factory = context.getSocketFactory();

		ci.println(String.format(Messages.getString("ConsoleCommandProvider.OpeningOcnnection"), host, port));  //$NON-NLS-1$
		SSLSocket socket = (SSLSocket) factory.createSocket(host, port);
		socket.setSoTimeout(10000);
		try {
			ci.println(Messages.getString("ConsoleCommandProvider.handshake")); //$NON-NLS-1$
			socket.startHandshake();
			socket.close();
			ci.println();
			ci.println(Messages.getString("ConsoleCommandProvider.Terminated")); //$NON-NLS-1$
			return;
		} catch (SSLException e) {
			ci.println(e.getLocalizedMessage());
		}
		X509Certificate[] chain = tm.chain;
		if (chain == null) {
			ci.println(Messages.getString("ConsoleCommandProvider.ErrorNoCertificate")); //$NON-NLS-1$
			return;
		}
		ci.println();
		ci.println(String.format(Messages.getString("ConsoleCommandProvider.CertificatesNumber"), chain.length)); //$NON-NLS-1$
		ci.println();
		MessageDigest sha1 = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$
		MessageDigest md5 = MessageDigest.getInstance("MD5"); //$NON-NLS-1$
		for (int i = 0; i < chain.length; i++) {
			X509Certificate cert = chain[i];
			ci.println(" " + (i + 1) + Messages.getString("ConsoleCommandProvider.Subject") + cert.getSubjectDN()); //$NON-NLS-1$ //$NON-NLS-2$
			ci.println("   Issuer  " + cert.getIssuerDN()); //$NON-NLS-1$
			sha1.update(cert.getEncoded());
			ci.println("   sha1    " + toHexString(sha1.digest())); //$NON-NLS-1$
			md5.update(cert.getEncoded());
			ci.println("   md5     " + toHexString(md5.digest())); //$NON-NLS-1$
			ci.println();
		}
		int k = 1;
		for (X509Certificate cert : chain) {
			ks.setCertificateEntry(host + "-" + k++, cert); //$NON-NLS-1$
		}
		OutputStream out = new FileOutputStream(file);
		try {
			ks.store(out, "inet#*qua@12dra".toCharArray()); //$NON-NLS-1$
		} finally {
			out.close();
		}
		ci.println(Messages.getString("ConsoleCommandProvider.CertificatesAdded")+file.getAbsolutePath()); //$NON-NLS-1$
	}	

	private static final char[] HEXDIGITS = "0123456789abcdef".toCharArray(); //$NON-NLS-1$

	private static String toHexString(byte[] bytes) {
		StringBuilder sb = new StringBuilder(bytes.length * 3);
		for (int b : bytes) {
			b &= 0xff;
			sb.append(HEXDIGITS[b >> 4]);
			sb.append(HEXDIGITS[b & 15]);
			sb.append(' ');
		}
		return sb.toString();
	}

	private static class SavingTrustManager implements X509TrustManager {

		private final X509TrustManager tm;
		private X509Certificate[] chain;

		SavingTrustManager(X509TrustManager tm) {
			this.tm = tm;
		}

		public X509Certificate[] getAcceptedIssuers() {
			throw new UnsupportedOperationException();
		}

		public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
			throw new UnsupportedOperationException();
		}

		public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
			this.chain = chain;
			tm.checkServerTrusted(chain, authType);
		}
	}

	public void _XStreamDiag(CommandInterpreter ci) throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		PrintStream oldout = System.out;
		try {
			System.setOut(new PrintStream(bos));
			JVM.main(new String[0]);
		} finally {
			System.setOut(oldout);
		}
		ci.println(bos.toString());
	}
}
