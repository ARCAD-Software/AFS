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
package com.arcadsoftware.restful.internal;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;

import com.arcadsoftware.osgi.ISystemParameters;
import com.arcadsoftware.rest.OSGiApplication;

/**
 * Very simple restlet that generate a minimal information about the system. 
 * 
 * @author ARCAD Software
 */
public class AboutRestlet extends Restlet {

	private static final ArrayList<MediaType> SUPPORTEDMEDIATYPES = new ArrayList<MediaType>(5);
	
	static {
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_HTML); // Required to support Internet Explorer 9>
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_XML);
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_PLAIN);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_JSON);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_XML);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_XHTML);
	}

	private final Activator activator;
	private final Date webStartDate;
	
	public AboutRestlet(Context context, Activator activator) {
		super(context);
		webStartDate = new Date();
		this.activator = activator;
	}

	@Override
	public void handle(Request request, Response response) {
		response.getAllowedMethods().add(Method.GET);
		response.getAllowedMethods().add(Method.HEAD);
		if (!Method.GET.equals(request.getMethod())) {
			response.setStatus(Status.SUCCESS_NO_CONTENT);
			return;
		}
		MediaType media = request.getClientInfo().getPreferredMediaType(SUPPORTEDMEDIATYPES);
		if (media == null) {
			media = MediaType.TEXT_PLAIN;
		}
		final StringBuilder res = new StringBuilder();
		String applicationName = ""; //$NON-NLS-1$
		String applicationAuthor = ""; //$NON-NLS-1$
		String applicationOwner = ""; //$NON-NLS-1$
		String applicationVersion = ""; //$NON-NLS-1$
		String applicationEmail = ""; //$NON-NLS-1$
		String applicationlic_name = ""; //$NON-NLS-1$
		String applicationlic_URL = ""; //$NON-NLS-1$
		String applicationUsage = ""; //$NON-NLS-1$
		String applicationURL = ""; //$NON-NLS-1$
		String sysparam = getSystemParameters(); //$NON-NLS-1$
		if (getApplication() != null) {
			if (getApplication().getName() != null) {
				applicationName = getApplication().getName();
			}
			if (getApplication().getAuthor() != null) {
				applicationAuthor = getApplication().getAuthor();
			} 
			if (getApplication().getOwner() != null) {
				applicationOwner = getApplication().getOwner();
			}
			if (getApplication() instanceof OSGiApplication) {
				applicationVersion = ((OSGiApplication) getApplication()).getVersion();
				applicationEmail = ((OSGiApplication) getApplication()).getEmail();
				applicationlic_name = ((OSGiApplication) getApplication()).getLicense();
				applicationlic_URL = ((OSGiApplication) getApplication()).getLicenseURL();
				applicationUsage = ((OSGiApplication) getApplication()).getTermsOfService();
				applicationURL = ((OSGiApplication) getApplication()).getWebSite();
			}
		}
		final BundleContext context = activator.getContext();
		if (MediaType.TEXT_XML.equals(media) || MediaType.APPLICATION_XML.equals(media)) {
			res.append("<?xml version=\"1.0\" encoding=\"utf-8\" ?><about>"); //$NON-NLS-1$		
			if (getApplication() != null) {
				res.append("<application><name>"); //$NON-NLS-1$
				res.append(escapeXML(applicationName));
				res.append("</name><version>"); //$NON-NLS-1$
				res.append(escapeXML(applicationVersion));
				res.append("</version><author>"); //$NON-NLS-1$
				res.append(escapeXML(applicationAuthor));		
				res.append("</author><owner>"); //$NON-NLS-1$
				res.append(escapeXML(applicationOwner));
				res.append("</owner><contact>"); //$NON-NLS-1$
				res.append(escapeXML(applicationEmail));
				res.append("</contact><usage>"); //$NON-NLS-1$
				res.append(escapeXML(applicationUsage));
				res.append("</usage><license>"); //$NON-NLS-1$
				res.append(escapeXML(applicationlic_name));
				res.append("</license><licenseUrl>"); //$NON-NLS-1$
				res.append(escapeXML(applicationlic_URL));
				res.append("</licenseUrl><docummentation>"); //$NON-NLS-1$
				res.append(escapeXML(applicationURL));
				res.append("</docummentation><sysparam>"); //$NON-NLS-1$
				res.append(escapeXML(sysparam));
				res.append("</sysparam></application>"); //$NON-NLS-1$
			}
			res.append("<framework><vendor>"); //$NON-NLS-1$
			res.append(escapeXML(context.getProperty(Constants.FRAMEWORK_VENDOR)));
			res.append("</vendor><version>"); //$NON-NLS-1$
			res.append(escapeXML(context.getProperty(Constants.FRAMEWORK_VERSION)));
			res.append("</version>"); //$NON-NLS-1$
			if (webStartDate != null) {
				res.append("<webstart>"); //$NON-NLS-1$
				res.append(dateToISOString(webStartDate));
				res.append("</webstart>"); //$NON-NLS-1$
			}
			if (activator.getStartDate() != null) {
				res.append("<start>"); //$NON-NLS-1$
				res.append(dateToISOString(activator.getStartDate()));
				res.append("</start>"); //$NON-NLS-1$
			}
			res.append("</framework></about>"); //$NON-NLS-1$;
		} else if (MediaType.TEXT_HTML.equals(media) || MediaType.APPLICATION_XHTML.equals(media)) {
			res.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"); //$NON-NLS-1$
			// TODO Manage multi-language document...
			// TODO Use a customizable template...
			if (!applicationName.isEmpty()) {
				res.append("<title>"); //$NON-NLS-1$
				res.append(escapeXML(applicationName));
				res.append(" , About Page</title>"); //$NON-NLS-1$ 
			} else {
				res.append("<title>About Page</title>"); //$NON-NLS-1$
			}
			res.append("</head><body><ul><li><strong>Vendor</strong>: "); //$NON-NLS-1$
			res.append(escapeXML(context.getProperty(Constants.FRAMEWORK_VENDOR)));
			res.append("</li><li><strong>Version</strong>: "); //$NON-NLS-1$
			res.append(escapeXML(context.getProperty(Constants.FRAMEWORK_VERSION)));
			res.append("</li></ul>"); //$NON-NLS-1$
			if (getApplication() != null) {
				res.append("<h3>Application</h3><ul><li><strong>Name</strong>: "); //$NON-NLS-1$
				res.append(escapeXML(applicationName));
				res.append("</li><li><strong>Version</strong>: "); //$NON-NLS-1$
				res.append(escapeXML(applicationVersion));
				res.append("</li><li><strong>Author</strong>: "); //$NON-NLS-1$
				res.append(escapeXML(applicationAuthor));
				res.append("</li><li><strong>Owner</strong>: "); //$NON-NLS-1$
				res.append(escapeXML(applicationOwner));
				res.append("</li><li><strong>Contact</strong>: "); //$NON-NLS-1$
				if ((applicationEmail != null) && !applicationEmail.trim().isEmpty()) {
					res.append("<a href=\"mailto:"); //$NON-NLS-1$
					res.append(escapeXML(applicationEmail));
					res.append("\">"); //$NON-NLS-1$
					res.append(escapeXML(applicationEmail));
					res.append("</a>"); //$NON-NLS-1$
				}
				res.append("</li><li><strong>License</strong>: "); //$NON-NLS-1$
				if ((applicationlic_URL != null) && !applicationlic_URL.trim().isEmpty()) {
					res.append("<a href=\""); //$NON-NLS-1$
					res.append(escapeXML(applicationlic_URL));
					res.append("\">"); //$NON-NLS-1$
				}
				res.append(escapeXML(applicationlic_name));
				if ((applicationlic_URL != null) && !applicationlic_URL.trim().isEmpty()) {
					res.append("</a>"); //$NON-NLS-1$
				}
				res.append("</li><li><strong>Terms of Service</strong>: "); //$NON-NLS-1$
				res.append(escapeXML(applicationUsage));
				res.append("</li><li><strong>Documentation</strong>: "); //$NON-NLS-1$
				if ((applicationlic_URL != null) && !applicationlic_URL.trim().isEmpty()) {
					res.append("<a href=\""); //$NON-NLS-1$
					res.append(escapeXML(applicationlic_URL));
					res.append("\">"); //$NON-NLS-1$
					res.append(escapeXML(applicationlic_URL));
					res.append("</a>"); //$NON-NLS-1$
				}
				res.append("</li><li><strong>System Parameters</strong>: "); //$NON-NLS-1$
				res.append(escapeXML(sysparam));
				res.append("</li>"); //$NON-NLS-1$
			}
			if ((webStartDate != null) || (activator.getStartDate() != null)) {
				res.append("<h3>Dates</h3><ul>"); //$NON-NLS-1$
				if (webStartDate != null) {
					res.append("<li><strong>Web services Start date</strong>: "); //$NON-NLS-1$
					res.append(dateToISOString(webStartDate));
					res.append("</li>"); //$NON-NLS-1$;
				}
				if (activator.getStartDate() != null) {
					res.append("<li><strong>OSGi Bundle Start date</strong>: "); //$NON-NLS-1$
					res.append(dateToISOString(activator.getStartDate()));
					res.append("</li>"); //$NON-NLS-1$
				}
				res.append("</ul>"); //$NON-NLS-1$
			}
			res.append("</body></html>"); //$NON-NLS-1$
		} else if (MediaType.APPLICATION_JSON.equals(media)) {
			res.append("{\"about\":{"); //$NON-NLS-1$
			if (getApplication() != null) {
				res.append("\"application\": {"); //$NON-NLS-1$
				res.append(jsonString("name", applicationName)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("version", applicationVersion)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("author", applicationAuthor)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("owner", applicationOwner)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("contact", applicationEmail)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("license", applicationlic_name)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("licenseurl", applicationlic_URL)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("usage", applicationUsage)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("url", applicationURL)); //$NON-NLS-1$
				res.append(',');
				res.append(jsonString("sysparam", sysparam)); //$NON-NLS-1$
				res.append("},"); //$NON-NLS-1$
			}
			res.append("\"framework\": {"); //$NON-NLS-1$
			res.append(jsonString("vendor", context.getProperty(Constants.FRAMEWORK_VENDOR))); //$NON-NLS-1$
			res.append(',');
			res.append(jsonString("version", context.getProperty(Constants.FRAMEWORK_VERSION))); //$NON-NLS-1$
			if (webStartDate != null) {
				res.append(',');
				res.append(jsonString("webstart", dateToISOString(webStartDate))); //$NON-NLS-1$
			}
			if (activator.getStartDate() != null) {
				res.append(',');
				res.append(jsonString("start", dateToISOString(activator.getStartDate()))); //$NON-NLS-1$
			}
			res.append("}}}"); //$NON-NLS-1$
		} else {
			media = MediaType.TEXT_PLAIN;
			if (getApplication() != null) {
				res.append(" =================================================== \n   "); //$NON-NLS-1$
				res.append(applicationName);
				res.append(' ');
				res.append(applicationVersion);
				res.append("\n      Author: "); //$NON-NLS-1$
				res.append(applicationAuthor);
				res.append("\n      Owner: "); //$NON-NLS-1$
				res.append(applicationOwner);
				res.append("\n      Contact: "); //$NON-NLS-1$
				res.append(applicationEmail);
				res.append("\n      License: "); //$NON-NLS-1$
				res.append(applicationlic_name);
				res.append("\n      License URL: "); //$NON-NLS-1$
				res.append(applicationlic_URL);
				res.append("\n      Terms of Service: "); //$NON-NLS-1$
				res.append(applicationUsage);
				res.append("\n      Documentation URL: "); //$NON-NLS-1$
				res.append(applicationURL);
				res.append("\n      System Parameters: "); //$NON-NLS-1$
				res.append(sysparam);
				res.append("\n =================================================== \n"); //$NON-NLS-1$
			}
			res.append("\nFramework Vendor: "); //$NON-NLS-1$
			res.append(context.getProperty(Constants.FRAMEWORK_VENDOR));
			res.append("\nFramework Version: "); //$NON-NLS-1$
			res.append(context.getProperty(Constants.FRAMEWORK_VERSION));
			if (webStartDate != null) {
				res.append("\nWeb services Start date: "); //$NON-NLS-1$
				res.append(dateToISOString(webStartDate));
			}
			if (activator.getStartDate() != null) {
				res.append("\nOSGi Bundle Start date: "); //$NON-NLS-1$
				res.append(dateToISOString(activator.getStartDate()));
			}
		}
		response.setEntity(res.toString(), media);
		response.setStatus(Status.SUCCESS_OK);
	}
	
	private String getSystemParameters() {
		try {
			ServiceReference<ISystemParameters> sr = activator.getContext().getServiceReference(ISystemParameters.class);
			if (sr != null) {
				ISystemParameters sp = activator.getContext().getService(sr);
				if (sp != null) {
					return sp.getSystemParameters();
				}
			}
		} catch (Exception e) {}
		return ""; //$NON-NLS-1$
	}

	private String escapeXML(String text) {
		if (text == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder(text.length());
		for(char c: text.toCharArray()) {
			switch (c) {
			case '<':
				sb.append("&lt;"); //$NON-NLS-1$
				break;
			case '\'':
				sb.append("&apos;"); //$NON-NLS-1$
				break;
			case '>':
				sb.append("&gt;"); //$NON-NLS-1$
				break;
			case '&':
				sb.append("&amp;"); //$NON-NLS-1$
				break;
			default:
				sb.append(c);
			}
		}
		return sb.toString();
	}
	
	private String escapeJSON(String text) {
		if (text == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder(text.length());
		for(char c: text.toCharArray()) {
			switch (c) {
			case '"':
				sb.append("\\\""); //$NON-NLS-1$
				break;
			case '\t':
				sb.append(' ');
				break;
			case '\n':
				sb.append(' ');
				break;
			case '\r':
				sb.append(' ');
				break;
			default:
				sb.append(c);
			}
		}
		return sb.toString();
	}

	private String dateToISOString(Date date) {
		Calendar calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		calendar.setTime(date);
		calendar.set(Calendar.ZONE_OFFSET, 0);
		return String.format("%04d-%02d-%02dT%02d:%02d:%02d", //$NON-NLS-1$
				calendar.get(Calendar.YEAR), //
				calendar.get(Calendar.MONTH) + 1, //
				calendar.get(Calendar.DAY_OF_MONTH), //
				calendar.get(Calendar.HOUR_OF_DAY), //
				calendar.get(Calendar.MINUTE), //
				calendar.get(Calendar.SECOND));
	}
	
	private String jsonString(final String key, final String value) {
		return String.format("\"%s\":\"%s\"", key, escapeJSON(value));
	}
}