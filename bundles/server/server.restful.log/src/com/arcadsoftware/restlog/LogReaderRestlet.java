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
package com.arcadsoftware.restlog;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;

import org.osgi.service.log.LogEntry;
import org.osgi.service.log.LogService;
import org.restlet.Context;
import org.restlet.Restlet;
import org.restlet.data.Form;
import org.restlet.data.MediaType;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.connection.IConnectionUserBean;

import org.restlet.Request;
import org.restlet.Response;

/**
 * This Restlet return the current log in XML only.
 * 
 * TODO Take into account PUT method (add a string to the log).
 * TODO Take into account DELETE to purge the log.
 * TODO Take into account HEADER to enable proxies.
 */
public class LogReaderRestlet extends Restlet {

	private static final ArrayList<MediaType> SUPPORTEDMEDIATYPES = new ArrayList<MediaType>(5);
	
	static {
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_HTML); // Required to support Internal Explorer 9>
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_XML);
		SUPPORTEDMEDIATYPES.add(MediaType.TEXT_PLAIN);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_XML);
		SUPPORTEDMEDIATYPES.add(MediaType.APPLICATION_XHTML);
	}
	
	private final Activator activator;
	
	public LogReaderRestlet(Context context, Activator activator) {
		super(context);
		this.activator = activator;
	}

	@Override
	public void handle(Request request, Response response) {
		IConnectionUserBean user = (IConnectionUserBean) request.getAttributes().get(IConnectionUserBean.CONNECTED_USER);
		if ((user == null) || !user.getProfile().hasRight(1)) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		LogEntry[] log = activator.getLog();
		MediaType media = request.getClientInfo().getPreferredMediaType(SUPPORTEDMEDIATYPES);
		int level = getLevel(request);
		String bundle = getBundle(request);
		if (MediaType.TEXT_XML.equals(media) || MediaType.APPLICATION_XML.equals(media)) {
			StringBuffer res = new StringBuffer();
			res.append("<?xml version=\"1.0\" encoding=\"utf-8\" ?>"); //$NON-NLS-1$;
			res.append("<log>"); //$NON-NLS-1$;
			for (LogEntry entry: log) {
				if (isListable(level, bundle, entry)) {
					res.append("<entry level=\""); //$NON-NLS-1$;
					res.append(convertLevel(entry.getLevel()));
					res.append("\">"); //$NON-NLS-1$;
					String mes = entry.getMessage();
					if (mes == null) {
						mes = ""; //$NON-NLS-1$
					} else {
						mes = mes.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
					}
					res.append("<message>" + mes + "</message>"); //$NON-NLS-1$ //$NON-NLS-2$
					if (entry.getBundle() != null) {
						res.append("<bundle name=\""+entry.getBundle().getSymbolicName()+"\" version=\""+entry.getBundle().getVersion().toString()+"\">" + entry.getBundle().getLocation() + "</bundle>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
					if (entry.getServiceReference() != null) {
						res.append("<service>" + entry.getServiceReference().getClass().getName() + "</service>"); //$NON-NLS-1$ //$NON-NLS-2$
						//TODO log service properties
					}
					if (entry.getException() != null) {
						res.append("<exception>"+entry.getException().toString()+"</exception>"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					res.append("</entry>"); //$NON-NLS-1$;
				}
			}
			res.append("</log>"); //$NON-NLS-1$;
			response.setEntity(res.toString(), media);
		} else if (MediaType.APPLICATION_JSON.equals(media)) {
			StringBuffer res = new StringBuffer("["); //$NON-NLS-1$
			boolean first = true;
			for (LogEntry entry: log) {
				if (isListable(level, bundle, entry)) {
					if (first) {
						first = false;
					} else {
						res.append(',');
					}
					res.append("{\"level\":\""); //$NON-NLS-1$
					res.append(convertLevel(entry.getLevel()));
					res.append("\",message\":\""); //$NON-NLS-1$
					res.append(escapeJSON(entry.getMessage()));
					if (entry.getBundle() != null) {
						res.append(",{\"bundle\":{\"name\":\""); //$NON-NLS-1$
						res.append(escapeJSON(entry.getBundle().getSymbolicName()));
						res.append("\",\"version\":\""); //$NON-NLS-1$
						res.append(escapeJSON(entry.getBundle().getVersion().toString()));
						res.append("\",\"location\":\""); //$NON-NLS-1$
						res.append(escapeJSON(entry.getBundle().getLocation()));
						res.append("\"}"); //$NON-NLS-1$
					} else {
						res.append('"');
					}
					if (entry.getServiceReference() != null) {
						res.append(",\"service\":\""); //$NON-NLS-1$
						res.append(escapeJSON(entry.getServiceReference().getClass().getName()));
						res.append('"');
					}
					if (entry.getException() != null) {
						res.append(",\"exception\":\""); //$NON-NLS-1$
						res.append(escapeJSON(entry.getException().toString()));
						res.append('"');
					}
					res.append('}');
				}
			}
			res.append(']');
			response.setEntity(res.toString(), media);
		} else if (MediaType.TEXT_HTML.equals(media) || MediaType.APPLICATION_XHTML.equals(media)) {
			StringBuffer res = new StringBuffer();
			res.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>Log</title><link type=\"text/css\" href=\"/css/service/status.css\" rel=\"stylesheet\" /></head><body><p>"); //$NON-NLS-1$;
			for (LogEntry entry: log) {
				if (isListable(level, bundle, entry)) {
					res.append("<div class=\"tt\""); //$NON-NLS-1$
					switch (entry.getLevel()) {
					case LogService.LOG_ERROR: res.append(" style=\"color: red;\""); break; //$NON-NLS-1$
					case LogService.LOG_WARNING: res.append(" style=\"color: orange;\""); break; //$NON-NLS-1$
					case LogService.LOG_DEBUG: res.append(" style=\"color: #333333;\""); break; //$NON-NLS-1$
					}
					res.append('>');
					res.append(entry.getMessage());
					if (((entry.getBundle() != null) && (bundle == null)) || (entry.getServiceReference() != null)) {
						res.append("<span class=\"ttt\">"); //$NON-NLS-1$
					}
					if ((entry.getBundle() != null) && (bundle == null)) {
						res.append("<strong>Bundle name</strong>: "); //$NON-NLS-1$
						res.append(entry.getBundle().getSymbolicName());
					}
					if (entry.getServiceReference() != null) {
						res.append("<strong>Service</strong>: "); //$NON-NLS-1$
						res.append(entry.getServiceReference().getClass().getName());
						//TODO log service properties
					}
					if ((entry.getBundle() != null) || (entry.getServiceReference() != null)) {
						res.append("</span>"); //$NON-NLS-1$
					}
					if (entry.getException() != null) {
						res.append("<br/><strong>Exception</strong>: "); //$NON-NLS-1$
						res.append(entry.getException().getLocalizedMessage());
						res.append("</p><pre>"); //$NON-NLS-1$
						StringWriter sw = new StringWriter();
						entry.getException().printStackTrace(new PrintWriter(sw));
						res.append(sw.toString());
						res.append("</pre>"); //$NON-NLS-1$
					}
					res.append("</div>"); //$NON-NLS-1$
				}
			}				
			res.append("</body></html>"); //$NON-NLS-1$	
			response.setEntity(res.toString(), media);
		} else {
			StringBuffer res = new StringBuffer();
			for (LogEntry entry: log) {
				if (isListable(level, bundle, entry)) {
					res.append(entry.getMessage());
					res.append('\n');
				}
			}
			response.setEntity(res.toString(), MediaType.TEXT_PLAIN);
		}
	}
	
	private String escapeJSON(String text) {
		if (text == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder(text.length());
		for (char c: text.toCharArray()) {
			switch (c) {
			case '"':
				sb.append('\\');
				sb.append('"');
				break;
			case '\n':
				sb.append('\\');
				sb.append('n');
				break;
			case '\t':
			case '\r':
				break;
			default:
				sb.append(c);
			}
		}
		return sb.toString();
	}

	protected String getBundle(Request request) {
		return request.getResourceRef().getQueryAsForm(true).getFirstValue("bundle"); //$NON-NLS-1$
	}

	protected int getLevel(Request request) {
		return activator.getLevel();
	}

	protected boolean isListable(int level, String bundle, LogEntry entry) {
		return (entry != null) && (entry.getLevel() <= level) && ((bundle == null) || bundle.equalsIgnoreCase(entry.getBundle().getSymbolicName()));
	}

	private Object convertLevel(int level) {
		switch (level) {
		case LogService.LOG_DEBUG: return "DEBUG"; //$NON-NLS-1$
		case LogService.LOG_ERROR: return "ERROR"; //$NON-NLS-1$
		case LogService.LOG_INFO: return "INFO"; //$NON-NLS-1$
		case LogService.LOG_WARNING: return "WARNING"; //$NON-NLS-1$
		default: return "Unknown";  //$NON-NLS-1$
		}
	}

}
