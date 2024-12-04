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
package com.arcadsoftware.connection.internal;

import java.io.File;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.rest.BaseResource;
import com.arcadsoftware.rest.OSGiResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.connection.ApplicationStatePlot;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IApplicationStateBroadcaster;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IPatchUserCredential;
import com.arcadsoftware.rest.connection.IProfileRightsListService;
import com.arcadsoftware.rest.connection.IUpdatableCredential;
import com.arcadsoftware.rest.connection.Right;
import com.arcadsoftware.rest.connection.RightInfo;
import com.thoughtworks.xstream.io.xml.CompactWriter;

/*
 * This resource identify the current user and can return different informations about this user.
 */
public class CurrentUserResource extends OSGiResource {

	private ConnectionUserBean user;
	private IConnectionCredential cc;
	private String oldp;

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		getAllowedMethods().add(Method.POST);
		getAllowedMethods().add(Method.PUT);
		// Retrieve the connected user information.
		try {
			user = (ConnectionUserBean) getRequest().getAttributes().get(ConnectionUserBean.CONNECTED_USER);
			cc = (IConnectionCredential) getRequest().getAttributes().get(IConnectionCredential.CONNECTED_CREDENTIAL);
			oldp = user.getPassword();
			user = (ConnectionUserBean) user.clone();
			if (cc instanceof IPatchUserCredential) {
				((IPatchUserCredential) cc).patchUser(user);
				if (user.getPassword() != null) {
					oldp = user.getPassword();
				}
			}
		} catch (Exception e) {
			getOSGiApplication().getActivator().debug(e.getLocalizedMessage());
			getResponse().setStatus(Status.SERVER_ERROR_INTERNAL);
		}
		// If there is no user information then the resource is not available.
		setExisting(user != null);
		// Add accepted representations.
		addVariants(MediaType.APPLICATION_XHTML, MediaType.TEXT_HTML, MediaType.TEXT_PLAIN, MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (MediaType.APPLICATION_W3C_SCHEMA.equals(variant.getMediaType())) {
			File file = Activator.getInstance().getSchema("/schema/currentUserBean.xsd"); //$NON-NLS-1$
			if ((file != null) && file.isFile()) {
				return XMLRepresentation.fromFile(file, MediaType.APPLICATION_W3C_SCHEMA);
			}
			throw new ResourceException(Status.CLIENT_ERROR_GONE);
		}
		if (MediaType.TEXT_PLAIN.equals(variant.getMediaType())) {
			// Return only the user full name.
			return new StringRepresentation(user.getFullname(), MediaType.TEXT_PLAIN, Language.ENGLISH, CharacterSet.UTF_8);
		}
		HashMap<String, ApplicationStatePlot> stts = new HashMap<>();
		Language language = getClientPreferedLanguage();
		for (IApplicationStateBroadcaster broadcaster: getOSGiServices(IApplicationStateBroadcaster.class)) {
			ApplicationStatePlot s = broadcaster.broadcast(user, language);
			if ((s != null) && (s.getHypertext() != null)) {
				ApplicationStatePlot o = stts.get(s.getCode());
				if ((o == null) || (s.getLevel().ordinal() > o.getLevel().ordinal())) {
					stts.put(s.getCode(), s);
				}
			}
		}
		ArrayList<ApplicationStatePlot> states = new ArrayList<>(stts.size());
		int minspawn = Integer.MAX_VALUE;
		for (ApplicationStatePlot asp: stts.values()) {
			states.add(asp);
			if ((asp.getLifeSpan() > 0) && (asp.getLifeSpan() < minspawn)) {
				minspawn = asp.getLifeSpan();
			}
		}
		Collections.sort(states);
		// Use a converter for that add a textual representation of the rights (use the database).
		IProfileRightsListService service = getOSGiService(IProfileRightsListService.class);
		List<RightInfo> rights;
		if (service == null) {
			rights = new ArrayList<>();
			for (Right r: user.getProfile().getRights()) {
				rights.add(new RightInfo(r.getId(), r.getParam(), Integer.toString(r.getId()), null));
			}
		} else {
			rights = service.getProfileRights(user.getProfile(), language);
		}
		Collections.sort(rights);
		if (MediaType.TEXT_XML.equals(variant.getMediaType()) ||
				MediaType.APPLICATION_XML.equals(variant.getMediaType())) {
			StringWriter sw = new StringWriter();
			CompactWriter writer = new CompactWriter(sw); 
			writer.startNode("user"); //$NON-NLS-1$
			if (user.getId() > 0) {
				writer.addAttribute("id", Integer.toString(user.getId())); //$NON-NLS-1$
			}			
			if (user.getPrincipal() > 0) {
				writer.addAttribute("principal", Integer.toString(user.getPrincipal())); //$NON-NLS-1$
			}			
			if (user.isChangePWD()) {
				writer.addAttribute("changePWD", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (user.isCanChangePWD()) {
				writer.addAttribute("canChangePWD", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (user.isLocked()) {
				writer.addAttribute("locked", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (user.getLogin() != null) {
				writer.addAttribute("login", escapeXML(user.getLogin())); //$NON-NLS-1$
			}
			if (user.getFullname() != null) {
				writer.addAttribute("fullname", escapeXML(user.getFullname())); //$NON-NLS-1$
			}
			writer.startNode("profile"); //$NON-NLS-1$
			for (RightInfo r: rights) {
				writer.startNode("right"); //$NON-NLS-1$
				writer.addAttribute("id", Integer.toString(r.getId())); //$NON-NLS-1$
				if (r.getParam() != 0) {
					writer.addAttribute("param", Integer.toString(r.getParam())); //$NON-NLS-1$
				}
				writer.addAttribute("code", r.getCode()); //$NON-NLS-1$
				if (r.getLabel() != null) {
					writer.setValue(r.getLabel());
				}
				writer.endNode();
			}
			writer.endNode();
			writer.startNode("status"); //$NON-NLS-1$
			if (minspawn < Integer.MAX_VALUE) {
				writer.addAttribute("lifespawn", Integer.toString(minspawn)); //$NON-NLS-1$
			}
			for (ApplicationStatePlot plot: states) {
				switch (plot.getLevel()) {
				case INFO:
					writer.startNode("info"); //$NON-NLS-1$
					break;
				case WARN:
					writer.startNode("warn"); //$NON-NLS-1$
					break;
				case CRITICAL:
					writer.startNode("critical"); //$NON-NLS-1$
					break;
				case BLOCKER:
					writer.startNode("blocker"); //$NON-NLS-1$
					break;
				}
				writer.addAttribute("code", plot.getCode()); //$NON-NLS-1$
				writer.setValue(plot.getHypertext());
				writer.endNode();
			}
			writer.endNode();
			writer.endNode();
			return new XMLRepresentation(sw.toString(), variant.getMediaType());
		}
		if (MediaType.APPLICATION_JSON.equals(variant.getMediaType())) {
			JSONObject result = new JSONObject();
			try {
				if (user.getId() > 0) {
					result.put("id", user.getId()); //$NON-NLS-1$
				}			
				if (user.getPrincipal() > 0) {
					result.put("principal", user.getPrincipal()); //$NON-NLS-1$
				}			
				if (user.isChangePWD()) {
					result.put("changePWD", true); //$NON-NLS-1$
				}
				if (user.isCanChangePWD()) {
					result.put("canChangePWD", true); //$NON-NLS-1$
				}
				if (user.isLocked()) {
					result.put("locked", true); //$NON-NLS-1$
				}
				if (user.getLogin() != null) {
					result.put("login", user.getLogin()); //$NON-NLS-1$
				}
				if (user.getFullname() != null) {
					result.put("fullname", user.getFullname()); //$NON-NLS-1$
				}
				JSONArray p = new JSONArray();
				for (RightInfo r: rights) {
					p.put(r.toJSON());
				}
				result.put("profile", p); //$NON-NLS-1$
				JSONObject st = new JSONObject();
				st.put("lifespawn", minspawn); //$NON-NLS-1$
				JSONArray plots = new JSONArray();
				for (ApplicationStatePlot plot: states) {
					plots.put(plot.toJSON());
				}			
				st.put("plots", plots); //$NON-NLS-1$
				result.put("status", st); //$NON-NLS-1$
			} catch (JSONException e) {}
			return new StringRepresentation(result.toString(), MediaType.APPLICATION_JSON, Language.ENGLISH, CharacterSet.UTF_8);
		}
		if (isHTML(variant)) {
			StringBuilder sb = new StringBuilder("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>"); //$NON-NLS-1$
			String fullname = escapeXML(user.getFullname());
			sb.append(Activator.getMessage("currentuser_title", language) + fullname); //$NON-NLS-1$
			sb.append("</title></head><body><p>"); //$NON-NLS-1$ 
			sb.append(String.format(Activator.getMessage("currentuser_intro", language), fullname, escapeXML(user.getLogin()), user.getId(), user.getPrincipal())); //$NON-NLS-1$
			if (user.isLocked()) {
				sb.append("</p><p><strong>"); //$NON-NLS-1$ 
				sb.append(Activator.getMessage("currentuser_locked", language)); //$NON-NLS-1$
				sb.append("</strong>"); //$NON-NLS-1$ 
			} else if (user.isCanChangePWD()) {
				sb.append("</p><p>"); //$NON-NLS-1$ 
				sb.append(Activator.getMessage("currentuser_canchangepwd", language)); //$NON-NLS-1$
				if (user.isChangePWD()) {
					sb.append("</p><p><strong>"); //$NON-NLS-1$ 
					sb.append(Activator.getMessage("currentuser_andchangepwd", language)); //$NON-NLS-1$
					sb.append("</strong>"); //$NON-NLS-1$ 
				}
			} else if (user.isChangePWD()) {
				sb.append("</p><p><strong>"); //$NON-NLS-1$ 
				sb.append(Activator.getMessage("currentuser_changepwd", language)); //$NON-NLS-1$
				sb.append("</strong>"); //$NON-NLS-1$ 
			}
			sb.append("</p><h3>"); //$NON-NLS-1$ 
			sb.append(Activator.getMessage("currentuser_profile", language)); //$NON-NLS-1$
			sb.append("</h3><ul>"); //$NON-NLS-1$ 
			for (RightInfo r: rights) {
				sb.append("<li>"); //$NON-NLS-1$
				sb.append(r.getLabel());
				sb.append(" <i>["); //$NON-NLS-1$
				sb.append(r.getCode());
				sb.append(" = "); //$NON-NLS-1$
				sb.append(r.getId());
				if (r.getParam() > 0) {
					sb.append('/');
					sb.append(r.getParam());
				}
				sb.append("]</i></li>"); //$NON-NLS-1$ 
			}		
			if (!states.isEmpty()) {
				sb.append("</ul><h3>"); //$NON-NLS-1$ 
				sb.append(Activator.getMessage("currentuser_status", language)); //$NON-NLS-1$
				sb.append("</h3>"); //$NON-NLS-1$ 
				if (minspawn < Integer.MAX_VALUE) {
					sb.append("<p>"); //$NON-NLS-1$ 
					sb.append(String.format(Activator.getMessage("currentuser_lifespawn", language), minspawn)); //$NON-NLS-1$
					sb.append("</p>"); //$NON-NLS-1$ 
				}
				sb.append("<ul>"); //$NON-NLS-1$ 
				for (ApplicationStatePlot plot: states) {
					switch (plot.getLevel()) {
					case INFO:
						sb.append("<li><i>"); //$NON-NLS-1$
						sb.append(Activator.getMessage("currentuser_status_info", language)); //$NON-NLS-1$
						break;
					case WARN:
						sb.append("<li>"); //$NON-NLS-1$
						sb.append(Activator.getMessage("currentuser_status_warn", language)); //$NON-NLS-1$
						break;
					case CRITICAL:
						sb.append("<li><strong>"); //$NON-NLS-1$
						sb.append(Activator.getMessage("currentuser_status_critical", language)); //$NON-NLS-1$
						break;
					case BLOCKER:
						sb.append("<li><strong><i>"); //$NON-NLS-1$
						sb.append(Activator.getMessage("currentuser_status_blocker", language)); //$NON-NLS-1$
						break;
					}
					sb.append(' ');
					sb.append(plot.getCode()); //$NON-NLS-1$
					sb.append(": "); //$NON-NLS-1$
					sb.append(plot.getHypertext());
					switch (plot.getLevel()) {
					case INFO:
						sb.append("</i></li>"); //$NON-NLS-1$
						break;
					case WARN:
						sb.append("</li>"); //$NON-NLS-1$
						break;
					case CRITICAL:
						sb.append("</strong></li>"); //$NON-NLS-1$
						break;
					case BLOCKER:
						sb.append("</i></strong></li>"); //$NON-NLS-1$
						break;
					}
				}
			}
			sb.append("</ul></body></html>"); //$NON-NLS-1$ 
			return new StringRepresentation(sb.toString(),MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
		}
		throw new ResourceException(Status.CLIENT_ERROR_NOT_ACCEPTABLE);
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

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		return put(entity, variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		Language language = BaseResource.getClientPreferedLanguage(getRequest());
		if (!user.isCanChangePWD()) {
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, Activator.getMessage("cannotChangePWD", language)); //$NON-NLS-1$
		}
		if (!(cc instanceof IUpdatableCredential)) {
			throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED, Activator.getMessage("cannotChangePWD", language)); //$NON-NLS-1$
		}
		Form form = getRequestForm();
		String newp = form.getFirstValue("newpassword"); //$NON-NLS-1$
		if ((newp == null) || newp.isEmpty()) {
			Activator.getInstance().debug(Messages.CurrentUserResource_Error_badparamters);
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST,
					Activator.getMessage("empty_new_pwd", language)); //$NON-NLS-1$
		}
		if (newp.equals(oldp)) {
			if (user.isChangePWD()) {
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("same_pwd", language)); //$NON-NLS-1$
			}
		} else {
			final char[] oldpc = oldp.toCharArray();
			final char[] newpc = newp.toCharArray();
			String result = null;
			try {
				result = ((IUpdatableCredential) cc).updatePassword(user, oldpc, newpc, language);
			} finally {
				Crypto.clear(oldpc);
				Crypto.clear(newpc);
			}
			if (result == null) {
				throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED, Activator.getMessage("no_authentification", language)); //$NON-NLS-1$
			}
			if (result.length() > 0) {
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, result);
			}
			broadcastChgPwd();
		}
		getResponse().setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}

	private void broadcastChgPwd() {
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		props.put("uid", user.getId()); //$NON-NLS-1$
		String login = user.getLogin();
		if (login == null) {
			login = "";
		}
		props.put("login", login); //$NON-NLS-1$
		props.put("code", "chg_pwd"); //$NON-NLS-1$ //$NON-NLS-2$
		String text = user.getFullname();
		if ((text == null) || text.isEmpty()) {
			text = ((IConnectionCredential) cc).getText();
			if ((text == null) || text.isEmpty()) {
				text = login;
			} else if (login.isEmpty()) {
				props.put("login", text);
			}
		}
		props.put("message", String.format("The user %s has a new password.", text)); //$NON-NLS-1$
		props.put("date", new Date()); //$NON-NLS-1$
		EventAdmin ad = getOSGiService(EventAdmin.class);
		if (ad != null) {
			ad.postEvent(new Event("com/arcadsoftware/user/action", (Dictionary<String,Object>) props)); //$NON-NLS-1$
		}
	}
}
