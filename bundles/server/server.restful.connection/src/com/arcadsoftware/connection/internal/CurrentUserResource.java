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
package com.arcadsoftware.connection.internal;

import java.io.File;
import java.util.Date;
import java.util.Dictionary;
import java.util.Hashtable;

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
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.connection.AutoProfile;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IPatchUserCredential;
import com.arcadsoftware.rest.connection.IUpdatableCredential;
import com.arcadsoftware.rest.connection.Profile;
import com.arcadsoftware.rest.connection.Right;
import com.arcadsoftware.rest.connection.XmlCurrentUserStream;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.json.JsonHierarchicalStreamDriver;

/*
 * This resource identify the current user and can return different informations about this user.
 */
public class CurrentUserResource extends BaseResource {

	private ConnectionUserBean user = null;
	private IConnectionCredential cc = null;
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
			Activator.getInstance().debug(e.getLocalizedMessage());
			getResponse().setStatus(Status.SERVER_ERROR_INTERNAL);
		}
		// If there is no user information then the resource is not available.
		setExisting(user != null);
		// Add accepted representations.
		getVariants().add(new Variant(MediaType.TEXT_PLAIN));
		getVariants().add(new Variant(MediaType.TEXT_XML));
		getVariants().add(new Variant(MediaType.APPLICATION_JSON));
		getVariants().add(new Variant(MediaType.APPLICATION_XML));
		getVariants().add(new Variant(MediaType.APPLICATION_W3C_SCHEMA));//application/x-xsd+xml
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (MediaType.TEXT_XML.equals(variant.getMediaType()) ||
				MediaType.APPLICATION_XML.equals(variant.getMediaType())) {
			XmlCurrentUserStream x = new XmlCurrentUserStream();
			// TODO Add a converter for a right that add a textual representation of the rights (use the database).
			return new XMLRepresentation(x.toXML(user), variant.getMediaType());
		}
		if (MediaType.APPLICATION_JSON.equals(variant.getMediaType())) {
			XStream x = new XStream(new JsonHierarchicalStreamDriver());
			x.alias("user", ConnectionUserBean.class); //$NON-NLS-1$
			x.alias("profile", Profile.class); //$NON-NLS-1$
			x.alias("autoprofile", AutoProfile.class); //$NON-NLS-1$
			x.alias("right", Right.class); //$NON-NLS-1$
			return new StringRepresentation(x.toXML(user), MediaType.APPLICATION_JSON, Language.ENGLISH, CharacterSet.UTF_8);
		}
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
		throw new ResourceException(Status.CLIENT_ERROR_NOT_ACCEPTABLE);
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		return put(entity, variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		Language language = BaseResource.getClientPreferedLanguage(getRequest());
		if (!user.isCanChangePWD()) {
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, Activator.getInstance().getMessage("cannotChangePWD", language)); //$NON-NLS-1$
		}
		if (!(cc instanceof IUpdatableCredential)) {
			throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED, Activator.getInstance().getMessage("cannotChangePWD", language)); //$NON-NLS-1$
		}
		Form form = getRequestForm();
		String newp = form.getFirstValue("newpassword"); //$NON-NLS-1$
		if ((newp == null) || newp.isEmpty()) {
			Activator.getInstance().debug(Messages.CurrentUserResource_Error_badparamters);
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST,
					Activator.getInstance().getMessage("empty_new_pwd", language)); //$NON-NLS-1$
		}
		String op = form.getFirstValue("oldpassword"); //$NON-NLS-1$
		if ((op != null) && !op.isEmpty()) {
			if ((oldp != null) && !oldp.isEmpty()) {
				if (!oldp.equals(op)) {
					throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getInstance().getMessage("invalid_old_pwd", language)); //$NON-NLS-1$
				}
			}
		} else {
			op = oldp;
		}
		if ((op == null) || op.isEmpty()) {
			Activator.getInstance().debug(Messages.CurrentUserResource_Debug_Empty_old_password);
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getInstance().getMessage("empty_old_pwd", language)); //$NON-NLS-1$
		}
		if (newp.equals(op)) {
			if (user.isChangePWD()) {
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getInstance().getMessage("same_pwd", language)); //$NON-NLS-1$
			}
		} else {
			final char[] oldpc = op.toCharArray();
			final char[] newpc = newp.toCharArray();
			String result = null;
			try {
				result = ((IUpdatableCredential) cc).updatePassword(user, oldpc, newpc, language);
			} finally {
				Crypto.clear(oldpc);
				Crypto.clear(newpc);
			}
			if (result == null) {
				throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED, Activator.getInstance().getMessage("no_authentification", language)); //$NON-NLS-1$
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
		EventAdmin ad = Activator.getInstance().getService(EventAdmin.class);
		if (ad != null) {
			ad.postEvent(new Event("com/arcadsoftware/user/action", (Dictionary<String,Object>) props)); //$NON-NLS-1$
		}
	}
}
