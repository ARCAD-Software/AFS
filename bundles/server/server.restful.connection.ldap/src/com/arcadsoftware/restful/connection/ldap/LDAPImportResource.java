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
package com.arcadsoftware.restful.connection.ldap;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.restlet.data.Form;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.XMLRepresentation;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;

/* Process to mannually import of new Users from LDAP :
 * GET = always return the list of import-ready users. Object returned are virtuals User object like they should be imported.
 * PUT/POST = import given users :
 * 
 * support multiple "id" or "login" parameters and support multiple addresses like :
 * 
 * /admin/ldap/import/1+2+3+4
 * /admin/ldap/import/login+login+login+login
 *
 * It is highly recommended to use "login" as parameters.
 * Using "User" id may be dangerous because theses Id are just an increment into the selected list if LDAP server do not always
 * return object in the same order (like ApacheDS) then theses ID will be false.
 * 
 * Other parameters :
 * 
 * all - import all selected users.
 * search - define the LDAP search pattern (must be a valid LDAP filter pattern).
 * profile - the profile Id associated to imported user (default 2).
 * principal or client - the Client ID associated to imported users (default 1).
 */
public class LDAPImportResource extends LDAPUserLinkedResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		setExisting((getLdapAuthentificationService() != null) && getLdapAuthentificationService().isUserImportEnabled());
		if (hasRight(31)) { // = User Create
			getAllowedMethods().add(Method.GET);
			getAllowedMethods().add(Method.POST);
			getAllowedMethods().add(Method.PUT);
		}
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		BeanMapList list = selectableLDAPUsers();
		if (isJSON(variant)) {
			return new JSONRepresentation(new JSonBeanMapStream().toXML(Activator.TYPE_USER, list), getClientPreferedLanguage());
		}
		return new XMLRepresentation(new XmlBeanMapStream().toXML(Activator.TYPE_USER, list), getClientPreferedLanguage());
	}
	
	private BeanMapList selectableLDAPUsers() throws ResourceException {
		final Form form = getRequestForm();
		final String searchPattern = form.getFirstValue("search"); //$NON-NLS-1$
		if ((searchPattern == null) || (searchPattern.length() == 0)) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The parameter \"search\" is mandatory.");
		}
		int timeLimit;
		try {
			timeLimit = Integer.parseInt(form.getFirstValue("limit.time", true, "0")); //$NON-NLS-1$ //$NON-NLS-2$
			if (timeLimit < 0) {
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The parameter \"limit.time\" must be positive or equal to zero.");
			}
		} catch (NumberFormatException e) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The parameter \"limit.time\" must be an integer value.");
		}
		int countLimit;
		try {
			countLimit = Integer.parseInt(form.getFirstValue("limit.count", true, "0")); //$NON-NLS-1$ //$NON-NLS-2$
			if (countLimit < 0) {
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The parameter \"limit.count\" must be positive or equal to zero.");
			}
		} catch (NumberFormatException e) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The parameter \"limit.count\" must be an integer value.");
		}
		Map<String, String> userMap = new HashMap<String, String>();
		for(String p: form.getNames()) {
			if (p.startsWith("map.")) { //$NON-NLS-1$
				userMap.put(p.substring(4), form.getFirstValue(p));
			}
		}
		LDAPConnection cn = getLDAPConnection();
		if (cn == null) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN, "Only an User connected through the LDAP service can import users from it. You have to mannually create such a login connection and reconnect with it before trying to import other users.");
		}
		LDAPException e = null;
		try {
			return getLdapAuthentificationService().listSelectableUsers(cn, searchPattern, countLimit, timeLimit, userMap);
		} catch (LDAPException ee) {
			getActivator().error(ee);
			e = ee;
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, ee.getDiagnosticMessage());
		} finally {
			closeLDAPConnection(cn, e);
		}
	}

	@Override
	protected Representation put(Representation representation, Variant variant)
			throws ResourceException {
		return post(representation, variant);
	}
	
	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		Form form = getRequestForm();
		ArrayList<Integer> profiles = new ArrayList<Integer>();
		String s = form.getFirstValue("profiles"); //$NON-NLS-1$
		if (s != null) {
			for(String i:s.split(" ")) { //$NON-NLS-1$
				try {
					profiles.add(Integer.decode(i));
				} catch (NumberFormatException e) {}
			}
		}
		if (!profiles.isEmpty() && !hasRight(37)) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN, "You do not own the suffisent access right to link users to specifics profiles, retry without assignating any profile to the imported user or contact an administrator.");
		}
		BeanMap userModel = new BeanMap(Activator.TYPE_USER, form);
		userModel.remove("profiles"); //$NON-NLS-1$
		userModel.remove("search"); //$NON-NLS-1$
		userModel.remove("ldap.login"); //$NON-NLS-1$
		userModel.remove("limit.time"); //$NON-NLS-1$
		userModel.remove("limit.count"); //$NON-NLS-1$
		userModel.removePrefix("map.", true); //$NON-NLS-1$
		MetaDataEntity uentity = MetaDataEntity.loadEntity(Activator.TYPE_USER);
		MetaDataEntity lentity = MetaDataEntity.loadEntity(Activator.LDAPAUTH);
		StringBuilder errors = new StringBuilder();
		boolean added = false;
		final String searchPattern = form.getFirstValue("search"); //$NON-NLS-1$
		if (searchPattern != null) {
			BeanMapList users = selectableLDAPUsers();
			if ((users == null) || (users.size() == 0)) {
				return null;
			} else {
				errors = new StringBuilder();
				for(String login: getLogins(form)) {
					BeanMap user = users.getFirst("ldap.login", login); //$NON-NLS-1$
					if (user == null) {
						continue;
					}
					users.remove(user);
					user.addAll(userModel);
					user.remove("ldap.login"); //$NON-NLS-1$
					int uid = createUser(uentity, user);
					if (uid > 0) {
						added = true;
						users.remove(user);
						createLogin(lentity, login, uid);
						for (Integer profile : profiles) {
							linktoProfile(uentity, uid, profile);
						}
					} else {
						errors.append(String.format(Messages.LDAPImportResource_ERROR_UserLoginError, login));
					}
				}
				for(Integer id:getIds(form)) {
					BeanMap user = users.getFirst(id);
					if (user == null) {
						continue;
					}
					String login = user.getString("ldap.login", null);
					if (login == null) {
						continue;
					}
					user.addAll(userModel);
					user.remove("ldap.login"); //$NON-NLS-1$
					int uid = createUser(uentity, user);
					if (uid > 0) {
						added = true;
						users.remove(user);
						createLogin(lentity, login, uid);
						for (Integer profile : profiles) {
							linktoProfile(uentity, uid, profile);
						}
					} else {
						errors.append(String.format(Messages.LDAPImportResource_ERROR_UserIdError, id));
					}
				}				
			}
		} else {
			final String login = form.getFirstValue("ldap.login"); //$NON-NLS-1$
			int uid = createUser(uentity, userModel);
			if (uid > 0) {
				added = true;				
				createLogin(lentity, login, uid);
				for (Integer profile : profiles) {
					linktoProfile(uentity, uid, profile);
				}
			} else {
				errors.append(String.format(Messages.LDAPImportResource_ERROR_UserIdError, login));
			}
		}
		if (errors.length() > 0) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, errors.toString());
		}
		if (added) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		}
		return null;
	}
	
	private String[] getLogins(Form form) {
		ArrayList<String> result = new ArrayList<String>();
		Collections.addAll(result, form.getValuesArray("ldap.login")); //$NON-NLS-1$
		String s = getAttribute("id"); //$NON-NLS-1$
		if ((s != null) && (s.length() > 0)) {
			for(String login: s.split("\\+")) { //$NON-NLS-1$
				try {
					// On ne récupère que les éléments qui ne sont pas des entiers...
					Integer.parseInt(login);
				} catch (NumberFormatException e) {
					result.add(login);
				}
			}
		}
		return result.toArray(new String[result.size()]);
	}
	
	private Integer[] getIds(Form form) {
		ArrayList<Integer> result = new ArrayList<Integer>();
		for(String id:form.getValuesArray("id")) { //$NON-NLS-1$
			try {
				int i = Integer.parseInt(id);
				if (i > 0) {
					result.add(i);
				}
			} catch (NumberFormatException e) {}
		}
		String s = getAttribute("id"); //$NON-NLS-1$
		if ((s != null) && (s.length() > 0)) {
			for(String login: s.split("\\+")) { //$NON-NLS-1$
				try {
					int i = Integer.parseInt(login);
					if (i > 0) {
						result.add(i);
					}
				} catch (NumberFormatException e) {
				}
			}
		}
		return result.toArray(new Integer[result.size()]);
	}

	private int createUser(MetaDataEntity entity, BeanMap user) {
		// TODO Tester si le user n'exite pas déjà (sans mapping...)
		// (Utiliser l'Email, ou une liste d'attribut à tester...)
		// Replace "code" of references by corresponding ID,
		// and remove "null" attributes...
		for (String key: user.keys()) {
			MetaDataAttribute att = entity.getAttribute(key);
			if (att == null) {
				//NO: that breaks the map! user.remove(key);
			} else if (att.isReference() && (!(user.get(key) instanceof Integer))) {
				String val = user.getString(key);
				if (val == null) {
					user.remove(key);
				} else {
					int i = 0;
					try {
						i = Integer.parseInt(val);
					} catch (NumberFormatException e) {
						// Convert link !!!
						i = getIdFromCode(att.getType(),val);
					}
					if (i <= 0) {
						user.remove(key);
					} else {
						user.put(key, i);
					}
				}
			}
		}
		user = entity.dataCreate(user);
		if (user == null) {
			return 0;
		}
		return user.getId();
	}

	private int getIdFromCode(String type, String val) {
		MetaDataEntity entity = MetaDataEntity.loadEntity(type);
		if (entity == null) {
			return 0;
		}
		MetaDataAttribute a = entity.getAttribute("code"); //$NON-NLS-1$
		if (a == null) {
			a = entity.getAttribute("name"); //$NON-NLS-1$
		}
		if (a == null) {
			return 0;
		}
		BeanMapList beans = entity.dataSelection("", false, a.getCode(), val);
		if ((beans == null) || (beans.size() == 0)) {
			return 0;
		}
		return beans.get(0).getId();
	}

	private void createLogin(MetaDataEntity entity, String lg, int uid) {
		BeanMap bean = entity.dataCreate(Activator.LDAPAUTH_LOGIN + ' ' + Activator.LDAPAUTH_USERID, lg, uid);
		if (bean != null) {
			getActivator().purgeConnectionCache();
			getActivator().setAuth(bean, null, 0);
		}
	}

	private void linktoProfile(MetaDataEntity entity, int uid, int pid) {
		if ((uid > 0) && (pid > 0)) {
			MetaDataLink link = entity.getLink(Activator.USER_PROFILELINK);
			if (link != null) {
				link.dataLinkTo(uid, pid);
			}
		}
	}
}