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

import java.util.ArrayList;
import java.util.Date;
import java.util.Dictionary;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.List;

import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.ChallengeRequest;
import org.restlet.data.ChallengeResponse;
import org.restlet.data.ChallengeScheme;
import org.restlet.data.Form;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;
import org.restlet.security.Authenticator;

import com.arcadsoftware.rest.BaseResource;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IBasicAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IPatchUserCredential;
import com.arcadsoftware.rest.connection.IWWWAuthentificationService;

public class CachedAuthentificator extends Authenticator {

	private static final long LASTERROR_LATENCY = 120000;
	private static final String HTTPPARAMETER_CLIENT_TRANSLATION = "localtranslate"; //$NON-NLS-1$	
	
	private final Activator activator;
	private volatile long lastErrorService;
	private volatile long lastErrorCache;
	private volatile long lastConnectionTiming;

	public CachedAuthentificator(Context context, Activator activator, Restlet next) {
		super(context);
		this.activator = activator;
		lastConnectionTiming = 500;
		setNext(next);
	}
	
	/*
	 * Check that the store date is still valid.
	 * 
	 * @param date
	 * @return true if the limit date is valid.
	 */
	private boolean checkDate(Date date) {
		final int duration = activator.getCacheDuration();
		if (duration <= 0) {
			return true;
		}
		final GregorianCalendar c = new GregorianCalendar();
		c.set(GregorianCalendar.SECOND, -duration);
		return c.getTime().before(date);
	}

	@Override
	protected boolean authenticate(Request request, Response response) {
		long timing = System.currentTimeMillis();
		// Si le service est inactif on refuse toute connection...
		if (activator.isInactivate()) {
			response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "inactivate")); //$NON-NLS-1$
			return false;
		}
		// Si aucun service n'est enregistré alors la connection ne peut aboutir.
		Object[] services = activator.getAuthServiceTracker().getServices();
		if ((services == null) || (services.length == 0)) {
			activator.debug(Messages.SecureGuard_NoAuthentificationService);
			response.setStatus(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, translateMessage(request, "notready")); //$NON-NLS-1$
			return false;
		}
		// si pas de challenge alors on renseigne les challenges possibles.
		ChallengeResponse cr = request.getChallengeResponse();
        if (cr == null) {
        	// Le client a envoyé une requête sans login+mdp on renvois une erreur avec la demande d'identifiant.
            response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED);
            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
            return false;
        }
        // Utilisation du Schema Basic.
        if (ChallengeScheme.HTTP_BASIC.equals(cr.getScheme())) {
        	return basicCachedAuthenticate(services, request, response, timing);
        }
        // utilisation d'un autre schema.
        boolean manage_error = true;
		for (Object o: services) {
			try {
				if (o instanceof IWWWAuthentificationService) {
					IConnectionUserBean user = ((IWWWAuthentificationService) o).checkCredential(request, response);
					if (user != null) {
						request.getAttributes().put(ConnectionUserBean.CONNECTED_USER, user);
						activator.checkUserConnection(user);
						broadcastSuccess(user);
						updateConnectionTiming(timing);
						return true;
					}
				}
			} catch (Exception e) {
				long t = System.currentTimeMillis();
				if ((t - lastErrorService) > LASTERROR_LATENCY) {
					activator.error("Error during authentification, some services are not correctly configured.", e);
					lastErrorService = t;
				} else {
					activator.debug("Error during authentification, some services are not correctly configured.", e);
				}
				if (e instanceof ResourceException) {
					manage_error = false;
					response.setStatus(((ResourceException) e).getStatus());
				}
			}
		}
		// pour des raison de sécurités on ne distingue pas un login incorrect d'une autre cause de refus.
		if (manage_error) {
			response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "wrong")); //$NON-NLS-1$
	        response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
		}
        broadcastFail(request);
        waitConnectionTiming(timing);
		return false;
	}

	private boolean basicCachedAuthenticate(Object[] services, Request request, Response response, long timing) {
        String identifier = request.getChallengeResponse().getIdentifier();
        char[] secret = request.getChallengeResponse().getSecret();
		// Ici on vérifie les paramètres d'identification en déléguant le processus 
		// à un service OSGi qualifié, c'est ce service qui décide lui-même s'il est qualifié ou pas.
		// Pour un même utilisateur plusieurs logins et services différents peuvent être employés.
		// 1. on cherche un service d'authentification qui accepte la requête.
		// Le premier service qui l'accepte renvois un "credential" non null.
		IConnectionCredential credential = null;
		for (Object o:services) {
			try {
				if (o instanceof IBasicAuthentificationService) {
					credential = ((IBasicAuthentificationService) o).generateCredential(request, identifier);
					if (credential != null) {
						break;
					}
				}
			} catch (Throwable e) {
				long t = System.currentTimeMillis();
				if ((t - lastErrorService) > LASTERROR_LATENCY) {
					activator.error("Error during authentification, some services are not correctly configured.", e);
					lastErrorService = t;
				} else {
					activator.debug("Error during authentification, some services are not correctly configured.", e);
				}
			}
		}
		// Aucun service n'a accepté de gérer cet identifiant: le login est incorrect.
		if (credential == null) {
			// pour des raison de sécurités on ne distingue pas un login incorrect d'un mot de passe incorrect !
			response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "wrong")); //$NON-NLS-1$
            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
            broadcastFail(request);
            waitConnectionTiming(timing);
            return false;
		}
		// 2. on recherche le credential dans le cache, grâce à son UniqueID.
		String uid = credential.getUniqueId();
		ConnectionUserBean user = activator.getCachedUser(uid);
		// On teste la validité de ce credential (il a peut être expiré).
		boolean trusted = false;
		if ((user != null) && checkDate(user.getStoreDate())) {
			// On charge alors le credential déjà caché. 
			IConnectionCredential oldcredential = user.getCredential(uid);
			// et s'il correspondait aux même paramètres utilisés actuellement alors on
			// peut conclure qu'il a passé les tests de connection avec succès.
			if ((oldcredential != null) && oldcredential.checkSecret(secret)) {
				// Blindage, on ne travaille que sur une copie éprouvée.
				credential = oldcredential;
				// Ce crédential n'est pas retesté
				trusted = true;
			}
		} else {
			// 3. On doit créer (ou recharger) les informations utilisateurs (elles sont périmées).
			// si necessaire on purge le cache.
			if (user != null) {
				activator.cachePurge(user.getUserType(),user.getId());
			}
			// Et on recharge les informations utilisateurs.
			int id = credential.loadUserId();
			// Support des identités virtuelles (id négatif).
			// Une identité virtuelle n'est pas associée à un "User", son Id
			// est utilisé pour contenir un niveau de "droit maximal"...
			if (id <= 0) {
				if ((credential.getUserType() == null) && (credential instanceof IPatchUserCredential)) {
					user = new ConnectionUserBean(id);
					((IPatchUserCredential) credential).patchUser(user);
				} else {
					// Aucune identification ne correspond à ce service bien qu'il l'est accepté.
					response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "deleted")); //$NON-NLS-1$
		            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
					return false;
				}
			} else if (credential.getUserType() != null) {
				// Chargement de l'utilisateur (par appel à un service connecté à la base de données).
				user = activator.createUser(credential.getUserType(), id);
			}
			if (user == null) {
				// Aucune identification ne correspond à ce service bien qu'il ai été accepté.
				response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "deleted")); //$NON-NLS-1$
	            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
				return false;
			}
		}
		// 4. Le crédential utilisé s'approprie l'utilisateur.
		// Cette implémentation est thread-safe si plusieurs services sont utilisés en 
		// parallèle chacun utilisera sa propre copie de l'utilisateur.
		final ConnectionUserBean clonedUser = user.clone();
		// clonedUser est une donnée volatile qui n'est pas conservée (pour éviter l'injection de données dans le cache).
		clonedUser.setPassword(new String(secret));
		clonedUser.setLogin(identifier);
		credential.update(clonedUser);
		// Store the user in the request.
		request.getAttributes().put(ConnectionUserBean.CONNECTED_USER, clonedUser);
		request.getAttributes().put(ConnectionUserBean.CONNECTED_UNIQUEID, credential.getUniqueId());
		request.getAttributes().put(IConnectionCredential.CONNECTED_CREDENTIAL, credential);
		// 5. On teste finalement la validité du credential.
		if (!trusted) {
			try {
				if (!credential.authenticate(clonedUser, identifier, secret)) {
					if (clonedUser.isLocked()) {
						// Attention cette information peut favoriser une attaque DDOS.
						response.setStatus(Status.CLIENT_ERROR_LOCKED, translateMessage(request, "locked")); //$NON-NLS-1$
						broadcastLocked(clonedUser);
					} else {
						response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "wrong")); //$NON-NLS-1$
						broadcastFail(request);
					}
		            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
		            waitConnectionTiming(timing);
		            return false;
				}
				// On doit s'assurer de ne cacher que des user et des credential authentifiés (attaque DOS).
				// Thread safe ?
				user.addCredential(credential);
				activator.cachePut(user);
			} catch (Throwable e) {
				long t = System.currentTimeMillis();
				if ((t - lastErrorCache) > LASTERROR_LATENCY) {
					activator.error(e.getLocalizedMessage(), e);
					lastErrorCache = t;
				} else {
					activator.debug(e.getLocalizedMessage(), e);
				}
				response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "misc")); //$NON-NLS-1$
	            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
	            waitConnectionTiming(timing);
	            return false;
			}
		}
		// Teste si l'utilisateur possède des droits, sans aucun droits on considère, peut être à tord (!!), 
		// qu'il n'a pas à se connecter aux web-services.
		if (clonedUser.getProfile() == null) {
			response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "noprofile")); //$NON-NLS-1$
            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
            waitConnectionTiming(timing);
            return false;
		}
		// Teste si l'utilisateur n'a pas été verrouillé depuis sa dernière connection.
		if (clonedUser.isLocked()) {
			response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "locked")); //$NON-NLS-1$
            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
			broadcastLocked(clonedUser);
            waitConnectionTiming(timing);
            return false;
		}
		// Teste si le mot de passe l'a pas expiré.
		if (clonedUser.isChangePWD()) {
			String seg = request.getResourceRef().getLastSegment();
			if ((seg == null) || !seg.equalsIgnoreCase("currentuser")) { //$NON-NLS-1$
				response.setStatus(Status.CLIENT_ERROR_FORBIDDEN, translateMessage(request, "outofdate")); //$NON-NLS-1$
	            response.getChallengeRequests().addAll(getAcceptedChanllenges(request, services));
				// TODO renvoyer plutot une redirection !
				broadcastLocked(clonedUser);
	            waitConnectionTiming(timing);
	            return false;
			}
		}
		// Check if the application accept this connection.
		// TODO use the connection cache to avoid recall...
		try {
			activator.checkUserConnection(clonedUser);
		} catch (ResourceException e) {
			response.setStatus(e.getStatus());
			broadcastFail(request);
			waitConnectionTiming(timing);
			return false;
		}
		broadcastSuccess(clonedUser);
		updateConnectionTiming(timing);
		return true;
	}

	private List<ChallengeRequest> getAcceptedChanllenges(Request request, Object[] services) {
		ArrayList<ChallengeRequest> result = new ArrayList<ChallengeRequest>();
        boolean isSomeBasic = false;
        for(Object service:services) {
        	if (service instanceof IWWWAuthentificationService) {
        		ChallengeRequest cr = ((IWWWAuthentificationService) service).getChallengeRequest(request, activator.getRealm());
        		if (cr != null) {
        			result.add(cr);
        		}
        	} else {
        		isSomeBasic = true;
        	}
        }
        if (isSomeBasic) {
        	result.add(new ChallengeRequest(ChallengeScheme.HTTP_BASIC, activator.getRealm()));
        }
        return result;
	}

	/*
	 * Translate error message or not, according to client request
	 * 
	 * <p>
	 * Default behavior is to respect HTTP Header and, then, translate user messages.
	 * 
	 * @param request
	 * @param msg
	 * @param language
	 * @return
	 */
	private String translateMessage(final Request request, final String msg) {
		Form f = request.getResourceRef().getQueryAsForm();
		if (f != null) {
			String value = f.getFirstValue(HTTPPARAMETER_CLIENT_TRANSLATION, true);
			if ((value != null) && Boolean.parseBoolean(value)) {
				// On renvois le code message c'est le client qui se chargera de la traduction.
				return msg;
			}
		}
		// Par défaut on traduit le message (pour respecter le protocole HTTP).
		return activator.getMessage(msg, BaseResource.getClientPreferedLanguage(request));
	}

	/**
	 * This delay avoid the detection of non registered login, which may be rejeted faster than 
	 * reals login with worng passwords.
	 * @param timing
	 */
	private void waitConnectionTiming(long timing) {
		long t = lastConnectionTiming;
		// average timing +/- 6% 
		t = t + (Math.round(t * (Math.random() - 0.5)) >> 3);
		timing = System.currentTimeMillis() - timing;
		if (timing < t) {
			try {
				Thread.sleep(t - timing);
			} catch (InterruptedException e) {
				// Nothing to do here... the connection process is terminated anyway.
			}
		}
	}

	/**
	 * This delay avoid the detection of non registered login, which may be rejeted faster than 
	 * reals login with worng passwords.
	 * @param timing
	 */
	private void updateConnectionTiming(final long timing) {
		lastConnectionTiming = (lastConnectionTiming + System.currentTimeMillis() - timing) >> 1;
	}

	private void broadcastSuccess(IConnectionUserBean user) {
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		props.put("uid", user.getId()); //$NON-NLS-1$
		if (user.getLogin() != null) {
			props.put("login", user.getLogin()); //$NON-NLS-1$
		}
		props.put("code", "rest_auth"); //$NON-NLS-1$ //$NON-NLS-2$
		props.put("message", "Succesful authentication on REST service call."); //$NON-NLS-1$
		props.put("date", new Date()); //$NON-NLS-1$
		postEvent(props);
	}

	private void broadcastLocked(IConnectionUserBean user) {
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		props.put("uid", user.getId()); //$NON-NLS-1$
		if (user.getLogin() != null) {
			props.put("login", user.getLogin()); //$NON-NLS-1$
		}
		props.put("code", "rest_auth"); //$NON-NLS-1$ //$NON-NLS-2$
		props.put("message", "Locked authentication attempt on REST service call."); //$NON-NLS-1$
		props.put("date", new Date()); //$NON-NLS-1$
		postEvent(props);
	}

	private void broadcastFail(Request request) {
		String login = null;
		if (request.getChallengeResponse() != null) {
			login = request.getChallengeResponse().getIdentifier();
		}
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		if (login != null) {
			props.put("login", login); //$NON-NLS-1$
		}
		props.put("code", "rest_auth"); //$NON-NLS-1$ //$NON-NLS-2$
		props.put("message", "Incorrect authentication on REST service call."); //$NON-NLS-1$
		props.put("date", new Date()); //$NON-NLS-1$
		postEvent(props);
	}

	private void postEvent(Hashtable<String, Object> props) {
		EventAdmin ad = activator.getService(EventAdmin.class);
		if (ad != null) {
			ad.postEvent(new Event("com/arcadsoftware/user/action", (Dictionary<String,Object>) props)); //$NON-NLS-1$
		}
	}

}
