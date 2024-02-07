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
package com.arcadsoftware.restful.internal;

import java.util.ArrayList;
import java.util.List;

import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Preference;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.resource.ResourceException;
import org.restlet.service.StatusService;

import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.XStreamCompact;

/**
 * This Status service implementation provide an XML or HTML or JSON representation of the problem when needed.
 */
public class ServicesStatus extends StatusService {

	private static final List<MediaType> mediatypes = new ArrayList<MediaType>();
	private static final XStreamCompact XS = new XStreamCompact(ServicesStatus.class.getClassLoader());

	static {
		mediatypes.add(MediaType.TEXT_HTML);
		mediatypes.add(MediaType.APPLICATION_XML);
		mediatypes.add(MediaType.APPLICATION_XHTML);
		mediatypes.add(MediaType.APPLICATION_JSON);
		mediatypes.add(MediaType.TEXT_ALL);
		XS.alias("message", Status.class); //$NON-NLS-1$
		XS.registerConverter(new StatusConvertor());
	}

	private final Activator activator;

	public ServicesStatus(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public Representation toRepresentation(Status status, Request request, Response response) {
		MediaType mediaType = request.getClientInfo().getPreferredMediaType(mediatypes);
		if (mediaType != null) {
			if (MediaType.APPLICATION_XHTML.equals(mediaType, true) || MediaType.TEXT_HTML.equals(mediaType, true)) {
				StringBuilder sb = new StringBuilder(
						"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><link href=\"/css/service/status.css\" rel=\"stylesheet\" /><title>Error "); //$NON-NLS-1$
				sb.append(status.getCode());
				sb.append("</title></head><body><h2>"); //$NON-NLS-1$
				sb.append(status.getReasonPhrase());
				sb.append(" (<strong>"); //$NON-NLS-1$
				sb.append(status.getCode());
				sb.append("</strong>)</h2><p>"); //$NON-NLS-1$
				sb.append(status.getDescription());
				sb.append("</p><p>Details: <i><a href=\""); //$NON-NLS-1$
				sb.append(status.getUri());
				sb.append("\">"); //$NON-NLS-1$
				sb.append(status.getUri());
				sb.append("</a></i></p><p><strong>Original Reference</strong>: <a href=\""); //$NON-NLS-1$
				sb.append(request.getOriginalRef());
				sb.append("\">"); //$NON-NLS-1$
				sb.append(request.getOriginalRef());
				sb.append("</a></p><p><strong>Resource Reference</strong>: <a href=\""); //$NON-NLS-1$
				sb.append(request.getResourceRef());
				sb.append("\">"); //$NON-NLS-1$
				sb.append(request.getResourceRef());
				sb.append("</a></p><p><strong>Method</strong>: "); //$NON-NLS-1$
				sb.append(request.getMethod());
				if (request.getClientInfo() != null) {
					sb.append("</p><p><strong>Media Types</strong>: "); //$NON-NLS-1$
					sb.append(request.getClientInfo().getAcceptedMediaTypes());
					sb.append("</p><p><strong>Character Sets</strong>: "); //$NON-NLS-1$
					sb.append(request.getClientInfo().getAcceptedCharacterSets());
					sb.append("</p><p><strong>Languages</strong>: "); //$NON-NLS-1$
					sb.append(request.getClientInfo().getAcceptedLanguages());
					sb.append("</p><p><strong>Encodings</strong>: "); //$NON-NLS-1$
					sb.append(request.getClientInfo().getAcceptedEncodings());
				}
				sb.append("</p><p>&nbsp;</p>"); //$NON-NLS-1$
				sb.append("</body></html>"); //$NON-NLS-1$
				return new StringRepresentation(sb, MediaType.TEXT_HTML, Language.ENGLISH_US, CharacterSet.UTF_8);
			}
			if (MediaType.APPLICATION_JSON.equals(mediaType, true)
					|| hasJSON(request.getClientInfo().getAcceptedMediaTypes())) {
				JSONObject o = new JSONObject();
				try {
					o.put("name", status.getReasonPhrase()); //$NON-NLS-1$
					o.put("code", status.getCode()); //$NON-NLS-1$
					if (status.getUri() != null) {
						o.put("href", status.getUri()); //$NON-NLS-1$
					}
					if (status.getDescription() != null) {
						o.put("description", status.getDescription());
					}
					return new StringRepresentation(o.toString(), MediaType.APPLICATION_JSON, Language.ENGLISH_US,
							CharacterSet.UTF_8);
				} catch (JSONException e) {
					activator.error("Unable to generate the JSON representation of the error message: " + status.toString(), e);
				}
			}
			if (MediaType.APPLICATION_XML.equals(mediaType, true)) {
				return new XMLRepresentation(XS.toXML(status), mediaType);
			}
		}
		return super.toRepresentation(status, request, response);
	}

	private boolean hasJSON(List<Preference<MediaType>> acceptedMediaTypes) {
		// May go through an unqualified response, as long as JSON was acceptable, we return a JSON response...
		for (Preference<MediaType> mtp : acceptedMediaTypes) {
			if (MediaType.APPLICATION_JSON.equals(mtp.getMetadata(), true)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public Status toStatus(Throwable throwable, Request request, Response response) {
		if (activator.isRestletLogDisabled() && !(throwable instanceof ResourceException)) {
			activator.info("Unexpected Exception during Web-Service call.", throwable);
		}
		return super.toStatus(throwable, request, response);
	}

}