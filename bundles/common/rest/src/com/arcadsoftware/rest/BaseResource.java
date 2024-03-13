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
package com.arcadsoftware.rest;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.restlet.Request;
import org.restlet.data.ClientInfo;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Metadata;
import org.restlet.data.Method;
import org.restlet.data.Parameter;
import org.restlet.data.Preference;
import org.restlet.data.Status;
import org.restlet.ext.fileupload.RestletFileUpload;
import org.restlet.representation.EmptyRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;
import org.restlet.resource.ServerResource;

/**
 * The BaseResource class define a Restlet Resource that can be used into any "servlet"-like
 * development (by implementation of method post/get/put/delete according to the HTTP accepted methods.
 * 
 * <p>
 * This Resource is a Language aware Resource, it provide a method <code>getClientLanguage</code> 
 * that can used to choose the best language compromise according to the desired client language set and the
 * available language provided by the server.
 * 
 * <p>
 * Descendant of this class can not use java annotations to declare Restlet methods.
 */
public abstract class BaseResource extends ServerResource {

	/**
	 * This request attribute indicate that the real Request Method is not the HTTP method that should be proceed
	 * by the resource. 
	 * 
	 * <p>
	 * The getMethod() return the correct proceeded HTTP Method for this Resource call.
	 */
	public static final String METHODPROXIED = "com.arcadsoftware.method.proxy"; //$NON-NLS-1$

	/**
	 * The value of this request query parameter indicate that the real Request Method is not the HTTP method that should be proceed
	 * by the resource. 
	 * 
	 * <p>
	 * The getMethod() return the correct proceed HTTP Method for this Resource call.
	 */
	public static final String QUERY_METHODPROXIED = "http_method_proxy"; //$NON-NLS-1$

	/**
	 * This is the basics MediaTypes list supported into Arcad Applications.
	 * 
	 * <p>
	 * This list contain two media types :
	 * <ul>
	 * <li>application/xml
	 * <li>application/json
	 * </ul>
	 */
	protected static final MediaType[] MEDIATYPES_BASE_XMLJSON = new MediaType[] {
			MediaType.APPLICATION_JSON,
			MediaType.APPLICATION_XML
	};

	/**
	 * This list contain 3 media types :
	 * <ul>
	 * <li>application/xml
	 * <li>application/json
	 * <li>application/x-xsd+xml
	 * </ul>
	 */
	protected static final MediaType[] MEDIATYPES_BASE_XMLJSONXSD = new MediaType[] {
			MediaType.APPLICATION_JSON,
			MediaType.APPLICATION_XML,
			MediaType.APPLICATION_W3C_SCHEMA
	};


	/**
	 * This list contain 5 media types (this list should be your privileged 
	 * choice if you do not plan to offer an HTML representation) :
	 * <ul>
	 * <li>text/xml
	 * <li>application/xml
	 * <li>application/json
	 * <li>application/x-xsd+xml
	 * </ul>
	 */
	protected static final MediaType[] MEDIATYPES_BASE_XMLJSONXSD2 = new MediaType[] {
			MediaType.TEXT_XML,
			MediaType.APPLICATION_JSON,
			MediaType.APPLICATION_XML,
			MediaType.APPLICATION_W3C_SCHEMA
	};

	/**
	 * This is the list of basic supported media types.
	 * 
	 * <p>
	 * This list contains :
	 * <ul>
	 * <li>text/xml
	 * <li>text/html
	 * <li>application/xml
	 * <li>application/json
	 * <li>application/x-xsd+xml
	 * <li>application/xhtml+xml
	 * <li>application/xml-dtd
	 * </ul>
	 */
	protected static final MediaType[] MEDIATYPES_BASE = new MediaType[] {
			MediaType.TEXT_XML,
			MediaType.APPLICATION_JSON,
			MediaType.APPLICATION_XML,
			MediaType.APPLICATION_W3C_SCHEMA,
			MediaType.APPLICATION_XHTML,
			MediaType.APPLICATION_XML_DTD,
			MediaType.TEXT_HTML
	};

	/**
	 * Add optional but frequently used MediaTypes to the list <b>MEDIATYPES_BASE_XMLJSON</b>.
	 * 
	 * <p>
	 * This list contains :
	 * <ul>
	 * <li>text/xml
	 * <li>text/html
	 * <li>application/x-xsd+xml
	 * <li>application/xhtml+xml
	 * <li>application/xml-dtd
	 * </ul>
	 */
	protected static final MediaType[] MEDIATYPES_BASESUP = new MediaType[] {
			MediaType.TEXT_XML,
			MediaType.APPLICATION_W3C_SCHEMA,
			MediaType.APPLICATION_XHTML,
			MediaType.APPLICATION_XML_DTD,
			MediaType.TEXT_HTML
	};

	private Method method;
	private Form requestForm;
	private List<Variant> variants;

	/**
	 * Return the request Attribute, the default value is returned if it does not exist.
	 * 
	 * @param key
	 * @param defaultValue
	 * @return
	 */
	protected final int getAttribute(String key, int defaultValue) {
		try {
			return Integer.parseInt(getRequest().getAttributes().get(key).toString());
		} catch (Throwable e) {
			return defaultValue;
		}
	}

	/**
     * Returns a modifiable list of exposed variants for the current request
     * method. You can declare variants manually by updating the result list,
     * by overriding this method. By default, the variants provided are XML
     * and JSON.
	 * 
	 * @return The modifiable list of variants.
	 */
	@Override
	public final List<Variant> getVariants() {
		return super.getVariants();
	}

	/**
     * Returns a modifiable list of exposed variants for the given method. You
     * can declare variants manually by updating the result list , by overriding
     * this method. By default, the variants provided are XML and JSON.
	 * 
	 * @param method
	 *            The method.
	 * @return The modifiable list of variants.
	 */
	@Override
	protected final List<Variant> getVariants(Method method) {
		if (variants == null) {
			variants = createVariants(MEDIATYPES_BASE_XMLJSON);
		}
		ArrayList<Variant> result = new ArrayList<Variant>(variants.size());
		result.addAll(variants);
		return result;
	}

	/**
	 * Appends some variants to the default accepted variants (XML and JSON).
	 * 
	 * @param variants
	 */
	protected final void setVariants(Variant... variants) {
		this.variants = new ArrayList<Variant>(variants.length);
		Collections.addAll(this.variants, variants);
	}

	/**
	 * Define the default accepted variants.
	 * 
	 * @param mediaTypes
	 */
	protected final void setVariants(MediaType... mediaTypes) {
		this.variants = createVariants(mediaTypes);
	}

	/**
	 * Appends some Mediatypes to the default accepted variants.
	 * 
	 * @param mediaTypes
	 */
	protected final void addVariants(MediaType... mediaTypes) {
		if (variants == null) {
			setVariants(mediaTypes);
		} else {
			for (MediaType mt : mediaTypes) {
				variants.add(new Variant(mt));
			}
		}
	}

	/**
	 * Set the default accepted variants.
	 * 
	 * @param mediaTypes
	 * @param languages
	 */
	protected final void setVariants(MediaType[] mediaTypes, Language[] languages) {
		this.variants = createVariants(mediaTypes, languages);
	}

	/**
	 * 
	 * @param mediaTypes
	 * @param languages
	 */
	protected final void addVariants(MediaType[] mediaTypes, Language[] languages) {
		if (variants == null) {
			setVariants(mediaTypes, languages);
		} else {
			for (MediaType mt : mediaTypes) {
				for (Language l : languages) {
					variants.add(new Variant(mt, l));
				}
			}
		}
	}

	/**
	 * Return a list of Variants constructed from a set of MediaTypes.
	 * You can use this method to build the resource provided Variant list.
	 * 
	 * @param mediaTypes
	 * @return
	 * @see #getVariants()
	 */
	public static final List<Variant> createVariants(MediaType... mediaTypes) {
		ArrayList<Variant> result = new ArrayList<Variant>();
		for (MediaType mt : mediaTypes) {
			result.add(new Variant(mt));
		}
		return result;
	}

	/**
	 * Return a list of Variants constructed from a set of MediaTypes.
	 * You can use this method to build the resource provided Variant list.
	 * 
	 * @param mediaTypes
	 * @param languages
	 * @return
	 * @see #getVariants()
	 */
	public static final List<Variant> createVariants(MediaType[] mediaTypes, Language[] languages) {
		ArrayList<Variant> result = new ArrayList<Variant>();
		for (MediaType mt : mediaTypes) {
			for (Language l : languages) {
				result.add(new Variant(mt, l));
			}
		}
		return result;
	}

	/**
	 * Returns the method.
	 * 
	 * @return The method.
	 * @see Request#getMethod()
	 */
	@Override
	public Method getMethod() {
		if (method != null) {
			return method;
		}
		// Value not initialized...
		Request request = getRequest();
		if (request == null) {
			return null;
		}
		// Use an attribute...
		Object o = request.getAttributes().get(METHODPROXIED);
		if (o instanceof Method) {
			method = (Method) o;
			return method;
		}
		method = request.getMethod();
		if (Method.GET.equals(method) || Method.POST.equals(method)) {
			Parameter p = request.getResourceRef().getQueryAsForm().getFirst(QUERY_METHODPROXIED);
			if (p != null) {
				String v = p.getValue();
				if (v != null) {
					v = v.toUpperCase();
					if (Method.GET.getName().equals(v)) {
						method = Method.GET;
					} else if (Method.PUT.getName().equals(v)) {
						method = Method.PUT;
					} else if (Method.DELETE.getName().equals(v)) {
						method = Method.DELETE;
					} else if (Method.OPTIONS.getName().equals(v)) {
						method = Method.OPTIONS;
					} else if (Method.POST.getName().equals(v)) {
						method = Method.POST;
					}
				}
			}
		}
		return method;
	}

	/**
	 * Force the Resource proceeded HTTP Method to the given value.
	 * 
	 * @param method
	 */
	public void setMethod(Method method) {
		this.method = method;
	}

	/**
     * Returns the preferred variant among the list of the resource supported variants. The
     * selection is based on the client preferences using the
     * {@link ClientInfo#getPreferredVariant} method.
	 * 
	 * @return The preferred variant.
	 */
	protected Variant getPreferredVariant() {
		return getPreferredVariant(getVariants());
	}

	/**
	 * Return the Client best scored language into the given request.
	 * 
	 * @param request the Restlet request
	 * @return the client prefered language, US english (en-us) if none.
	 */
	public final static Language getClientPreferedLanguage(Request request) {
		Language result = Language.ENGLISH_US;
		if ((request == null) || (request.getClientInfo() == null) || (request.getClientInfo().getAcceptedLanguages() == null)) {
			return result;
		}
		float quality = Float.MIN_VALUE;
		for (Preference<Language> pref : request.getClientInfo().getAcceptedLanguages()) {
			if ((pref != null) && (pref.getQuality() > quality)) {
				quality = pref.getQuality();
				result = pref.getMetadata();
			}
		}
		return result;
	}

	/**
	 * @param variant
	 * @return true if the Variant has an XML MediaType
	 */
	protected final static boolean isXML(Variant variant) {
		return MediaType.APPLICATION_XML.equals(variant.getMediaType()) || MediaType.TEXT_XML.equals(variant.getMediaType());
	}

	/**
	 * @param variant
	 * @return true if the Variant has a JSON MediaType
	 */
	protected final static boolean isJSON(Variant variant) {
		return MediaType.APPLICATION_JSON.equals(variant.getMediaType());
	}

	/**
	 * @param variant
	 * @return true if the Variant has a HTML MediaType
	 */
	protected final static boolean isHTML(Variant variant) {
		return MediaType.APPLICATION_XHTML.equals(variant.getMediaType()) || MediaType.TEXT_HTML.equals(variant.getMediaType());
	}

	/**
	 * @param variant
	 * @return true if the Variant has an XSD MediaType
	 */
	protected final static boolean isXSD(Variant variant) {
		return MediaType.APPLICATION_W3C_SCHEMA.equals(variant.getMediaType());
	}

	/**
	 * @param variant
	 * @return true if the Variant has an YAML MediaType
	 */
	protected final static boolean isYAML(Variant variant) {
		return MediaType.APPLICATION_YAML.equals(variant.getMediaType()) || MediaType.TEXT_YAML.equals(variant.getMediaType());
	}

	/**
	 * Given a list of Language return the client most preferred one.
	 * 
	 * By default return the first language from the given list. 
	 * 
	 * @param availableLanguages the list of languages available on the server.
	 * @return the best choice, never return null, if none language is available return English(US).
	 */
	public final Language getClientLanguage(List<Language> availableLanguages) {
		Language result = Language.ENGLISH_US;
		float quality = Float.MIN_VALUE;
		try {
			if (availableLanguages.size() > 0) {
				result = availableLanguages.get(0);
			}
			for (Preference<Language> pref : getRequest().getClientInfo().getAcceptedLanguages()) {
				if ((availableLanguages.indexOf(pref.getMetadata()) > -1) &&
						(pref.getQuality() > quality)) {
					quality = pref.getQuality();
					result = pref.getMetadata();
				}
			}
		} catch (NullPointerException e) {/*Too many null test to track*/}
		return result;
	}

	/**
	 * Return the client preferred Language from its Request.
	 * @return
	 */
	protected final Language getClientPreferedLanguage() {
		return getClientPreferedLanguage(getRequest());
	}

	/**
	 * Utility method to transform a Language to a Locale.
	 * 
	 * @param language
	 * @return
	 * @see MultiLanguageMessages
	 */
	protected final Locale toLocale(Language language) {
		return MultiLanguageMessages.toLocale(language);
	}

	/**
	 * Utility method to transform a Localt into a Language.
	 * 
	 * @param locale
	 * @return
	 * @see MultiLanguageMessages
	 */
	protected final Language toLanguage(Locale locale) {
		return MultiLanguageMessages.toLanguage(locale);
	}
	/**
	 * Return the client preferred Locale.
	 * @return
	 */
	protected final Locale getClientPreferedLocale() {
		return toLocale(getClientPreferedLanguage());
	}

	/**
	 * Return the prefered Language from this Variant.
	 * @param variant
	 * @return
	 */
	protected final Language getLanguage(Variant variant) {
		if ((variant != null) && (variant.getLanguages() != null) && (variant.getLanguages().size() > 0)) {
			return variant.getLanguages().get(0);
		}
		return Language.ENGLISH_US;
	}

	/**
	 * Get a language code (2 char length string) corresponding to the given Language object.
	 * @param language
	 * @return A String code ("en" is the default code).
	 */
	protected final String getLanguageCode(Language language) {
		if ((language == null) || Language.ALL.equals(language) || (language.getName() == null) || (language.getName().length() < 2)) {
			return "en"; //$NON-NLS-1$
		} else {
			return language.getName().substring(0, 2).toLowerCase();
		}
	}

	/**
	 * Return true if the client request contain (*.* / 1.0) as MediaType preference.
	 * @return
	 */
	protected final boolean isClientAcceptAnyMediaType() {
		ClientInfo ci = getClientInfo();
		if ((ci == null) || (ci.getAcceptedMediaTypes() == null)) {
			return false;
		}
		if (ci.getAcceptedMediaTypes().isEmpty()) {
			return true;
		}
		for (Preference<MediaType> p : ci.getAcceptedMediaTypes()) {
			if (MediaType.ALL.equals(p.getMetadata())) {
				return p.getQuality() >= 1.0F;
			}
		}
		return false;
	}

	/**
	 * Get a parameters Form from the Request.
	 * <p>
	 * The Form is generated from the entity or the Query, if there is no entity attached to the request. 
	 * The entity is read after a call to this method.
	 * 
	 * <p>
	 * This method can be called several times, the entity is read just one time.
	 * 
	 * <p>
	 * This method is compatible with the "_empty_" entity trick used to send non empty entities into HTTP request.
	 * 
	 * @return Never return null.
	 */
	protected Form getRequestForm() {
		if (requestForm != null) {
			return requestForm;
		}
		requestForm = new Form();
		if (getRequest() != null) {
			Representation entity = getRequest().getEntity();
			if ((entity != null) && entity.isAvailable() && !(entity instanceof EmptyRepresentation) && (entity.getMediaType() != null)) {
				// Header possibly added by proxies when the body of a POST/PUT request is empty, uses the URL params instead
				if ("none".equals(requestForm.getFirstValue("_empty_"))) { //$NON-NLS-1$ //$NON-NLS-2$
					requestForm = getRequest().getResourceRef().getQueryAsForm();
				}
				// Build the form from the body of the request (entity)
				if (entity.getMediaType().equals(MediaType.APPLICATION_JSON, true)) {
					// JSON Content, parse body to instantiate the Form
					try {
						// Usage of getText could be an issue for large bodies, can throw OutOfMemory error if the body is too large and cannot be processed
						// Do we manage what is sent ? Do we need to find an alternative of is this sufficient ?
						JSONObject o = new JSONObject(entity.getText());
						for (Object e : o.toMap().entrySet()) {
							if (e instanceof Entry) {
								@SuppressWarnings("rawtypes")
								Entry en = (Entry) e;
								// For Arrays, we instantiate Parameters with the same key and all the values present in the array
								// { "list": ["element1","element2"] } will be transformed into { "list":"element1", "list":"element2" }
								if (en.getValue() instanceof JSONArray) {
									JSONArray a = (JSONArray) en.getValue();
									for (int i = 0; i < a.length(); i++) {
										requestForm.add(requestForm.createEntry(en.getKey().toString(), a.get(i).toString()));
									}
								} else {
									// Every other type of value we 
									requestForm.add(requestForm.createEntry(en.getKey().toString(), en.getValue().toString()));
								}
							}
						}
					} catch (IOException | JSONException e) {
						throw new ResourceException(Status.CLIENT_ERROR_UNSUPPORTED_MEDIA_TYPE, e.getLocalizedMessage());
					}
				} else if (entity.getMediaType().equals(MediaType.APPLICATION_WWW_FORM, true)) {
					// Form-URL-Encoded format for HTML forms
					requestForm = new Form(entity);
				} else if (entity.getMediaType().equals(MediaType.MULTIPART_FORM_DATA, true)) {
					// Form-Data format for HTML forms.
					requestForm = new Form();
					final String encoding;
					if (entity.getCharacterSet() != null) {
						encoding = entity.getCharacterSet().getName();
					} else {
						encoding = "UTF-8"; //$NON-NLS-1$
					}
					DiskFileItemFactory factory = new DiskFileItemFactory();
					factory.setSizeThreshold(1000240);
					factory.setRepository(new File("_tempdir")); //$NON-NLS-1$
					RestletFileUpload upload = new RestletFileUpload(factory);
					try {
						for (FileItem item : upload.parseRequest(getRequest())) {
							if (item.isFormField()) {
								try {
									requestForm.add(item.getFieldName(), item.getString(encoding));
								} catch (UnsupportedEncodingException e) {
									throw new ResourceException(Status.CLIENT_ERROR_UNSUPPORTED_MEDIA_TYPE, e.getLocalizedMessage());
								}
							} else {
								requestForm.add(new FileItemParameter(item));
							}
						}
					} catch (FileUploadException e) {
						throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, e.getLocalizedMessage());
					}
				} else if (entity.getMediaType().equals(MediaType.APPLICATION_XML, true)) {
					// If the content is XML, this code is for backward compatibility
					requestForm = new Form(entity);
				} else if (entity.getMediaType().equals(MediaType.APPLICATION_OCTET_STREAM, true)) {
					// How to handle that ? Currently loads as a string the content of the file that is uploaded
					// If file in body, params should be in url
					requestForm = getRequest().getResourceRef().getQueryAsForm();
				} else {
					throw new ResourceException(Status.CLIENT_ERROR_UNSUPPORTED_MEDIA_TYPE);
				}
			} else {
				// The body is empty and the form content is in the URL
				requestForm = getRequest().getResourceRef().getQueryAsForm();
			}
		}
		return requestForm;
	}

	/**
	 * This method return true if the given preferences contains the given Metadata at least at the specified quality.
	 * 
	 * @param <T> The metadata type.
	 * @param preferences The list of preferences to check.
	 * @param metadata The Metadate to test
	 * @param quality The needed level of quality
	 * @return True if metadata is found at least at the quality level.
	 */
	protected final <T extends Metadata> boolean isPrefered(List<Preference<T>> preferences, T metadata, float quality) {
		if (metadata == null) {
			return false;
		}
		for (Preference<T> p : preferences) {
			if (metadata.equals(p.getMetadata()) && (p.getQuality() >= quality)) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected final Representation options(Variant variant) throws ResourceException {
		// Cette implémentation n'a pas d'utilité à être surchargé par les classes filles.
		// Si cela devient nécessaire supprimez cette surcharge finale.
		return super.options(variant);
	}

	@Override
	public final Representation options() {
		// Cette implémentation n'a pas d'utilité à être surchargé par les classes filles.
		// Si cela devient nécessaire supprimez cette surcharge finale.
		return super.options();
	}

	@Override
	public final void updateAllowedMethods() {
		// Do not update accepted method this useless !!!!
	}
}