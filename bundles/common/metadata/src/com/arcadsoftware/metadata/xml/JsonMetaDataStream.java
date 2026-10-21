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
package com.arcadsoftware.metadata.xml;

import java.text.ParseException;
import java.time.Instant;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Map.Entry;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.UpdateMetaDataEntity;
import com.arcadsoftware.osgi.ISODateFormater;

/**
 * JSon Streamer specialized to the serialization of MetaData objects.
 * 
 * <p>
 * This class can be used to serialize BeanMaps too.
 * 
 * <p>
 * <b>This serialized can not transform JSON into Entity classe.</b>
 * 
 * @author ARCAD Software
 */
public class JsonMetaDataStream extends JsonCriteriaStream {

	public static final String TAG_UPDATEENTITY = "update-entity"; //$NON-NLS-1$

	public JsonMetaDataStream() {
		super();
	}

	public String writeEntity(final MetaDataEntity entity, final boolean documentation) {
		if (entity == null) {
			return null;
		}
		try {
			return convert(entity, documentation).toString();
		} catch (JSONException e) {
			return null;
		}
	}
	
	private JSONObject convert(MetaDataEntity entity, boolean documentation) throws JSONException {
		JSONObject result = new JSONObject();
		if (entity instanceof UpdateMetaDataEntity) {
			result.put(TAG_UPDATEENTITY, true);
		}
		if (entity.getDomain() != null) {
			result.put("domain", entity.getDomain()); //$NON-NLS-1$
		}
		if (entity.getType() != null) {
			result.put("type", entity.getType()); //$NON-NLS-1$
		}
		if (entity.getDate() != null) {
			result.put("date", ISODateFormater.toString(entity.getDate())); //$NON-NLS-1$
		}
		if (entity.getVersion() > 0) {
			result.put("version", entity.getVersion()); //$NON-NLS-1$
		}
		if (documentation) {
			if (entity.getName() != null) {
				result.put("name", entity.getName()); //$NON-NLS-1$
			}
			if (entity.getDescription() != null) {
				result.put("description", entity.getDescription()); //$NON-NLS-1$
			}
		}
		if (entity.isReadOnly()) {
			result.put("readonly", true); //$NON-NLS-1$
		}
		if (entity.isLockable()) {
			result.put("lockable", true); //$NON-NLS-1$
		}
		if (entity.getMetadata() != null) {
			result.put("metadata", writeMetadata(entity.getMetadata())); //$NON-NLS-1$
		}
		JSONObject rights = new JSONObject();
		if (entity.getCreate() != null) {
			rights.put("create", convert(entity.getCreate())); //$NON-NLS-1$
		}
		if (entity.getUpdate() != null) {
			rights.put("update", convert(entity.getUpdate())); //$NON-NLS-1$
		}
		if (entity.getDelete() != null) {
			rights.put("delete", convert(entity.getDelete())); //$NON-NLS-1$
		}
		if (entity.getList() != null) {
			rights.put("list", convert(entity.getList())); //$NON-NLS-1$
		}
		if (entity.getRead() != null) {
			rights.put("read", convert(entity.getRead())); //$NON-NLS-1$
		}
		if (rights.length() > 0) {
			result.put("rights", rights); //$NON-NLS-1$
		}
		JSONArray attributes = new JSONArray();
		for (MetaDataAttribute att: entity.getAttributes().values()) {
			JSONObject a = new JSONObject();
			fillElement(a, att, documentation);
			if (att.getLength() != 0) {
				a.put("length", att.getLength()); //$NON-NLS-1$
			}
			if (att.getPrecision() != 0) {
				a.put("precision", att.getPrecision()); //$NON-NLS-1$
			}
			if (att.isListable()) {
				a.put("listable", true); //$NON-NLS-1$
			}
			if (att.isMandatory()) {
				a.put("mandatory", true); //$NON-NLS-1$
			}
			rights = new JSONObject();
			if (att.getRightUpdate(false) != null) {
				rights.put("update", convert(att.getRightUpdate(false))); //$NON-NLS-1$
			}
			if (att.getRightRead(false) != null) {
				rights.put("read", convert(att.getRightRead(false))); //$NON-NLS-1$
			}
			if (rights.length() > 0) {
				a.put("rights", rights); //$NON-NLS-1$
			}
			attributes.put(a);
		}
		if (attributes.length() > 0) {
			result.put("attibutes", attributes); //$NON-NLS-1$
		}
		JSONArray tests = new JSONArray();
		for (MetaDataTest test: entity.getTests().values()) {
			JSONObject t = new JSONObject();
			if (test.getCode() != null) {
				t.put("code", test.getCode()); //$NON-NLS-1$
			}
			if (test.getEvent() != null) {
				t.put("event", test.getEvent()); //$NON-NLS-1$
			}
			if (test.getTest() != null) {
				t.put("test", test.getTest()); //$NON-NLS-1$
			}
			tests.put(t);
		}
		if (tests.length() > 0) {
			result.put("tests", tests); //$NON-NLS-1$
		}
		JSONArray links = new JSONArray();
		for (MetaDataLink ln: entity.getLinks().values()) {
			JSONObject l = new JSONObject();
			fillElement(l, ln, documentation);
			rights = new JSONObject();
			if (ln.getRightCreate(false) != null) {
				rights.put("create", convert(ln.getRightCreate(false))); //$NON-NLS-1$
			}
			if (ln.getRightList(false) != null) {
				rights.put("list", convert(ln.getRightList(false))); //$NON-NLS-1$
			}
			if (rights.length() > 0) {
				l.put("rights", rights); //$NON-NLS-1$
			}
			links.put(l);
		}
		if (links.length() > 0) {
			result.put("links", links); //$NON-NLS-1$
		}
		return result;
	}

	private void fillElement(JSONObject j, Element e, boolean documentation) throws JSONException {
		if (e.getCode() != null) {
			j.put("code", e.getCode()); //$NON-NLS-1$
		}
		if (e.getType() != null) {
			j.put("type", e.getType()); //$NON-NLS-1$
		}
		if (documentation) {
			if (e.getName() != null) {
				j.put("name", e.getName()); //$NON-NLS-1$
			}
			if (e.getDescription() != null) {
				j.put("description", e.getDescription()); //$NON-NLS-1$
			}
		}
		if (e.isReadonly()) {
			j.put("readonly", true); //$NON-NLS-1$
		}
		if (e.getTest() != null) {
			j.put("test", e.getTest()); //$NON-NLS-1$
		}
		if (e.getMetadata() != null) {
			j.put("metadata", writeMetadata(e.getMetadata())); //$NON-NLS-1$
		}
	}

	private JSONObject writeMetadata(BeanMap metadata) throws JSONException {
		JSONObject result = new JSONObject();
		for (Entry<String, Object> e: metadata.entrySet()) {
			if (e.getValue() instanceof Date v) {
				result.put(e.getKey(), ISODateFormater.toString(v));
			} else if (e.getValue() instanceof Calendar v) {
				result.put(e.getKey(), ISODateFormater.toString(v));
			} else if (isSimpleValue(e.getValue())) {
				result.put(e.getKey(), e.getValue());
			} else if (e.getValue() instanceof BeanMap v) {
				result.put(e.getKey(), writeMetadata(v));
			} else if (e.getValue() instanceof Collection v) {
				result.put(e.getKey(), writeCollection(v));
			} else if (e.getValue() != null) {
				result.put(e.getKey(), e.getValue().toString());
			}
		}
		return result;
	}
	
	private JSONArray writeCollection(Collection<?> list) throws JSONException {
		JSONArray result = new JSONArray();
		for (Object o: list) {
			if (o instanceof Date v) {
				result.put(ISODateFormater.toString(v));
			} else if (o instanceof Calendar v) {
				result.put(ISODateFormater.toString(v));
			} else if (isSimpleValue(o)) {
				result.put(o);
			} else if (o instanceof BeanMap v) {
				result.put(writeMetadata(v));
			} else if (o instanceof Collection v) {
				result.put(writeCollection(v));
			} else if (o != null) {
				result.put(o.toString());
			}
		}
		return result;
	}

	private boolean isSimpleValue(Object v) {
		return (v instanceof Integer) ||
				(v instanceof Boolean) ||
				(v instanceof Long) ||
				(v instanceof Date) ||
				(v instanceof Calendar) ||
				(v instanceof Instant) ||
				(v instanceof String) ||
				(v instanceof Double) ||
				(v instanceof Float);
	}

	public MetaDataEntity readEntity(String json) {
		try {
			final JSONObject e = new JSONObject(json);
			final MetaDataEntity result;
			String type = ""; //$NON-NLS-1$
			if (e.has("type")) { //$NON-NLS-1$
				type = e.getString("type"); //$NON-NLS-1$
			}
			int version = 0;
			if (e.has("version")) { //$NON-NLS-1$
				version = e.getInt("version"); //$NON-NLS-1$
			}
			if (e.getBoolean(TAG_UPDATEENTITY)) {
				result = new UpdateMetaDataEntity(type, version);
			} else {
				result = new MetaDataEntity(type, version);
			}
			if (e.has("domain")) { //$NON-NLS-1$
				result.setDomain(e.getString("domain")); //$NON-NLS-1$
			}
			if (e.has("name")) { //$NON-NLS-1$
				result.setName(e.getString("name")); //$NON-NLS-1$
			}
			if (e.has("description")) { //$NON-NLS-1$
				result.setDomain(e.getString("description")); //$NON-NLS-1$
			}
			if (e.has("readonly") && e.getBoolean("readonly")) { //$NON-NLS-1$ //$NON-NLS-2$
				result.setReadOnly(true);
			}
			if (e.has("lockable") && e.getBoolean("lockable")) { //$NON-NLS-1$ //$NON-NLS-2$
				result.setLockable(true);
			}
			if (e.has("date")) { //$NON-NLS-1$
				result.setDate(getDate(e.get("date"))); //$NON-NLS-1$
			}
			if (e.has("metadata")) { //$NON-NLS-1$
				result.setMetadata(readMetadata(e.getJSONObject("metadata"))); //$NON-NLS-1$
			}
			if (e.has("rights")) { //$NON-NLS-1$
				JSONObject rights = e.getJSONObject("rights"); //$NON-NLS-1$
				if (rights.has("create")) { //$NON-NLS-1$
					result.setCreate(convert(rights.getJSONObject("create"))); //$NON-NLS-1$
				}
				if (rights.has("update")) { //$NON-NLS-1$
					result.setUpdate(convert(rights.getJSONObject("update"))); //$NON-NLS-1$
				}
				if (rights.has("delete")) { //$NON-NLS-1$
					result.setDelete(convert(rights.getJSONObject("delete"))); //$NON-NLS-1$
				}
				if (rights.has("list")) { //$NON-NLS-1$
					result.setList(convert(rights.getJSONObject("list"))); //$NON-NLS-1$
				}
				if (rights.has("read")) { //$NON-NLS-1$
					result.setRead(convert(rights.getJSONObject("read"))); //$NON-NLS-1$
				}
			}
			if (e.has("attributes")) { //$NON-NLS-1$
				JSONArray array = e.getJSONArray("attributes"); //$NON-NLS-1$
				for (int i = 0; i < array.length(); i++) {
					JSONObject j = array.getJSONObject(i);
					if (j.has("type")) { //$NON-NLS-1$
						type = j.getString("type"); //$NON-NLS-1$
					} else {
						type = ""; //$NON-NLS-1$
					}
					MetaDataAttribute a = new MetaDataAttribute(result, type);
					fillElement(a, j);
					if (j.has("length")) { //$NON-NLS-1$
						a.setLength(j.getInt("length")); //$NON-NLS-1$
					}
					if (j.has("precision")) { //$NON-NLS-1$
						a.setPrecision(j.getInt("precision")); //$NON-NLS-1$
					}
					if (j.has("listable") && j.getBoolean("listable")) { //$NON-NLS-1$ //$NON-NLS-2$
						a.setListable(true);
					}
					if (j.has("mandatory") && j.getBoolean("mandatory")) { //$NON-NLS-1$ //$NON-NLS-2$
						a.setMandatory(true);
					}
					if (j.has("rights")) { //$NON-NLS-1$
						JSONObject rights = j.getJSONObject("rights"); //$NON-NLS-1$
						if (rights.has("update")) { //$NON-NLS-1$
							a.setUpdate(convert(rights.getJSONObject("update"))); //$NON-NLS-1$
						}
						if (rights.has("read")) { //$NON-NLS-1$
							a.setRead(convert(rights.getJSONObject("read"))); //$NON-NLS-1$
						}
					}
					result.addAttribute(a);
				}
			}
			if (e.has("tests")) { //$NON-NLS-1$
				JSONArray array = e.getJSONArray("tests"); //$NON-NLS-1$
				for (int i = 0; i < array.length(); i++) {
					JSONObject j = array.getJSONObject(i);
					String code = ""; //$NON-NLS-1$
					if (j.has("code")) { //$NON-NLS-1$
						code = j.getString("code"); //$NON-NLS-1$
					}
					String event = null;
					if (j.has("event")) { //$NON-NLS-1$
						event = j.getString("event"); //$NON-NLS-1$
					}
					String test = null;
					if (j.has("test")) { //$NON-NLS-1$
						test = j.getString("test"); //$NON-NLS-1$
					}
					result.addTest(new MetaDataTest(result, code, event, test));
				}
			}
			if (e.has("links")) { //$NON-NLS-1$
				JSONArray array = e.getJSONArray("links"); //$NON-NLS-1$
				for (int i = 0; i < array.length(); i++) {
					JSONObject j = array.getJSONObject(i);
					if (j.has("type")) { //$NON-NLS-1$
						type = j.getString("type"); //$NON-NLS-1$
					} else {
						type = ""; //$NON-NLS-1$
					}
					MetaDataLink l = new MetaDataLink(result);
					fillElement(l, j);
					if (j.has("rights")) { //$NON-NLS-1$
						JSONObject rights = j.getJSONObject("rights"); //$NON-NLS-1$
						if (rights.has("create")) { //$NON-NLS-1$
							l.setCreate(convert(rights.getJSONObject("create"))); //$NON-NLS-1$
						}
						if (rights.has("list")) { //$NON-NLS-1$
							l.setList(convert(rights.getJSONObject("list"))); //$NON-NLS-1$
						}
					}
					result.addLink(l);
				}
			}
			return result;
		} catch (JSONException e) {
			return null;
		}
	}

	private void fillElement(Element e, JSONObject j) throws JSONException {
		if (j.has("code")) { //$NON-NLS-1$
			e.setCode(j.getString("code")); //$NON-NLS-1$
		}
		if (j.has("name")) { //$NON-NLS-1$
			e.setName(j.getString("name")); //$NON-NLS-1$
		}
		if (j.has("description")) { //$NON-NLS-1$
			e.setDescription(j.getString("description")); //$NON-NLS-1$
		}
		if (j.has("type")) { //$NON-NLS-1$
			e.setType(j.getString("type")); //$NON-NLS-1$
		}
		if (j.has("test")) { //$NON-NLS-1$
			e.setTest(j.getString("test")); //$NON-NLS-1$
		}
		if (j.has("metadata")) { //$NON-NLS-1$
			e.setMetadata(readMetadata(j.getJSONObject("metadata"))); //$NON-NLS-1$
		}
		if (j.has("readonly") && j.getBoolean("readonly")) { //$NON-NLS-1$ //$NON-NLS-2$
			e.setReadonly(true);
		}
	}

	private BeanMap readMetadata(JSONObject json) throws JSONException {
		BeanMap result = new BeanMap();
		Iterator<?> itt = json.keys();
		while (itt.hasNext()) {
			String k = (String) itt.next();
			if ("id".equals(k)) {
				result.forceId(json.getInt(k));
			} else if ("type".equals(k)) {
				result.setType(json.getString(k));
			} else if ("muid".equals(k)) {
				result.setMUID(json.getInt(k));
			} else if ("date".equals(k)) {
				try {
					result.setDate(ISODateFormater.toDate(json.getString(k)));
				} catch (ParseException e) {
					throw new JSONException("Not an ISO date: " + json.getString(k));
				}
			} else if (json.isNull(k)) {
				result.put(k, null);
			} else {
				Object x = json.get(k);
				if (x instanceof JSONObject j) {
					result.put(k, readMetadata(j));
				} else if (x instanceof JSONArray a) {
					BeanMapList list = new BeanMapList();
					for (int i = 0; i < a.length(); i++) {
						list.add(readMetadata(a.getJSONObject(i)));
					}
					result.put(k, list);
				} else {
					result.put(k, x);
				}
			}
		}
		return result;
	}

	public String writeEntities(Collection<MetaDataEntity> list, boolean description) {
		if (list == null) {
			return null;
		}
		JSONArray array = new JSONArray();
		try {
			for (MetaDataEntity e: list) {
				if (e != null) {
					array.put(convert(e, description));
				}
			}
		} catch (JSONException e) {}
		return array.toString();
	}

}
