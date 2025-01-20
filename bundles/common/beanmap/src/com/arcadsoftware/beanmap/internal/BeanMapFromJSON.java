package com.arcadsoftware.beanmap.internal;

import java.text.ParseException;
import java.util.Iterator;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.osgi.ISODateFormater;

public final class BeanMapFromJSON {

	public static BeanMap parse(BeanMap bean, JSONObject o) throws JSONException {
		Iterator<?> itt = o.keys();
		while (itt.hasNext()) {
			String k = (String) itt.next();
			if ("id".equals(k)) {
				bean.forceId(o.getInt(k));
			} else if ("type".equals(k)) {
				bean.setType(o.getString(k));
			} else if ("date".equals(k)) {
				try {
					bean.setDate(ISODateFormater.toDate(o.getString(k)));
				} catch (ParseException e) {
					throw new JSONException("Not an ISO date: " + o.getString(k));
				}
			} else if (o.isNull(k)) {
				bean.put(k, null);
			} else {
				Object x = o.get(k);
				if (x instanceof JSONObject) {
					bean.put(k, parse(new BeanMap(k), (JSONObject) x));
				} else if (x instanceof JSONArray) {
					BeanMapList list = new BeanMapList();
					for (int i = 0; i < ((JSONArray) x).length(); i++) {
						list.add(parse(new BeanMap(), ((JSONArray) x).getJSONObject(i)));
					}
					bean.put(k, list);
				} else {
					bean.put(k, x);
				}
			}
		}
		return bean;
	}

	private BeanMapFromJSON() {}

}
