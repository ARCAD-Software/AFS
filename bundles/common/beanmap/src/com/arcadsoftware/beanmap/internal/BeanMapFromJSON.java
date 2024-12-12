package com.arcadsoftware.beanmap.internal;

import java.text.ParseException;
import java.util.Iterator;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.arcadsoftware.beanmap.BeanMap;
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
				bean.put(k, o.get(k));
			}
		}
		return bean;
	}
	
	private BeanMapFromJSON() {}

}
