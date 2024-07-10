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
package com.arcadsoftware.beanmap;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IAdaptable;
import org.restlet.data.Form;

import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.osgi.ISODateFormater;
import com.thoughtworks.xstream.converters.ConversionException;
import com.thoughtworks.xstream.converters.basic.DateConverter;

/**
 * The BeanMap class is an generic implementation of bean objects that are send to clients.
 * 
 * <p>
 * This implementation support dynamic attributes. It is not really optimized for modifications and data injection.
 * 
 * <p>
 * To be able to deserialize this class the BeanMap must NOT be derived.
 * 
 */
public final class BeanMap implements Map<String, Object>, IBeanMap, IIdentifiedBean, ITypedBean, IDatedBean, IDeletableBean,
		Cloneable, Serializable, Comparable<IBeanMap> {

	private static final long serialVersionUID = 8028198902353463923L;

	/*
	 * Shared Date Converter (use a pool of 20 SimpleFormater);
	 * Creation of this Object may take some time (2 to 6 seconds) so it is created once for all.
	 */
	public static final DateConverter DATECONVERTER = new DateConverter("yyyy-MM-dd'T'HH:mm:ss", new String[] { //$NON-NLS-1$ 
			"yyyy-MM-dd HH:mm:ss.S z", //$NON-NLS-1$
					"yyyy-MM-dd HH:mm:ss.S a", //$NON-NLS-1$
					"yyyy-MM-dd HH:mm:ssz", //$NON-NLS-1$ 
					"yyyy-MM-dd HH:mm:ss z", //$NON-NLS-1$
					"yyyy-MM-dd HH:mm:ssa", //$NON-NLS-1$
					"EEE MMM dd HH:mm:ss zzz yyyy" }); //$NON-NLS-1$;

	private static final int NEW_BEAN_MAP_ID = 0;
	
	/**
	 * Helper method.
	 * 
	 * Load a BeanMap from an XML fragment string.
	 * 
	 * @param type
	 *            the desired type of the BeanMap, may be null.
	 * @param xml
	 *            the XML fragment.
	 * @return null if the XML fragment does not conform to the expected format.
	 */
	static public BeanMap loadFromXml(String type, String xml) {
		XmlBeanMapStream xs = new XmlBeanMapStream();
		if (type != null) {
			xs.alias(type, BeanMap.class);
		}
		Object result = null;
		try {
			result = xs.fromXML(xml);
		} catch (Exception e) {
			return null;
		}
		if (result instanceof BeanMap) {
			return (BeanMap) result;
		}
		if (result instanceof IAdaptable) {
			result = ((IAdaptable) result).getAdapter(BeanMap.class);
			if (result instanceof BeanMap) {
				return (BeanMap) result;
			}
		}
		return null;
	}
	
	/**
	 * Helper method.
	 * 
	 * Load a BeanMap from an JSON document string.
	 * 
	 * @param type
	 *            the desired type of the BeanMap, may be null.
	 * @param json
	 *            the JSon document.
	 * @return null if the XML fragment does not conform to the expected format.
	 */
	static public BeanMap loadFromJSon(String type, String json) {
		JSonBeanMapStream xs = new JSonBeanMapStream();
		if (type != null) {
			xs.alias(type, BeanMap.class);
		}
		Object result = null;
		try {
			result = xs.fromXML(json);
		} catch (Exception e) {
			return null;
		}
		if (result == null) {
			return null;
		}
		if (result instanceof BeanMap) {
			return (BeanMap) result;
		}
		if (result instanceof IAdaptable) {
			result = ((IAdaptable) result).getAdapter(BeanMap.class);
			if (result instanceof BeanMap) {
				return (BeanMap) result;
			}
		}
		try {
			return new BeanMap(result);
		} catch (IntrospectionException e) {
			return null;
		}
	}

	// Technical attributes :
	private int id = NEW_BEAN_MAP_ID;
	private String type;
	private Date date;
	private boolean deleted;
	
	// Other attributes :
	private HashMap<String, Object> list;

	/**
	 * Create a default empty BeanMap.
	 */
	public BeanMap() {
		super();
		list = new HashMap<String, Object>(9);
	}

	/**
	 * Create a default empty BeanMap.
	 * 
	 * @param type
	 *            the attribute type.
	 */
	public BeanMap(String type) {
		this();
		this.type = type;
	}

	/**
	 * Create a BeanMap with an ID.
	 * 
	 * @param id
	 *            the internal identification.
	 */
	public BeanMap(int id) {
		this();
		this.id = id;
	}

	/**
	 * Create a BeanMap with an ID.
	 * 
	 * @param type
	 *            the attribute type.
	 * @param id
	 *            the internal identification.
	 */
	public BeanMap(String type, int id) {
		this(type);
		this.id = id;
	}

	/**
	 * Create an identified BeanMap.
	 * 
	 * @param id
	 *            the internal identification.
	 * @param code
	 *            the public identification.
	 */
	public BeanMap(int id, String code) {
		this(id);
		put(KEY_CODE, code);
	}

	/**
	 * Create an identified BeanMap.
	 * 
	 * @param type
	 *            the attribute type.
	 * @param id
	 *            the internal identification.
	 * @param code
	 *            the public identification.
	 */
	public BeanMap(String type, int id, String code) {
		this(type, id);
		put(KEY_CODE, code);
	}

	/**
	 * Create an identified BeanMap.
	 * 
	 * @param id
	 *            the internal identification.
	 * @param code
	 *            the public identification.
	 * @param text
	 *            the description text.
	 */
	public BeanMap(int id, String code, String text) {
		this(id, code);
		put(KEY_TEXT, text);
	}

	/**
	 * Create an identified BeanMap.
	 * 
	 * @param type
	 *            the attribute type.
	 * @param id
	 *            the internal identification.
	 * @param code
	 *            the public identification.
	 * @param text
	 *            the description text.
	 */
	public BeanMap(String type, int id, String code, String text) {
		this(type, id, code);
		put(KEY_TEXT, text);
	}

	/**
	 * Create an identified BeanMap with a first value.
	 * 
	 * @param type
	 *            the attribute type.
	 * @param id
	 *            the internal identification.
	 * @param key
	 * @param value
	 */
	public BeanMap(String type, int id, String key, Object value) {
		this(type, id);
		put(key, value);
	}

	/**
	 * Create a new BeanMap from an original one.
	 * 
	 * <p>
	 * If this object implement the typed, identified and dated interfaces then the current BeanMap will reuse theses
	 * values.
	 * 
	 * @param beanMap
	 *            the source BeanMap.
	 */
	public BeanMap(BeanMap beanMap) {
		this((IBeanMap) beanMap);
	}
	/**
	 * Create a new BeanMap from an original one.
	 * 
	 * <p>
	 * If this object implement the typed, identified and dated interfaces then the current BeanMap will reuse theses
	 * values.
	 * 
	 * @param beanMap
	 *            the source IBeanMap.
	 */
	public BeanMap(IBeanMap beanMap) {
		this();
		if (beanMap instanceof ITypedBean) {
			this.type = beanMap.getType();
		}
		if (beanMap instanceof IIdentifiedBean) {
			this.id = beanMap.getId();
		}
		if (beanMap instanceof IDatedBean) {
			this.date = ((IDatedBean) beanMap).getDate();
		}
		addAll(beanMap);
	}

	/**
	 * Create a BeanMap from a Dictionary source.
	 * 
	 * <p>
	 * The properties from the Dictionary are used to set the properties of the BeanMap and the fields as well (id,
	 * type, date).
	 * 
	 * @param source
	 */
	@SuppressWarnings("rawtypes")
	public BeanMap(Dictionary source) {
		this();
		Enumeration e = source.keys();
		while (e.hasMoreElements()) {
			Object k = e.nextElement();
			if (k != null) {
				Object v = source.get(k);
				String ks = k.toString();
				if (v == null) {
					put(ks, null);
				} else if (KEY_ID.equals(ks)) {
					if (v instanceof Integer) {
						id = (Integer) v;
					} else {
						try {
							id = Integer.parseInt(v.toString());
						} catch (NumberFormatException ee) {
							put(ks, v);
						}
					}
				} else if (KEY_DATE.equals(ks)) {
					if (v instanceof Date) {
						date = (Date) v;
					} else if (v instanceof Calendar) {
						date = ((Calendar) v).getTime();
					} else {
						String d = v.toString();
						if (ISODateFormater.mayIsoDate(d)) {
							try {
								date = ISODateFormater.toDate(d);
							} catch (ParseException ee) {
								put(ks, v);
							}
						} else {
							try {
								Object x = DATECONVERTER.fromString(d);
								if (x instanceof Date) {
									date = (Date) x;
								} else if (x instanceof Calendar) {
									date = ((Calendar) x).getTime();
								} else {
									put(ks, v);
								}
							} catch (ConversionException ee) {
								put(ks, v);
							}
						}
					}
				} else if (KEY_TYPE.equals(ks)) {
					type = v.toString();
				} else {
					put(ks, v);
				}
			}
		}
	}

	/**
	 * Create a BeanMap from a Dictionary source.
	 * 
	 * <p>
	 * The properties from the Dictionary are used to set the properties of the BeanMap.
	 * 
	 * @param source
	 * @param type
	 *            The BeanMap type
	 * @param id
	 *            The BeanMap internal id.
	 */
	public BeanMap(@SuppressWarnings("rawtypes") Dictionary source, String type, int id) {
		this(source);
		this.type = type;
		this.id = id;
	}

	/**
	 * Create a BeanMap from a Dictionary source.
	 * 
	 * <p>
	 * The properties from the Dictionary are used to set the properties of the BeanMap and the fields as well (id,
	 * type, date).
	 * 
	 * @param source
	 */
	@SuppressWarnings("rawtypes")
	public BeanMap(Map source) {
		this();
		for(Object e: source.entrySet()) {
			Object k = ((Entry) e).getKey();
			if (k != null) {
				Object v = ((Entry) e).getValue();
				String ks = k.toString();
				if (v == null) {
					put(ks, null);
				} else if (KEY_ID.equals(ks)) {
					if (v instanceof Integer) {
						id = (Integer) v;
					} else {
						try {
							id = Integer.parseInt(v.toString());
						} catch (NumberFormatException ee) {
							put(ks, v);
						}
					}
				} else if (KEY_DATE.equals(ks)) {
					if (v instanceof Date) {
						date = (Date) v;
					} else if (v instanceof Calendar) {
						date = ((Calendar) v).getTime();
					} else {
						String d = v.toString();
						if (ISODateFormater.mayIsoDate(d)) {
							try {
								date = ISODateFormater.toDate(d);
							} catch (ParseException ee) {
								put(ks, v);
							}
						} else {
							try {
								Object x = DATECONVERTER.fromString(d);
								if (x instanceof Date) {
									date = (Date) x;
								} else if (x instanceof Calendar) {
									date = ((Calendar) x).getTime();
								} else {
									put(ks, v);
								}
							} catch (ConversionException ee) {
								put(ks, v);
							}
						}
					}
				} else if (KEY_TYPE.equals(ks)) {
					type = v.toString();
				} else {
					put(ks, v);
				}
			}
		}
	}

	/**
	 * Create a BeanMap from a Dictionary source.
	 * 
	 * <p>
	 * The properties from the Dictionary are used to set the properties of the BeanMap.
	 * 
	 * @param source
	 * @param type
	 *            The BeanMap type
	 * @param id
	 *            The BeanMap internal id.
	 */
	public BeanMap(@SuppressWarnings("rawtypes") Map source, String type, int id) {
		this(source);
		this.type = type;
		this.id = id;
	}

	/**
	 * Create an BeanMap from an existing Java Bean using introspection.
	 * 
	 * <p>
	 * Note that if the BeanMap use is to be sent via XML serialization, the BeanMap serialization is compatible with
	 * standard Java Bean one, it should be most faster to directly serialize the java using XSteam.
	 * 
	 * @param bean The java Bean to "clone"
	 * @param type The BeanMap type name
	 * @param id The BeanMap default id (if the java bean possess an "id" property then this property value is used).
	 * @param beanInfo
	 */
	public BeanMap(Object bean, String type, int id, BeanInfo beanInfo) {
		this(type, id);
		if (type == null) {
			this.type = beanInfo.getBeanDescriptor().getName();
		}
		for (PropertyDescriptor property : beanInfo.getPropertyDescriptors()) {
			if (KEY_ID.equalsIgnoreCase(property.getName())) {
				setId(id);
			} else {
				Method read = property.getReadMethod();
				if (read != null) {
					try {
						put(property.getName(), read.invoke(bean));
					} catch (IllegalArgumentException e) {
						// FIXME Journalizer l'erreur quand on est en debug !
					} catch (IllegalAccessException e) {
						// FIXME Journalizer l'erreur quand on est en debug !
					} catch (InvocationTargetException e) {
						// FIXME Journalizer l'erreur quand on est en debug !
					}
				}
			}
		}
	}

	/**
	 * Create an BeanMap from an existing Java Bean using introspection.
	 * 
	 * <p>
	 * Note that if the BeanMap use is to be sent via XML serialization, the BeanMap serialization is compatible with
	 * standard Java Bean one, it should be most faster to directly serialize the java using XSteam.
	 * 
	 * @param bean The java Bean to "clone"
	 * @param type The BeanMap type name
	 * @param id The BeanMap default id (if the java bean posses an "id" property then this porperty value is used).
	 * @throws IntrospectionException
	 */
	public BeanMap(Object bean, String type, int id) throws IntrospectionException {
		this(bean, type, id, Introspector.getBeanInfo(bean.getClass(), Object.class));
	}

	/**
	 * Create a BeanMap from a Restlet Form.
	 * 
	 * @param type
	 *            the attribute type.
	 * @param form
	 *            The Restlet Form to transform.
	 * @see #loadFromForm(Form)
	 */
	public BeanMap(String type, Form form) {
		this(type);
		loadFromForm(form);
	}


	/**
	 * Create a BeanMap from a Restlet Form.
	 * 
	 * @param type
	 *            the attribute type.
	 * @param id
	 *            The BeanMap internal id.
	 * @param form
	 *            The Restlet Form to transform.
	 * @see #loadFromForm(Form)
	 */
	public BeanMap(String type, int id, Form form) {
		this(type, id);
		loadFromForm(form);
	}

	/**
	 * Create an BeanMap from an existing Java Bean using introspection.
	 * 
	 * <p>
	 * Note that if the BeanMap use is to be sent via XML serialization, the BeanMap serialization is compatible with
	 * standard Java Bean one, it should be most faster to directly serialize the java bean using XSteam.
	 * 
	 * @param bean The java Bean to "clone"
	 * @throws IntrospectionException
	 */
	public BeanMap(Object bean) throws IntrospectionException {
		this(bean, null, NEW_BEAN_MAP_ID, Introspector.getBeanInfo(bean.getClass(), Object.class));
	}

	/**
	 * Set an attribute Value
	 * 
     * @param key The attribute key with which the specified value is to be associated
     * @param value value to be associated with the specified attribute
     * @return the previous value associated with <tt>key</tt>, or
     *         <tt>null</tt> if there was no mapping for <tt>key</tt>.
     *         (A <tt>null</tt> return can also indicate that the BeanMap
     *         previously associated <tt>null</tt> with <tt>key</tt>.)
	 */
	public Object put(String key, Object value) {
		return list.put(key, value);
	}

	/**
	 * Removes the mapping for the specified attribute key from this BeanMap if present.
	 * 
	 * @param key
	 *            key whose mapping is to be removed from the BeanMap
	 * @return the previous value associated with <tt>key</tt>, or <tt>null</tt> if there was no mapping for
	 *         <tt>key</tt>. (A <tt>null</tt> return can also indicate that the BeanMap previously associated
	 *         <tt>null</tt> with <tt>key</tt>.)
	 */
	public Object remove(String key) {
		return list.remove(key);
	}

	/**
	 * Remove all value matching the given key.
	 * 
	 * @param key
	 * @param ignoreCase determine is the search is case sensitive, note that if this parameter is false the result is equal to remove(String).
	 * @return the removed values (an Object or a array of objects).
	 */
	public Object remove(String key, boolean ignoreCase) {
		if (ignoreCase) {
			Iterator<String> i = list.keySet().iterator();
			ArrayList<Object> results = new ArrayList<Object>(); 
			while (i.hasNext()) {
				String k = i.next();
				if (k.equalsIgnoreCase(key)) {
					results.add(list.get(k));
					i.remove();
				}
			}
			return results.toArray(new Object[results.size()]);
		} else {
			return list.remove(key);
		}
	}

	/**
	 * Remove all element with the given key prefix.
	 * 
	 * @param prefix
	 * @param ignoreCase determine is the search is case sensitive.
	 * @return the list of removed value, or an empty array if none.
	 */
	public Object[] removePrefix(String prefix, boolean ignoreCase) {
		ArrayList<Object> results = new ArrayList<Object>(); 
		if (ignoreCase) {
			prefix = prefix.toLowerCase();
			Iterator<String> i = list.keySet().iterator();
			while (i.hasNext()) {
				String k = i.next();
				if (k.toLowerCase().startsWith(prefix)) {
					results.add(list.get(k));
					i.remove();
				}
			}
		} else {
			Iterator<String> i = list.keySet().iterator();
			while (i.hasNext()) {
				String k = i.next();
				if (k.startsWith(prefix)) {
					results.add(list.get(k));
					i.remove();
				}
			}
		}
		return results.toArray(new Object[results.size()]);
	}

	/**
	 * Remove all attributes of this BeanMap.
	 */
	public void clear() {
		list.clear();
	}

	/**
	 * Set all attributes values to null. But do not remove theses values.
	 */
	public void putToNull() {
		for (String key : list.keySet()) {
			list.put(key, null);
		}
	}

	public Object get(String key) {
		return list.get(key);
	}

	/**
	 * Return the value of the given key or the default value if its null.
	 * @param key the attribute code.
	 * @param defaultValue a default value.
	 * @return the value.
	 */
	public Object get(String key, Object defaultValue) {
		Object result = list.get(key);
		if (result == null) {
			return defaultValue;
		}
		return result;
	}

	public <T> T get(String key, Class<T> clazz) {
		Object result = list.get(key);
		if (result == null) {
			return null;
		}
		if (clazz.isInstance(result)) {
			return clazz.cast(result);
		}
		if (clazz.equals(Integer.class)) {
			// Return the BeanMap Id.
			if (result instanceof BeanMap) {
				result = ((BeanMap) result).getId();
			} else {
				try {
					result = Integer.decode(result.toString());
				} catch (NumberFormatException e) {
					return null;
				}
			}
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		} else if (clazz.equals(String.class)) {
			result = result.toString();
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		} else if (clazz.equals(Date.class)) {
			String d = result.toString();
			if (ISODateFormater.mayIsoDate(d)) {
				try {
					result = ISODateFormater.toDate(d);
				} catch (ParseException e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				}
			} else {
				try {
					result = DATECONVERTER.fromString(d);
				} catch (ConversionException e) {
					// FIXME Journalizer l'erreur quand on est en debug !
					return null;
				}
			}
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		} else if (clazz.equals(Float.class)) {
			try {
				result = Float.valueOf(result.toString());
			} catch (NumberFormatException e) {
				return null;
			}
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		}
		return null;
	}

	/**
	 * @return the number of actual attributes.
	 */
	public int size() {
		return list.size();
	}

	/**
	 * @return true is this BeanMap contain no specifics informations (aka. attributes).
	 */
	public boolean isEmpty() {
		return list.isEmpty();
	}

	/**
	 * @return the current list of used keys.
	 */
	public Set<String> keys() {
		return list.keySet();
	}

	public int getId() {
		return id;
	}

	/**
	 * Change the Id of this BeanMap.
	 * 
	 * <p>
	 * This modification is allowed only if the current Id is null.
	 * 
	 * @param id
	 */
	public void setId(int id) {
		if (this.id == 0) {
			this.id = id;
		}
	}

	/**
	 * Change the current id of the BeanMap, even if the current ID is not null.
	 * 
	 * <p>
	 * <b>Note:</b> Changing the Id of a BeanMap may have side effect on the way other use it.
	 * @param id
	 * @see #clone(int)
	 */
	public void forceId(int id) {
		this.id = id;
	}

	public String getType() {
		return type;
	}

	/**
	 * Change the type of the BeanMap.
	 * 
	 * <p>BeanMap type define the interface connection to the server 
	 * and the attribute nature of it. 
	 * Changing the BeanMap type may produce incoherent behavior. You 
	 * may need to clone the BeanMap before changing its type.
	 * @param type
	 */
	public void setType(String type) {
		this.type = type;
	}
	
	public void setDate(Date date) {
		this.date = date;
	}

	public Date getDate() {
		return date;
	}

	/**
	 * Set the deleted status of this BeanMap.
	 * 
	 * @param deleted the deleted status to set
	 */
	public void setDeleted(boolean deleted) {
		this.deleted = deleted;
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.beanmap.IDeletableBean#isDeleted()
	 */
	public boolean isDeleted() {
		return deleted;
	}

	/**
	 * Return the key value as an integer.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return zero if the key does not exist, -1 if the key is not an integer.
	 */
	public int getInt(String key) {
		Object o = get(key);
		if (o == null) {
			return 0;
		}
		if (o instanceof Short) {
			return ((Short) o).intValue();
		}
		if (o instanceof Integer) {
			return (Integer) o;
		}
		if (o instanceof BigInteger) {
			return ((BigInteger) o).intValue();
		}
		if (o instanceof Long) {
			return ((Long) o).intValue();
		}
		if (o instanceof BigDecimal) {
			return ((BigDecimal) o).intValue();
		}
		if (o instanceof String) {
			try {
				return Integer.parseInt((String) o);
			} catch (NumberFormatException e) {
				// Do nothing
			}
		}
		return -1;
	}

	/**
	 * Return the key value as a long integer.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return zero if the key does not exist, -1 if the key is not a long integer.
	 */
	public long getLong(String key) {
		Object o = get(key);
		if (o == null) {
			return 0;
		}
		if (o instanceof Long) {
			return (Long) o;
		}
		if (o instanceof Integer) {
			return (Integer) o;
		}
		if (o instanceof BigInteger) {
			return ((BigInteger) o).longValue();
		}
		if (o instanceof BigDecimal) {
			return ((BigDecimal) o).longValue();
		}
		if (o instanceof Date) {
			return ((Date) o).getTime();
		}
		if (o instanceof Calendar) {
			return ((Calendar)o).getTimeInMillis();
		}
		if (o instanceof Timestamp) {
			return ((Timestamp)o).getTime();
		}
		String s = o.toString();
		if (ISODateFormater.mayIsoDate(s)) {
			try {
				return ISODateFormater.toDate(s).getTime();
			} catch (ParseException e) {}
		}
		if (o instanceof String) {
			try {
				return Long.parseLong((String) o);
			} catch (NumberFormatException e) {
				// Do nothing
			}
		}
		return -1;
	}

	/**
	 * Return the key value as a float.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return zero if the key does not exist, -1 if the key is not a float.
	 */
	public float getFloat(String key) {
		Object o = get(key);
		if (o == null) {
			return 0F;
		}
		if (o instanceof Float) {
			return (Float) o;
		}
		try {
			return Float.parseFloat(o.toString());
		} catch (NumberFormatException e) {
			return -1F;
		}
	}
	
	/**
	 * Return the key value as an integer.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return zero if the key does not exist, null if the key is not an integer.
	 */
	public Integer getInteger(String key) {
		Object o = get(key);
		if (o == null) {
			return Integer.valueOf(0);
		}
		if (o instanceof Integer) {
			return (Integer) o;
		}
		if (o instanceof String) {
			try {
				return Integer.valueOf(Integer.parseInt((String) o));
			} catch (NumberFormatException e) {
				// Do nothing
			}
		}
		return null;
	}
	
	/**
	 * Return the key value as an integer.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return zero if the key does not exist, null if the key is not an integer.
	 */
	public BigInteger getBigInteger(String key) {
		Object o = get(key);
		if (o == null) {
			return BigInteger.valueOf(0);
		}
		if (o instanceof BigInteger) {
			return (BigInteger) o;
		}
		try {
			return new BigInteger(o.toString());
		} catch (NumberFormatException e) {
			return null;
		}
	}

	/**
	 * Return the key value as a char array.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return null if the key does not exist or the string representation of the value.
	 */
	public char[] getCharArray(String key) {
		Object o = get(key);
		if (o == null) {
			return null;
		}
		if (o instanceof char[]) {
			return (char[]) o;
		}
		return o.toString().toCharArray();
	}

	/**
	 * Return the key value as a String.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return null if the key does not exist or the string representation of the value.
	 */
	public String getString(String key) {
		Object o = get(key);
		if (o == null) {
			return null;
		}
		if (o instanceof String) {
			return (String) o;
		}
		if (o instanceof char[]) {
			return new String((char[]) o);
		}
		return o.toString();
	}

	/**
	 * Return the key value as a String.
	 * 
	 * @param key
	 *            the attribute key.
	 * @param defaultValue
	 *            The default value.
	 * @return defaultValue if the key does not exist or the string representation of the value.
	 */
	public String getString(String key, String defaultValue) {
		Object o = get(key);
		if (o == null) {
			return defaultValue;
		}
		if (o instanceof String) {
			return (String) o;
		}
		if (o instanceof char[]) {
			return new String((char[]) o);
		}
		return o.toString();
	}

	/**
	 * Get the key value as a BeanMap.
	 * 
	 * <p>
	 * This method try to guess the BeanMap type from the XML code.
	 * This operation may fail if the XML file is too complex. You should consider to 
	 * use the other <code>getBeanMap</code> method if you already know the type
	 * of the BeanMap.
	 * 
	 * @param key the attribute key.
	 * @return null if this key does not contain a BeanMap or is null.
	 * @see #getBeanMap(String, String)
	 */
	public BeanMap getBeanMap(String key) {
		Object o = get(key);
		if (o != null) {
			if (o instanceof BeanMap) {
				return (BeanMap) o;
			}
			if (o instanceof String) {
				try {
					String btype = XmlBeanMapStream.guessTypeFromXML((String) o);
					if (btype == null) {
						return null;
					}
					return loadFromXml(btype, (String) o);
				} catch (Throwable e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				}
			} else if (o instanceof Integer) {
				// Assume that the type is the key code ! 
				BeanMap b = new BeanMap(key, (Integer) o);
				String prefix = key + "."; //$NON-NLS-1$
				for (Entry<String, Object> e: entrySet()) {
					if ((e.getKey() != null) && e.getKey().startsWith(prefix)) {
						b.put(e.getKey().substring(prefix.length()), e.getValue());
					}
				}
				return b;
			}
		}
		return null;
	}

	/**
	 * Get the key value as a BeanMapList.
	 * 
	 * @param key the attribute key.
	 * @return null if this key does not contain a BeanMapList or is null.
	 * @see #getBeanMapList(String, String)
	 */
	public BeanMapList getBeanMapList(String key) {
		Object o = get(key);
		if (o != null) {
			if (o instanceof BeanMapList) {
				return (BeanMapList) o;
			}
			if (o instanceof String) {
				try {
					return BeanMapList.loadFromXml(key, (String) o);
				} catch (Throwable e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				}
			}
		}
		return null;
	}

	/**
	 * Get the key value as a BeanMap.
	 * 
	 * @param key the attribute key.
	 * @param type The BeanMap type.
	 * @return null if this key does not contain a BeanMap or is null.
	 */
	public BeanMap getBeanMap(String key, String type) {
		Object o = get(key);
		if (o != null) {
			if (o instanceof BeanMap) {
				return (BeanMap) o;
			}
			if (o instanceof String) {
				try {
					return loadFromXml(type, (String) o);
				} catch (Throwable e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				}
			} else if (o instanceof Integer) {
				// Reconstruit le beanmap... 
				BeanMap b = new BeanMap(type, (Integer) o);
				String prefix = key + "."; //$NON-NLS-1$
				for (Entry<String, Object> e: entrySet()) {
					if ((e.getKey() != null) && e.getKey().startsWith(prefix)) {
						b.put(e.getKey().substring(prefix.length()), e.getValue());
					}
				}
				return b;
			}
		}
		return null;
	}

	/**
	 * Get the key value as a BeanMapList.
	 * 
	 * @param key the attribute key.
	 * @param type The BeanMap type.
	 * @return null if this key does not contain a BeanMapList or is null.
	 */
	public BeanMapList getBeanMapList(String key, String type) {
		Object o = get(key);
		if (o != null) {
			if (o instanceof BeanMapList) {
				return (BeanMapList) o;
			}
			if (o instanceof String) {
				try {
					return BeanMapList.loadFromXml(type, (String) o);
				} catch (Throwable e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				}
			}
		}
		return null;
	}

	/**
	 * Return the key value as a boolean.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return false if the key is not an boolean representation.
	 */
	public boolean getBoolean(String key) {
		final Object o = get(key);
		if (o == null) {
			return false;
		}
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		if (o instanceof Integer) {
			return ((Integer) o).intValue() != 0;
		}
		if (o instanceof Float) {
			return ((Float) o).floatValue() != 0;
		}
		else {
			//Handles any other type (like Short)
			final String value = String.valueOf(o);		
			return 	"true".equalsIgnoreCase(value) || //$NON-NLS-1$
					"yes".equalsIgnoreCase(value) || //$NON-NLS-1$
					"1".equalsIgnoreCase(value); //$NON-NLS-1$
		
		}
	}

	/**
	 * Return the key value as a boolean.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return null if the key is not a date representation.
	 */
	public Date getDate(String key) {
		Object o = get(key);
		if (o == null) {
			return null;
		}
		if (o instanceof Date) {
			return (Date)o;
		}
		if (o instanceof Calendar) {
			return ((Calendar)o).getTime();
		}
		if (o instanceof Timestamp) {
			return new Date(((Timestamp)o).getTime());
		}
		final String s = o.toString();
		if (ISODateFormater.mayIsoDate(s)) {
			try {
				return ISODateFormater.toDate(s);
			} catch (ParseException e) {
				// FIXME Journalizer l'erreur quand on est en debug !
			}
		}
		DateFormat df = DateFormat.getDateInstance(DateFormat.SHORT);
		try {
			return df.parse(s);
		} catch (ParseException e) {
			// FIXME Journalizer l'erreur quand on est en debug !
		}
		return null;
	}

	/**
	 * @return the list of the attributes of this BeanMap except "id" and "type"
	 */
	public Set<Entry<String, Object>> entrySet() {
		return list.entrySet();
	}

	/**
	 * Insert into this bean all the values contained into another one except its Id and type.
	 * 
	 * @param bean
	 */
	public IBeanMap addAll(IBeanMap bean) {
		if (bean != null) {
			for (Entry<String, Object> entry : bean.entrySet()) {
				Object o = get(entry.getKey());
				if ((o instanceof IBeanMap) && (entry.getValue() instanceof IBeanMap)) {
					((IBeanMap) o).addAll(((IBeanMap) entry.getValue()));
				} else {
					put(entry.getKey(), entry.getValue());
				}
			}
		}
		return this;
	}

	/**
	 * Insert into this bean all the values contained into another one except its Id and type.
	 * 
	 * <p>
	 * Inserted key are prefixed with the given string.
	 * @param prefix
	 * @param bean
	 * @return
	 */
	public IBeanMap addAll(String prefix, BeanMap bean) {
		if (bean != null) {
			for (Entry<String, Object> entry : bean.entrySet()) {
				String key = prefix + entry.getKey();
				Object o = get(key);
				if ((o instanceof IBeanMap) && (entry.getValue() instanceof IBeanMap)) {
					((IBeanMap) o).addAll(((IBeanMap) entry.getValue()));
				} else {
					put(key, entry.getValue());
				}
			}
		}
		return this;
	}
	
	public IBeanMap addAll(String keyAttribute, BeanMapList list) {
		if (list != null) {
			for (BeanMap bm : list) {
				String key = bm.getString(keyAttribute);
				if (key != null) {
					put(key, bm);
				}
			}
		}
		return this;
	}
	
	public IBeanMap addAll(String keyAttribute, String valueAttribute, BeanMapList list) {
		if (list != null) {
			for (BeanMap bm : list) {
				String key = bm.getString(keyAttribute);
				if (key != null) {
					put(key, bm.get(valueAttribute));
				}
			}
		}
		return this;
	}
	
	/**
	 * Update the values of this BeanMap from an XML fragment.
	 * 
	 * @param xml
	 *            the XML string
	 * @return the object deserialized from the XML fragment, this object can be the BeanMap itself.
	 */
	public Object update(String xml) {
		XmlBeanMapStream xs = new XmlBeanMapStream(this);
		return xs.fromXML(xml);
	}

	/**
	 * Helper method.
	 * 
	 * Serialize the BeanMap to an XML fragment string.
	 * 
	 * @return thE XML string
	 */
	public String saveAsXml() {
		XmlBeanMapStream xs = new XmlBeanMapStream(getClass().getClassLoader());
		return xs.toXML(this);
	}

	/**
	 * Helper method. Create an Object  
	 * @param <T>  
	 * @param clazz The Java bean class.
	 * @return
	 * @throws IntrospectionException
	 */
	public <T> T saveAsBean(Class<T> clazz) throws IntrospectionException {
		T result;
		try {
			result = clazz.getConstructor().newInstance();
		} catch (Throwable e) {
			throw new IntrospectionException(e.getLocalizedMessage());
		}
		toBean(result);
		return result;
	}

	/**
	 * Generate an HTTP Form representation of this BeanMap.
	 * 
	 * @return a Form object (never return Null).
	 */
	public Form saveAsForm() {
		Form form = new Form();
		XmlBeanMapStream xs = null;
		for (Entry<String, Object> entry : list.entrySet()) {
			Object o = entry.getValue();
			if (o == null) {
				form.add(entry.getKey(), ""); //$NON-NLS-1$
			} else if (o instanceof String) {
				form.add(entry.getKey(), (String) o);
			} else if ((o instanceof BigInteger) || //
					(o instanceof Long)) {
				form.add(entry.getKey(), "<long>" + o.toString() + "</long>"); //$NON-NLS-1$ //$NON-NLS-2$
			} else if ((o instanceof Integer) || //
					(o instanceof Boolean) || //
					(o instanceof Float) || //
					(o instanceof Double)) {
				form.add(entry.getKey(), o.toString());
			} else if (o instanceof Date) {
				form.add(entry.getKey(), ISODateFormater.toString((Date) o));
			} else if (o instanceof Calendar) {
				form.add(entry.getKey(), ISODateFormater.toString((Calendar) o));
			} else {
				if (xs == null) {
					xs = new XmlBeanMapStream(getClass().getClassLoader());
				}
				form.add(entry.getKey(), xs.toXML(o));
			}
		}
		if (id > 0) {
			form.add(KEY_ID, Integer.toString(id));
		}
		if (date != null) {
			form.add(KEY_DATE, ISODateFormater.toString(date));
		}
		return form; 
	}

	/**
	 * Set content of this BeanMap to the values contained into the given Restlet Form.
	 * 
	 * <p>
	 * Existing values are preserved.
	 * 
	 * @param form
	 */
	public void loadFromForm(Form form) {
		for (String key : form.getNames()) {
			String value = form.getValues(key);
			// Récupération de l'ID du BeanMap.
			if (KEY_ID.equalsIgnoreCase(key)) {
				try {
					id = Integer.parseInt(value);
				} catch (NumberFormatException e) {}
			}
			// Récupération de la Date du BeanMap.
			if (KEY_DATE.equalsIgnoreCase(key)) {
				if (ISODateFormater.mayIsoDate(value)) {
					try {
						date = ISODateFormater.toDate(value);
					} catch (ParseException e) {}
				}
				if (date == null) {
					try {
						date = new Date(Long.valueOf(value));
					} catch (NumberFormatException ee) {
					}
				}
			}
			// Chargement d'une valeur.
			if (value != null) {
				if (ISODateFormater.mayIsoDate(value)) {
					try {
						put(key, ISODateFormater.toDate(value));
					} catch (ParseException e) {
						put(key, value);
					}
				} else if ("true".equalsIgnoreCase(value)) { //$NON-NLS-1$
					put(key, Boolean.TRUE);
				} else if ("false".equalsIgnoreCase(value)) { //$NON-NLS-1$
					put(key, Boolean.FALSE);
				} else {
					if (value.startsWith("<long>") && value.endsWith("</long>")) { //$NON-NLS-1$ //$NON-NLS-2$
						try {
							put(key,Long.valueOf(value.substring(6, value.length() - 7)));
							continue;
						} catch (Throwable e) {
							// FIXME Journalizer l'erreur quand on est en debug !
						}
					}
					// TODO a lot of "string" value which look like number shoulc not be converted in numbers...
					String lkey = key.toLowerCase();
					if (lkey.contains("version") || //$NON-NLS-1$
							lkey.contains("phone")) { //$NON-NLS-1$
						put(key, value);
					} else {
						try {
							put(key, Integer.valueOf(value));
						} catch (NumberFormatException e) {
							try {
								put(key, Float.valueOf(value));
							} catch (NumberFormatException ee) {
								put(key, value);
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Create a new BeanMap with same id.
	 * 
	 * <p>
	 * The attributes and their values are not duplicated, use <code>clone()</code> for a complete duplication.
	 * The last modification date is reseted to the current date. Deleted flag is dropped.
	 * @see java.lang.Object#clone()
	 * @return a new BeanMap with the same id and type.
	 */
	public BeanMap duplicate() {
		return new BeanMap(getType(), getId());
	}

	public boolean equalsType(ITypedBean bm) {
		if (type == null) {
			return bm.getType() == null;
		}
		return type.equals(bm.getType());
	}


	/**
	 * Create a new BeanMap that only contains the specified values.
	 * 
	 * @param keys a set of keys to add.
	 * @return a new BeanMap with same type and id.
	 */
	public BeanMap extract(String[] keys) {
		BeanMap result = duplicate();
		for(String key:keys) {
			Object value = get(key);
			if (value != null) {
				result.put(key,value);
			}
		}
		return result;
	}

	/**
	 * Create a new BeanMap that only contains the specified values.
	 * 
	 * @param keys a set of keys to add.
	 * @return a new BeanMap with same type and id.
	 */
	public BeanMap extract(Collection<?> keys) {
		BeanMap result = duplicate();
		for(Object key:keys) {
			String skey = key.toString();
			Object value = get(skey);
			if (value != null) {
				result.put(skey,value);
			}
		}
		return result;
	}
	
	/**
	 * Return true if and only if the current BeanMap has a Date more recent than the one of the given one.
	 */
	public boolean moreRecent(IDatedBean bm) {
		Date d = bm.getDate();
		return (date != null) && (d != null) && date.after(d);
	}

	/**
	 * The BeanMap equality is a lazy equivalence. If id and types are equals then the Beans are considered as equal.
	 * Use <code>equalsComplete()<code> to test equality through
	 * the whole values.
	 * 
	 * @param obj
	 *            an ITypedBean or an IIdentifiedBean, or both.
	 * @return true if obj has the same ID and type.
	 * @see java.lang.Object#equals(java.lang.Object)
	 * @see #equalsComplete(Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof IIdentifiedBean) {
			if ((id == 0) || (((IIdentifiedBean) obj).getId() == 0)) {
				// Without an ID a complete comparison is required.
				return 	(obj instanceof Map) && //
						(obj instanceof ITypedBean) && //
						equalsType((ITypedBean) obj) && //
						list.equals((Map<?,?>) obj);
			}
			return (id == ((IIdentifiedBean) obj).getId()) && //
					(obj instanceof ITypedBean) && //
					equalsType((ITypedBean) obj);
		} else if (obj instanceof ITypedBean) {
			return equalsType((ITypedBean) obj);
		}
		return false;
	}

	/**
	 * Test the complete equality of the ID, types and the attributes.
	 * 
	 * @param obj must at least be a Map to be comparable, a IIdentifiedBean or a ITypedBean compare ID and type too.
	 * @return
	 * @see #equals(Object)
	 */
	public boolean equalsComplete(Object obj) {
		if (obj instanceof Map) {
			if (obj instanceof IIdentifiedBean) {
				if ((id == 0) || (((IIdentifiedBean) obj).getId() == 0) || (id == ((IIdentifiedBean) obj).getId())) {
					if (obj instanceof ITypedBean) {
						return equalsType((ITypedBean) obj) && //
								list.equals((Map<?,?>) obj);
					}
					return list.equals((Map<?,?>) obj);
				}
			} else if (obj instanceof ITypedBean) {
				return equalsType((ITypedBean) obj) && //
						list.equals((Map<?,?>) obj);
			} else {
				return list.equals((Map<?,?>) obj);
			}
		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		String code = getString(KEY_CODE);
		if (code != null) {
			sb.append("["); //$NON-NLS-1$
			sb.append(code);
			sb.append("] "); //$NON-NLS-1$
		}
		String text = getString(KEY_TEXT);
		if (text == null) {
			text = getString(KEY_NAME);
		}
		if (text != null) {
			sb.append(text);
		}
		if ((code == null) && (text == null)) {
			sb.append(getId());
			String type = getType();
			if (type != null) {
				sb.append(" ("); //$NON-NLS-1$
				sb.append(type);
				sb.append(")"); //$NON-NLS-1$
			}
		}
		return sb.toString();
	}

	/**
	 * Create a formated String representation of this BeanMap.
	 * 
	 * @return a String representation.
	 */
	public String prettyprint() {
		return prettyprint(0);
	}

	protected String prettyprint(int level) {
		StringBuilder result = new StringBuilder();
		result.append(type);
		result.append('/');
		result.append(id);
		if (date != null) {
			result.append('(');
			result.append(date);
			result.append(')');
		}
		result.append(" {\n"); //$NON-NLS-1$
		for (Entry<String, Object> entry : list.entrySet()) {
			Object value = entry.getValue();
			pad(result, level);
			if (value instanceof BeanMap) {
				result.append(entry.getKey());
				result.append(" = BeanMap {"); //$NON-NLS-1$
				result.append(((BeanMap) value).prettyprint(level + 1));
				pad(result, level);
				result.append("}\n"); //$NON-NLS-1$
			} else {
				result.append(entry.getKey());
				result.append(" = "); //$NON-NLS-1$
				result.append(value);
				result.append("\n"); //$NON-NLS-1$
			}
		}
		pad(result, level);
		result.append("}\n"); //$NON-NLS-1$
		return result.toString();
	}

	private void pad(StringBuilder s, int level) {
		for (int i = 0; i < level; i++) {
			s.append(' ');
		}
	}

	/**
	 * Duplicate this BeanMap an change it identifier.
	 * Any other values is unchanged (type, date, delete state, and values).
	 * 
	 * <p>
	 * Values themselves are not cloned.
	 * 
	 * @param id the new identifier of this BeanMap.
	 * @return a non null BeanMap with specified identifier.
	 */
	@SuppressWarnings("unchecked")
	public BeanMap clone(int id) {
		BeanMap result = new BeanMap(type, id);
		result.setDate(date);
		result.setDeleted(deleted);
		result.list = (HashMap<String, Object>) list.clone();
		return result;
	}
	
	/**
	 * Duplicate this BeanMap.
	 * 
	 * <p>
	 * Values themselves are not cloned.
	 * 
	 * @return a non null BeanMap.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public BeanMap clone() {
		BeanMap result = duplicate();
		result.setDate(date);
		result.setDeleted(deleted);
		result.list = (HashMap<String, Object>) list.clone();
		return result;
	}
	
	/**
	 * Test if this BeanMap has an ID value.
	 * <p>
	 * <b>WARNING:</b> There is absolutely no guarantees that BeanMap that have a null ID do corresponds to any new
	 * entity.
	 * 
	 * @return true if the id of this BeanMap is null
	 */
	public boolean isNewBeanMap() {
		return id == NEW_BEAN_MAP_ID;
	}

	/**
	 * Load the BeanMap values from the properties of the specified Java Bean.
	 * 
	 * @param <T>
	 * @param bean
	 * @return this BeanMap
	 * @throws IntrospectionException
	 */
	public <T> BeanMap fromBean(T bean) throws IntrospectionException {
		if ((type == null) || (type.length() == 0)) {
			type = bean.getClass().getSimpleName();
		}
		for (PropertyDescriptor property : Introspector.getBeanInfo(bean.getClass(), Object.class)
				.getPropertyDescriptors()) {
			Method read = property.getReadMethod();
			if (read == null) {
				continue;
			}
			if (property.getName().equals(KEY_ID)) {
				try {
					Object o = read.invoke(bean);
					if (o instanceof Integer) {
						id = (Integer) o;
					} else if (o != null) {
						id = Integer.parseInt(o.toString());
					}
				} catch (Throwable e) {
					throw new IntrospectionException(e.getLocalizedMessage());
				}
			} else {
				try {
					put(property.getName(), read.invoke(bean));
				} catch (Throwable e) {
					throw new IntrospectionException(e.getLocalizedMessage());
				}
			}
		}
		return this;
	}

	/**
	 * Load the BeanMap values into the given Java Bean.
	 * 
	 * @param bean
	 *            the java bean to load.
	 * @return the Bean loaded (for chained operations.
	 * @throws IntrospectionException
	 */
	public <T> T toBean(T bean) throws IntrospectionException {
		return toBean(bean, Introspector.getBeanInfo(bean.getClass(), Object.class));
	}

	/**
	 * Load the BeanMap values into the given Java Bean.
	 * 
	 * @param bean
	 *            the java bean to load.
	 * @param beanInfo
	 *            the Bean info of the Java Bean class.
	 * @return the Bean loaded (for chained operations.
	 */
	public <T> T toBean(T bean, BeanInfo beanInfo) {
		HashMap<String, PropertyDescriptor> map = new HashMap<String, PropertyDescriptor>();
		for (PropertyDescriptor property : beanInfo.getPropertyDescriptors()) {
			map.put(property.getName(), property);
		}
		return toBean(bean, map);
	}

	/**
	 * Load the BeanMap values into the given Java Bean.
	 * 
	 * @param bean
	 *            the java bean to load.
	 * @return the Bean loaded (for chained operations.
	 * @param propertiesMap
	 *            the list of properties to load.
	 * @return
	 */
	public <T> T toBean(T bean, HashMap<String, PropertyDescriptor> propertiesMap) {
		PropertyDescriptor property = propertiesMap.get(KEY_ID);
		if (property != null) {
			Method write = property.getWriteMethod();
			if (write != null) {
				try {
					write.invoke(bean, getId());
				} catch (IllegalArgumentException e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				} catch (IllegalAccessException e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				} catch (InvocationTargetException e) {
					// FIXME Journalizer l'erreur quand on est en debug !
				}
			}
		}
		for (Entry<String, Object> e : entrySet()) {
			property = propertiesMap.get(e.getKey());
			if (property != null) {
				Method write = property.getWriteMethod();
				if (write != null) {
					try {
						write.invoke(bean, e.getValue());
					} catch (IllegalArgumentException e1) {
						// FIXME Journalizer l'erreur quand on est en debug !
					} catch (IllegalAccessException e1) {
						// FIXME Journalizer l'erreur quand on est en debug !
					} catch (InvocationTargetException e1) {
						// FIXME Journalizer l'erreur quand on est en debug !
					}
				}
			}
		}
		return bean;
	}
	
	/**
	 * Return an XML representation of this BeanMap.
	 * 
	 * @return
	 */
	public String toXML() {
		return new XmlBeanMapStream().toXML(this);
	}
	
	/**
	 * Return an JSON representation of this BeanMap.
	 * 
	 * @return
	 */
	public String toJSon() {
		return new JSonBeanMapStream().toXML(this);
	}
	
	/**
	 * Returns <tt>true</tt> if this BeanMap contains a mapping for the specified key.
	 * 
	 * @param key
	 *            The key whose presence in this BeanMap is to be tested
	 * @return <tt>true</tt> if this BeanMap contains a mapping for the specified key.
	 */
	public boolean contains(String key) {
		return list.containsKey(key);
	}

	/**
	 * Returns <tt>true</tt> if this BeanMap contains a mapping for the specified key, and if this value is not an empty string nor zero integer.
	 * 
	 * @param key
	 *            The key whose presence in this BeanMap is to be tested
	 * @return <tt>true</tt> if this BeanMap contains a mapping for the specified key.
	 */
	public boolean containsNotEmpty(String key) {
		Object v = list.get(key);
		if (v instanceof Integer) {
			return ((Integer)v).intValue() != 0;
		}
		if (v == null) {
			return false;
		}
		return v.toString().length() > 0;
	}

	/**
	 * Returns <tt>true</tt> if this BeanMap maps one or more keys to the specified value.
	 * 
	 * @param value
	 *            value whose presence in this BeanMap is to be tested
	 * @return <tt>true</tt> if this BeanMap maps one or more keys to the specified value
	 */
	public boolean containsValue(Object value) {
		return list.containsValue(value);
	}

	/**
	 * Returns <tt>true</tt> if this BeanMap contains a mapping for the specified key.
	 * 
	 * @param key
	 *            The key whose presence in this BeanMap is to be tested
	 * @return <tt>true</tt> if this BeanMap contains a mapping for the specified key.
	 */
	public boolean containsKey(Object key) {
		return list.containsKey(key);
	}

	/**
	 * Returns the value to which the specified key is mapped, or {@code null} if this BeanMap contains no mapping for
	 * the key.
	 * 
	 * <p>
	 * More formally, if this BeanMap contains a mapping from a key {@code k} to a value {@code v} such that
	 * {@code (key==null ? k==null :
	 * key.equals(k))}, then this method returns {@code v}; otherwise it returns {@code null}. (There can be at most one
	 * such mapping.)
	 * 
	 * <p>
	 * A return value of {@code null} does not <i>necessarily</i> indicate that the BeanMap contains no mapping for the
	 * key; it's also possible that the BeanMap explicitly maps the key to {@code null}. The {@link #containsKey
	 * containsKey} operation may be used to distinguish these two cases.
	 * 
	 * @see #put(Object, Object)
	 */
	public Object get(Object key) {
		if (key == null) {
			return null;
		}
		String k = key.toString();
		if (KEY_ID.equals(k)) {
			return id;
		}
		if (KEY_TYPE.equals(k)) {
			return type;
		}
		return list.get(key);
	}

	/**
	 * Removes the mapping for the specified key from this BeanMap if present.
	 * 
	 * @param key
	 *            key whose mapping is to be removed from the BeanMap
	 * @return the previous value associated with <tt>key</tt>, or <tt>null</tt> if there was no mapping for
	 *         <tt>key</tt>. (A <tt>null</tt> return can also indicate that the BeanMap previously associated
	 *         <tt>null</tt> with <tt>key</tt>.)
	 */
	public Object remove(Object key) {
		return list.remove(key);
	}

	/**
	 * Copies all of the mappings from the specified map to this BeanMap. These mappings will replace any mappings that
	 * this BeanMap had for any of the keys currently in the specified map.
	 * 
	 * @param m
	 *            mappings to be stored in this BeanMap
	 * @throws NullPointerException
	 *             if the specified map is null
	 */
	public void putAll(Map<? extends String, ? extends Object> m) {
		list.putAll(m);
	}

	/**
	 * Copies all of the mappings from the specified map to this BeanMap and prefix the keys with the given prefix.
	 * These mappings will replace any mappings that this BeanMap had for any of the keys currently in the specified map.
	 *
	 * @param prefix The string to add before all keys.
	 * @param m
	 *            mappings to be stored in this BeanMap
	 * @throws NullPointerException
	 *             if the specified map is null
	 */
	public void putAll(String prefix, Map<? extends String, ? extends Object> m) {
		if ((prefix == null) || (prefix.length() == 0)) {
			list.putAll(m);
		} else if (m != null) {
			for(Entry<? extends String, ? extends Object> e: m.entrySet()) {
				list.put(prefix + e.getKey(), e.getValue());
			}
		}
	}

	/**
	 * Returns a {@link Set} view of the keys contained in this BeanMap. The set is backed by the BeanMap, so changes to
	 * the BeanMap are reflected in the set, and vice-versa. If the BeanMap is modified while an iteration over the set
	 * is in progress (except through the iterator's own <tt>remove</tt> operation), the results of the iteration are
	 * undefined. The set supports element removal, which removes the corresponding mapping from the BeanMap, via the
	 * <tt>Iterator.remove</tt>, <tt>Set.remove</tt>, <tt>removeAll</tt>, <tt>retainAll</tt>, and <tt>clear</tt>
	 * operations. It does not support the <tt>add</tt> or <tt>addAll</tt> operations.
	 */
	public Set<String> keySet() {
		return list.keySet();
	}

	/**
	 * Returns a {@link Collection} view of the values contained in this BeanMap. The collection is backed by the
	 * BeanMap, so changes are reflected in the collection, and vice-versa. If the BeanMap is modified while an
	 * iteration over the collection is in progress (except through the iterator's own <tt>remove</tt> operation), the
	 * results of the iteration are undefined. The collection supports element removal, which removes the corresponding
	 * mapping from the BeanMap, via the <tt>Iterator.remove</tt>, <tt>Collection.remove</tt>, <tt>removeAll</tt>,
	 * <tt>retainAll</tt> and <tt>clear</tt> operations. It does not support the <tt>add</tt> or <tt>addAll</tt>
	 * operations.
	 * @return A collection of Object.
	 */
	public Collection<Object> values() {
		return list.values();
	}
	
	/**
	 * Returns a {@link Collection} view of the values contained in this BeanMap according to the given list of keys.
	 * The collection is <b>not</b> backed by the BeanMap, the list may contain null objects if the key does not
	 * correspond to any value into this BeanMap.
	 * 
	 * @param keys list of keys separated with spaces. 
	 * @return A collection of Object.
	 */
	public Collection<Object> values(String keys) {
		ArrayList<Object> result = new ArrayList<Object>();
		if (keys == null) {
			return result;
		}
		for(String s:keys.split(" "))  { //$NON-NLS-1$
			result.add(list.get(s));
		}
		return result;
	}
	
	/**
	 * Compare the specified value to the same value into the given BeanMap.
	 * 
	 * @param bean the BeanMap to be compared to.
	 * @param code the key value to test.
     * @return  a negative integer, zero, or a positive integer as this BeanMap value
     *		is less than, equal to, or greater than the specified BeanMap value.
	 */
	@SuppressWarnings("unchecked")
	public int compareTo(BeanMap bean, String code) {
		final Object o = get(code);
		final Object o2 = bean.get(code);
		if (o == null) {
			if (o2 == null) {
				return 0;
			}
			return -1;
		}
		if (o2 == null) {
			return 1;
		}
		// Try to use the comparable implementation.
		if (o instanceof Comparable<?>) {
			try {
				return ((Comparable<Object>) o).compareTo(o2);
			} catch (Exception e) {}
		}
		// if the direct comparison fail
		try {
			int i = getInt(code);
			int i2 = bean.getInt(code);
			if ((i != -1) && (i2 != -1)) {
				return i - i2;
			}
		} catch (NumberFormatException e) {}
		try {
			float f = getFloat(code);
			float f2 = bean.getFloat(code);
			if ((f != -1F) && (f2 != -1F)) {
				return Float.compare(f, f2);
			}
		} catch (NumberFormatException e) {}
		return o.toString().compareTo(o2.toString());
	}

	/**
	 * Compare the specified value to the same value into the given BeanMap, ignoring case in case of String comparison.
	 * 
	 * @param bean the BeanMap to be compared to.
	 * @param code the key value to test.
     * @return  a negative integer, zero, or a positive integer as this BeanMap value
     *		is less than, equal to, or greater than the specified BeanMap value.
	 */
	@SuppressWarnings("unchecked")
	public int compareICTo(BeanMap bean, String code) {
		final Object o = get(code);
		final Object o2 = bean.get(code);
		if (o == null) {
			if (o2 == null) {
				return 0;
			}
			return -1;
		}
		if (o2 == null) {
			return 1;
		}
		// Try to use the comparable implementation.
		if (o instanceof String) {
			if (o2 instanceof String) {
				return ((String) o).compareToIgnoreCase((String) o2);
			}
			return ((String) o).compareToIgnoreCase(o2.toString());
		} else if (o2 instanceof String) {
			return o.toString().compareToIgnoreCase((String) o2);
		}
		if (o instanceof Comparable<?>) {
			try {
				return ((Comparable<Object>) o).compareTo(o2);
			} catch (Exception e) {}
		}
		// if the direct comparison fail
		try {
			int i = getInt(code);
			int i2 = bean.getInt(code);
			if ((i != -1) && (i2 != -1)) {
				return i - i2;
			}
		} catch (NumberFormatException e) {}
		try {
			float f = getFloat(code);
			float f2 = bean.getFloat(code);
			if ((f != -1F) && (f2 != -1F)) {
				return Float.compare(f, f2);
			}
		} catch (NumberFormatException e) {}
		return o.toString().compareToIgnoreCase(o2.toString());
	}

    /**
     * Compares this BeanMap with the specified BeanMap for order.  Returns a
     * negative integer, zero, or a positive integer as this object is less
     * than, equal to, or greater than the specified object.
     *
     * @param   bean the BeanMap to be compared.
     * @return  a negative integer, zero, or a positive integer as this object
     *		is less than, equal to, or greater than the specified object.
     *
     * @throws ClassCastException if the specified object's type prevents it
     *         from being compared to this object.
     */
	public int compareTo(IBeanMap bean) {
		int i = type.compareTo(bean.getType());
		if (i != 0) {
			return i;
		}
		return id - bean.getId();
	}

	/**
	 * Remove the given key and replace it by the new one. the value is preserved.
	 * 
	 * @param oldKey
	 * @param newKey
	 */
	public void replace(String oldKey, String newKey) {
		Object value = list.remove(oldKey);
		if (value != null) {
			list.put(newKey, value);
		}
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}