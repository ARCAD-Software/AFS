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
package com.arcadsoftware.beanmap;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.eclipse.core.runtime.IAdaptable;

import com.arcadsoftware.beanmap.internal.BeanMapFromJSON;
import com.arcadsoftware.beanmap.internal.BeanMapIterator;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;

/**
 * Default container associated with BeanMap.
 * 
 * <p>
 * This implementation extends an <code>ArrayList&lt;BeanMap&gt;</code>.
 */
public class BeanMapList extends ArrayList<BeanMap> implements IDatedBean, Cloneable {

	/**
	 * Slash character ('/').
	 */
	public static final char SLASH_TYPE = '/';

	/**
	 * Slash Character representation into XML document (actually equal to '-').
	 */
	public static final char SLASH_TAG = '-';

	private static final long serialVersionUID = 8051313140936350926L;

	private Date date;
	private int muid;

	/**
	 * Construct a pseudo list type that can be used to determine the resource base uri from a specific BeanMap type.
	 * 
	 * @see com.arcadsoftware.utils.BeanMapList#getBeanType(String)
	 * @param type
	 *            The type is a string key that identify an Entity uniquely
	 * @return
	 */
	public static String getListTag(String type) {
		type = type.replace(SLASH_TYPE, SLASH_TAG);
		if (type.endsWith("y")) { //$NON-NLS-1$
			return type.substring(0, type.length() - 1) + "ies"; //$NON-NLS-1$
		}
		if (type.endsWith("s")) { //$NON-NLS-1$
			return type + "list"; //$NON-NLS-1$
		}
		if (type.endsWith("item")) { //$NON-NLS-1$
			return type.substring(0, type.length() - 4);
		}
		return type + "s"; //$NON-NLS-1$
	}

	/**
	 * Retrieve the BeanMap type associated to a pseudo list type. This pseudo list type is assumed to be constructed
	 * with the <code>getListType</code> static method.
	 * 
	 * @see #getListTag(java.lang.String)
	 * @param listType
	 * @return
	 */
	public static String getType(String listType) {
		listType = listType.replace(SLASH_TAG, SLASH_TYPE);
		if (listType.endsWith("ies")) { //$NON-NLS-1$
			return listType.substring(0, listType.length() - 3) + "y"; //$NON-NLS-1$
		}
		if (listType.endsWith("s")) { //$NON-NLS-1$
			return listType.substring(0, listType.length() - 1);
		}
		if (listType.endsWith("list")) { //$NON-NLS-1$
			return listType.substring(0, listType.length() - 4);
		}
		return listType + "item"; //$NON-NLS-1$
	}
	
	/**
	 * Helper method.
	 * 
	 * Load a BeanMap list from an XML fragment string.
	 * 
	 * @param type
	 *            the desired type of the BeanMap, may be null.
	 * @param xml
	 *            the XML fragment.
	 * @return null if the XML fragment does not conform to the expect format.
	 */
	static public BeanMapList loadFromXml(String type, String xml) {
		Object result = null;
		try {
			if (type != null) {
				result = new XmlBeanMapStream().fromXML(type, xml);
			} else {
				result = new XmlBeanMapStream().fromXML(xml);
			}
		} catch (Exception e) {
			return null;
		}
		if (result instanceof BeanMapList) {
			return (BeanMapList) result;
		}
		if (result instanceof IAdaptable) {
			result = ((IAdaptable) result).getAdapter(BeanMapList.class);
			if (result instanceof BeanMapList) {
				return (BeanMapList) result;
			}
		}
		if (result instanceof List<?>) {
			BeanMapList list = new BeanMapList(((List<?>) result).size());
			for (Object o: (List<?>) result) {
				if (o instanceof BeanMap) {
					list.add((BeanMap) o);
				} else {
					try {
						list.add(new BeanMap(o));
					} catch (IntrospectionException e) {}
				}
			}
			return list;
		}
		return null;
	}
	
	/**
	 * Helper method.
	 * 
	 * Load a BeanMap list from an XML fragment string.
	 * 
	 * @param xml
	 *            the XML fragment.
	 * @return null if the XML fragment does not conform to the expect format.
	 */
	static public BeanMapList loadFromXml(String xml) {
		return loadFromXml(null, xml);
	}
	
	/**
	 * Helper method.
	 * 
	 * Load a BeanMap list from an JSON document string.
	 * 
	 * @param type
	 *            the desired type of the BeanMap, may be null.
	 * @param json
	 *            the JSON document.
	 * @return null if the JSon document does not conform to the expect format.
	 * @deprecated use {@link #loadFromJSon(String)}
	 */
	static public BeanMapList loadFromJSon(String type, String json) {
		return loadFromJSon(json);
	}
	
	/**
	 * Helper method.
	 * 
	 * Load a BeanMap list from an JSON document string.
	 * 
	 * @param json
	 *            the JSON document.
	 * @return null if the JSon document does not conform to the expect format.
	 */
	static public BeanMapList loadFromJSon(String json) {
		try {
			JSONObject o = new JSONObject(json);
			Iterator<?> itt = o.keys();
			if (!itt.hasNext()) {
				return null;
			}
			String listKey = (String) itt.next();
			JSONObject list = o.getJSONObject(listKey);
			int count = 10;
			try {
				count = list.getInt("count"); //$NON-NLS-1$
			} catch (JSONException e) {}
			BeanMapList result = new BeanMapList(count);
			JSONArray items = list.getJSONArray("items"); //$NON-NLS-1$
			for (int i = 0; i < items.length(); i++) {
				result.add(BeanMapFromJSON.parse(new BeanMap(), items.getJSONObject(i)));
			}
			return result;
		} catch (JSONException e) {
			return null;
		}
	}

	/**
	 * Construct an empty list with an initial capacity of 12.
	 */
	public BeanMapList() {
		super();
	}

	/**
	 * Construct a dated list.
	 */
	public BeanMapList(Date date) {
		super();
		this.date = date;
	}

	/**
	 * Construct a list with only one element.
	 * 
	 * @param firstValue
	 *            the first element of the list.
	 */
	public BeanMapList(BeanMap firstValue) {
		super(1);
		add(firstValue);
	}

	/**
	 * Construct a list from an array of BeanMap.
	 * 
	 * @param items
	 */
	public BeanMapList(BeanMap[] items) {
		super(items.length);
		for (BeanMap bm : items) {
			add(bm);
		}
	}

	/**
	 * Construct an empty list with the specified capacity.
	 * 
	 * @param initialCapacity
	 */
	public BeanMapList(int initialCapacity) {
		super(initialCapacity);
	}

	/**
	 * Constructs a list containing the elements of the specified collection, in the order they are returned by the
	 * collection's iterator. The <tt>ArrayList</tt> instance has an initial capacity of 110% the size of the specified
	 * collection.
	 * 
	 * @param c
	 *            the collection whose elements are to be placed into this list.
	 * @throws NullPointerException
	 *             if the specified collection is null.
	 */
	public BeanMapList(Collection<? extends BeanMap> c) {
		super(c);
	}

	/**
	 * Add the given beanmap to the list only if it does not already exist into the list.
	 * 
	 * @param bean
	 */
	public void addUnique(BeanMap bean) {
		if ((bean != null) && (find(bean.getId()) == null)) {
			add(bean);
		}
	}

	/**
	 * Search the first bean corresponding to the given id.
	 * 
	 * @param id
	 *            the bean corresponding to this Id.
	 * @return the BeanMap
	 */
	public BeanMap find(int id) {
		for (BeanMap bm : this) {
			if ((bm != null) && (bm.getId() == id)) {
				return bm;
			}
		}
		return null;
	}

	/**
	 * Search the first bean corresponding to the given id.
	 * 
	 * @param id
	 *            the bean corresponding to this Id.
	 * @return the BeanMap index in the list, -1 if not found.
	 */
	public int findIndex(int id) {
		for (int i = size() - 1; i >= 0; i--) {
			BeanMap bm = get(i);
			if ((bm != null) && (bm.getId() == id)) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Search the first bean corresponding to the given id and type.
	 * 
	 * @param id
	 *            the bean Id.
	 * @param type
	 *            the bean type (can not be null).
	 * @return
	 */
	public BeanMap find(int id, String type) {
		if (type == null) {
			return null;
		}
		for (BeanMap bm : this) {
			if ((bm != null) && (bm.getId() == id) && (type.equals(bm.getType()))) {
				return bm;
			}
		}
		return null;
	}

	/**
	 * Return the sub list of BeanMap with matching codes.
	 * 
	 * @param type
	 *            a non null type string.
	 * @return a sub list, can be empty, but never null.
	 */
	public BeanMapList find(String type) {
		BeanMapList result = new BeanMapList(1);
		if (type == null) {
			return result;
		}
		for (BeanMap bm : this) {
			if ((bm != null) && type.equals(bm.getType())) {
				result.add(bm);
			}
		}
		return result;
	}
	
	/**
	 * Filter the list, return only the BeanMap with a Modification User ID (MUID) equal to the given one.
	 * 
	 * @param uid A negative or null value return all BeanMap without a MUID.
	 * @return
	 */
	public BeanMapList findMUID(int uid) {
		BeanMapList result = new BeanMapList(1);
		if (uid <= 0)  {
			for (BeanMap bm : this) {
				if ((bm != null) && (bm.getMUID() < 0)) {
					result.add(bm);
				}
			}
		} else {
			for (BeanMap bm : this) {
				if ((bm != null) && (bm.getMUID() == uid)) {
					result.add(bm);
				}
			}
		}
		return result;
	}

	/**
	 * Return the first BeanMap of the given type from this list.
	 * 
	 * @param type
	 * @return
	 */
	public BeanMap findFirst(String type) {
		if (type != null) {
			for (BeanMap bm : this) {
				if ((bm != null) && type.equals(bm.getType())) {
					return bm;
				}
			}
		}
		return null;
	}
	
	/**
	 * Return the set of types used in this list.
	 * 
	 * @return a set of type, may be empty if BeanMap have no type, but must not be null.
	 */
	public Set<String> getTypes() {
		HashSet<String> result = new HashSet<String>(size() / 10);
		for (BeanMap bm : this) {
			if (bm != null) {
				String type = bm.getType();
				result.add(type);
			}
		}
		return result;
	}

	/**
	 * Get the contained BeanMap with the more recent date.
	 * 
	 * @return null if there is no BeanMap with a date in this list.
	 */
	public BeanMap getMostRecent() {
		BeanMap result = null;
		for (BeanMap b: this) {
			Date d = b.getDate();
			if ((d != null) && ((result == null) || d.after(result.getDate()))) {
				result = b;
			}
		}
		return result;
	}
	
	/**
	 * <p>
	 * Dated list can be cached. This date is supposed to be last modification date of the items of this list. This
	 * suppose, last Item addition, remove, edition onto the server side.
	 * 
	 * <p>
	 * Direct modifications of this list do not affect this date.
	 * 
	 * @return the last modification date of this list.
	 */
	@Override
	public Date getDate() {
		return date;
	}

	/**
	 * Get the modification date of this list, this value is the date of the list, or the maximal value of the date of the contained BeanMap.
	 * 
	 * <p>
	 * Direct modifications of this list may affect this date.
	 * 
	 * @return null if nothing possess a Date in this list.
	 * @see #getDate()
	 */
	public Date getRealDate() {
		Date result = date;
		for (BeanMap b: this) {
			if (result == null) {
				result = b.getDate();
			} else {
				Date d = b.getDate();
				if ((d != null) && d.after(result)) {
					result = d;
				}
			}
		}
		return result;
	}
	
	/**
	 * Set the last modification date of the list.
	 * 
	 * <p>
	 * Consumer should not modify this date as it may affect the Cache system.
	 * 
	 * @param date
	 */
	@Override
	public void setDate(Date date) {
		this.date = date;
	}

	@Override
	public int getMUID() {
		return muid;
	}

	@Override
	public void setMUID(int id) {
		muid = id;
	}

	@Override
	public void setModification(int uid, Date date) {
		setMUID(uid);
		setDate(date);
	}

	/**
	 * This method search for the BeanMap corresponding with the given Id and then return the value of the given
	 * attribute.
	 * 
	 * @param id
	 * @param attribute
	 * @return
	 */
	public Object findValue(int id, String attribute) {
		BeanMap result = find(id);
		if (result != null) {
			return result.get(attribute);
		}
		return null;
	}

	/**
	 * Get the first BeanMap that match the given source BeanMap attributes.
	 * 
	 * @param keys the Attributes keys to test.
	 * @param source the model that possess the attributes to test.
	 * @return
	 */
	public BeanMap find(String[] keys, BeanMap source) {
		for(BeanMap b:this) {
			boolean match = true;
			for(String key:keys) {
				Object v = source.get(key);
				if (v == null) {
					if (b.get(key) != null) {
						match = false;
						break;
					}
				} else if (!v.equals(b.get(key))) {
					match = false;
					break;
				}
			}
			if (match) {
				return b;
			}
		}
		return null;
	}

	/**
	 * Search for a BeanMap and remove it from the list.
	 * 
	 * @param keys the Attributes keys to test.
	 * @param source the model that possess the attributes to test.
	 * @return
	 * @see #find(String[], BeanMap)
	 */
	public BeanMap remove(String[] keys, BeanMap source) {
		for(int i = size() - 1; i >= 0; i--) {
			BeanMap b = get(i);
			boolean match = true;
			for(String key:keys) {
				Object v = source.get(key);
				if (v == null) {
					if (b.get(key) != null) {
						match = false;
						break;
					}
				} else if (!v.equals(b.get(key))) {
					match = false;
					break;
				}
			}
			if (match) {
				remove(i);
				return b;
			}
		}
		return null;
	}

	/**
	 * <p>
	 * Look for the first BeanMap from the list which possess the given attribute and equal to the given value.
	 * </p>
	 * 
	 * <p>
	 * If the Value is null then this method return the first BeanMap that do not possess the given attribute.
	 * </p>
	 * 
	 * @param attribute
	 *            and attribute key.
	 * @param value
	 *            the desired value, can be null.
	 * @return A BeanMap, null if none.
	 */
	public BeanMap getFirst(String attribute, Object value) {
		if (value == null) {
			for (BeanMap bm : this) {
				if ((bm != null) && (bm.get(attribute) == null)) {
					return bm;
				}
			}
		} else {
			for (BeanMap bm : this) {
				if ((bm != null) && value.equals(bm.get(attribute))) {
					return bm;
				}
			}
		}
		return null;
	}

	/**
	 * <p>
	 * Look for the first BeanMap from the list which possess the given attribute and 
	 * equal to the given value and remove it from the list.
	 * </p>
	 * 
	 * <p>
	 * If the Value is null then this method return the first BeanMap that do not possess the given attribute.
	 * </p>
	 * 
	 * @param attribute
	 *            and attribute key.
	 * @param value
	 *            the desired value, can be null.
	 * @return A BeanMap, null if none.
	 */
	public BeanMap removeFirst(String attribute, Object value) {
		Iterator<BeanMap> itt = iterator();
		if (value == null) {
			while (itt.hasNext()) {
				BeanMap bm = itt.next();
				if ((bm != null) && (bm.get(attribute) == null)) {
					itt.remove();
					return bm;
				}
			}
		} else {
			while (itt.hasNext()) {
				BeanMap bm = itt.next();
				if ((bm != null) && value.equals(bm.get(attribute))) {
					itt.remove();
					return bm;
				}
			}			
		}
		return null;
	}

	/**
	 * <p>
	 * Look for the first BeanMap from the list which possess the given attribute and equal to the given value.
	 * </p>
	 * 
	 * <p>
	 * If the Value is null then this method return the first BeanMap that do not possess the given attribute.
	 * </p>
	 * 
	 * @param attribute
	 *            and attribute key.
	 * @param value
	 *            the desired value, can be null.
	 * @return an index, -1 if nothing match.
	 */
	public int getFirstIndex(String attribute, Object value) {
		int i = 0;
		if (value == null) {
			for (BeanMap bm : this) {
				if ((bm != null) && (bm.get(attribute) == null)) {
					return i;
				}
				i++;
			}
		} else {
			for (BeanMap bm : this) {
				if ((bm != null) && value.equals(bm.get(attribute))) {
					return i;
				}
				i++;
			}
		}
		return -1;
	}

	/**
	 * Get the first BeanMap from the list that possess the given ID, return null if none.
	 * @param id A BeanMap ID, generally a positive value.
	 * @return a BeanMap from the list or null.
	 */
	public BeanMap getFirst(int id) {
		for (BeanMap bm: this) {
			if ((bm != null) && (bm.getId() == id)) {
				return bm;
			}
		}
		return null;
	}

	/**
	 * Get all the BeanMap with the given type.
	 * 
	 * @param type
	 * @return never return null.
	 */
	public List<BeanMap> get(String type) {
		ArrayList<BeanMap> result = new ArrayList<BeanMap>(size());
		if (type == null) {
			for(BeanMap b: this) {
				if (b.getType() == null) {
					result.add(b);
				}
			}
		} else {
			for(BeanMap b: this) {
				if (type.equals(b.getType())) {
					result.add(b);
				}
			}
		}
		return result;
	}

	/**
	 * Get a new list containing all the BeanMap with the given attribute value.
	 * 
	 * @param attribute
	 *            and attribute key.
	 * @param value
	 *            the desired value, can be null.
	 * @return a non null BeanMapList.
	 */
	public BeanMapList get(String attribute, Object value) {
		BeanMapList result = new BeanMapList(size());
		if (value == null) {
			for (BeanMap bm : this) {
				if ((bm != null) && (bm.get(attribute) == null)) {
					result.add(bm);
				}
			}
		} else {
			for (BeanMap bm : this) {
				if ((bm != null) && value.equals(bm.get(attribute))) {
					result.add(bm);
				}
			}
		}
		return result;
	}
	
	/**
	 * <p>
	 * Returns an iterator over the elements in this list in proper sequence.
	 * </p>
	 * 
	 * <p>
	 * This Iterator will only select the desired BeanMap that have an <code>attribute</code> equal to the specified
	 * <code>value</code>.
	 * </p>
	 * 
	 * <p>
	 * If the Value is null then the selected BeanMap will not possess the given attribute.
	 * </p>
	 * 
	 * @param attribute
	 *            and attribute key.
	 * @param value
	 *            the desired value, can be null.
	 * @return an iterator over the elements in this list in proper sequence.
	 */
	public Iterator<BeanMap> iterator(String attribute, Object value) {
		return new BeanMapIterator(super.iterator(), attribute, value);
	}

	/**
	 * Construct a List of values extracted from the BeanMap list.
	 * 
	 * @param <T>
	 *            any type (converted types are String, Integer, Date, Boolean, Float).
	 * @param attribute
	 *            the attribute code.
	 * @param clazz
	 *            The desired class
	 * @return a List of <code>T</code> Objects.
	 */
	public <T> ArrayList<T> getValues(String attribute, Class<T> clazz) {
		ArrayList<T> result = new ArrayList<T>();
		for (BeanMap bm : this) {
			if (bm != null) {
				T o = bm.get(attribute, clazz);
				if (o != null) {
					result.add(o);
				}
			}
		}
		return result;
	}

	@Override
	public boolean moreRecent(IDatedBean bm) {
		return (date != null) && date.after(bm.getDate());
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("Size="); //$NON-NLS-1$
		sb.append(size());
		sb.append(' ');
		sb.append(super.toString());
		return sb.toString();
	}

	public String prettyPrint() {
		StringBuilder sb = new StringBuilder("BeanMap List (Size="); //$NON-NLS-1$
		sb.append(size());
		sb.append(") [\n"); //$NON-NLS-1$
		for(BeanMap b: this) {
			sb.append(' ');
			sb.append(b.prettyprint(2));
			sb.append("\n"); //$NON-NLS-1$
		}
		sb.append(']');
		return sb.toString();
	}
	
	@Override
	public BeanMap[] toArray() {
		return toArray(new BeanMap[0]);
	}

	/**
	 * Load the BeanMap for this list into an list of Java Bean.
	 * 
	 * @param <T> The Java Bean type.
	 * @param list The result list, to be loaded (if null an ArrayList is returned).
	 * @param clazz the class of the Java Bean to create.
	 * @param constructorParameters the Constructor parameters (fixed for all Beans). 
	 * @return the list with the bean corresponding to the BeanMaps (added in the same order).
	 * @throws IntrospectionException thrown if an introspection error is raised
	 */
	public <T> Collection<T> toList(Collection<T> list, Class<T> clazz, Object... constructorParameters) throws IntrospectionException {
		if (list == null) {
			list = new ArrayList<T>();
		}
		BeanInfo beanInfo = Introspector.getBeanInfo(clazz, Object.class);
       	HashMap<String,PropertyDescriptor> propertiesMap = new HashMap<String, PropertyDescriptor>();
       	for (PropertyDescriptor property: beanInfo.getPropertyDescriptors()) {
       		propertiesMap.put(property.getName(), property);
       	}
		Constructor<?> constructor = null;
        for (Constructor<?> c: clazz.getConstructors()) {
        	Class<?>[] parameterTypes = c.getParameterTypes();
            if ((parameterTypes.length == constructorParameters.length) && Modifier.isPublic(c.getModifiers())) {
            	boolean sametypes = true;
            	for (int i = 0; i < parameterTypes.length; i++) {
            		if (!parameterTypes[i].isAssignableFrom(constructorParameters[i].getClass())) {
            			sametypes = false;
            			break;
            		}
            	}
            	if (sametypes) {
            		constructor = c;
            	}
            }
        }
        if (constructor == null) {
        	throw new IntrospectionException("Constructor parameters do not match any constructor from the given class.");
        }
        for (BeanMap bm: this) {
        	try {
        		Object o = constructor.newInstance(constructorParameters);
        		if (clazz.isInstance(o)) {
        			list.add(bm.toBean(clazz.cast(o), propertiesMap));
        		}
			} catch (IllegalArgumentException | InstantiationException | IllegalAccessException  | InvocationTargetException e) {}
        }
		return list;
	}
	
	/**
	 * Return an XML representation of this list.
	 * 
	 * @return
	 */
	public String toXML() {
		return new XmlBeanMapStream().toXML(this);
	}
	
	/**
	 * Return an JSON representation of this list.
	 * 
	 * @return
	 */
	public String toJSon() {
		return new JSonBeanMapStream().toXML(this);
	}
	
	/**
	 * Duplicate this list and clone all member of it.
	 * 
	 * @return an non null beanMap list.
	 * @see #clone()
	 */
	public BeanMapList cloneBeanMaps() {
		BeanMapList result = new BeanMapList(size());
		for (BeanMap bean:this) {
			if (bean != null) {
				result.add((BeanMap)bean.clone());
			}
		}
		return result;
	}

	/**
	 * Replace the given key into all contained beanmap
	 * @param oldKey
	 * @param newKey
	 */
	public void replaceKey(String oldKey, String newKey) {
		for(BeanMap b: this) {
			b.replace(oldKey, newKey);
		}
	}

	/**
	 * Get a string from all the values of the given key into the list.
	 * 
	 * @param key
	 * @param separator
	 * @return
	 */
	public Object getString(String key, char separator) {
		StringBuilder sb = new StringBuilder();
		boolean first = true;
		for(BeanMap b: this) {
			String s = b.getString(key);
			if (s != null) {
				if (first) {
					first = false;
				} else {
					sb.append(separator);
				}
				sb.append(s);
			}
		}
		return sb.toString();
	}

	/**
	 * Set the specified key to all members of the list.
	 * 
	 * @param key
	 * @param value
	 */
	public void putAll(String key, Object value) {
		for(BeanMap bean: this) {
			if (bean != null) {
				bean.put(key, value);
			}
		}
	}
	
	/**
	 * Sort the list according to the given key values.
	 * 
	 * @param key a list of key to test.
	 * @see BeanMapComparator
	 */
	public void sort(String... key) {
		Collections.sort(this, new BeanMapComparator(key));
	}
	
	/**
	 * Sort the list according to the given key values, ignoring case for String values.
	 * 
	 * @param key a list of key to test.
	 * @see BeanMapComparator
	 */
	public void sortIC(String... key) {
		Collections.sort(this, new BeanMapComparator(key) {
			@Override
			protected int compareTo(BeanMap data1, BeanMap data2, String key) {
				return data1.compareICTo(data2, key);
			}
		});
	}

	/**
	 * Sort the BeanMap of this list in the ascendant (if "up" is true) or the descendant order of their modification dates.
	 * 
	 * @param up If true the the result is order in the ascendant order.
	 */
	public void sortByDate(final boolean up) {
		final Comparator<BeanMap> comparator;
		if (up) {
			comparator = new Comparator<BeanMap>() {
				@Override
				public int compare(BeanMap o1, BeanMap o2) {
					Date d1 = o1.getDate();
					Date d2 = o2.getDate();
					if (d1 == null) {
						if (d2 == null) {
							return 0;
						}
						return -1;
					} else if (d2 == null) {
						return 1;
					}
					return d1.compareTo(d2);
				}
			};
		} else {
			comparator = new Comparator<BeanMap>() {
				@Override
				public int compare(BeanMap o2, BeanMap o1) {
					Date d1 = o1.getDate();
					Date d2 = o2.getDate();
					if (d1 == null) {
						if (d2 == null) {
							return 0;
						}
						return -1;
					} else if (d2 == null) {
						return 1;
					}
					return d1.compareTo(d2);
				}
			};
		}
		Collections.sort(this, comparator);
	}
	
	@Override
	public BeanMapList clone() {
		BeanMapList result = new BeanMapList(this);
		result.date = date;
		return result;
	}
	
}