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
package com.arcadsoftware.beanmap.internal.xml;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map.Entry;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.ITransientAttribute;
import com.arcadsoftware.osgi.ISODateFormater;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriter;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

@SuppressWarnings("deprecation")
public class BeanMapConverter implements Converter {

	private static final String KEY_LIST = "list"; //$NON-NLS-1$

	@SuppressWarnings("unused")
	private final Mapper mapper;
	private boolean writeType;
	private BeanMapConverter thisConverter;
	private BeanMapListConverter listConverter;
	private boolean addNulls;
	private final boolean useAttributes;

	public BeanMapConverter(Mapper mapper) {
		this(mapper, false, true);
	}

	public BeanMapConverter(Mapper mapper, boolean writeType, boolean useAttributes) {
		super();
		this.mapper = mapper;
		this.writeType = writeType;
		this.useAttributes = useAttributes;
		thisConverter = this;
	}

	@SuppressWarnings("rawtypes")
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		int id = ((BeanMap) source).getId();
		String type = ((BeanMap) source).getType();
		Date date = ((BeanMap) source).getDate();
		if (!useAttributes && addNulls) {
			for (Entry<String, Object> entry : ((BeanMap) source).entrySet()) {
				if (entry.getValue() == null) {
					// For JSON generate key:null
					writer.addAttribute(entry.getKey().replace('.', '_'), null);
				}
			}
		}
		if (id != 0) {
			if (useAttributes) {
				writer.addAttribute(BeanMap.KEY_ID, String.valueOf(id));
			} else {
				((ExtendedHierarchicalStreamWriter) writer).startNode(BeanMap.KEY_ID, Integer.class);
				context.convertAnother(id);
				writer.endNode();
			}
		}
		if (writeType && (type != null)) {
			if (useAttributes) {
				writer.addAttribute(BeanMap.KEY_TYPE, type);
			} else {
				((ExtendedHierarchicalStreamWriter) writer).startNode(BeanMap.KEY_TYPE, String.class);
				context.convertAnother(type);
				writer.endNode();
			}
		}
		if (date != null) {
			if (useAttributes) {
				writer.addAttribute(BeanMap.KEY_DATE, ISODateFormater.toString(date));
			} else {
				((ExtendedHierarchicalStreamWriter) writer).startNode(BeanMap.KEY_DATE, String.class);
				context.convertAnother(ISODateFormater.toString(date));
				writer.endNode();
			}
		}
		if (((BeanMap)source).isDeleted()) {
			if (useAttributes) {
				writer.addAttribute(BeanMap.KEY_DELETED, "true"); //$NON-NLS-1$
			} else {
				((ExtendedHierarchicalStreamWriter) writer).startNode(BeanMap.KEY_DATE, Boolean.class);
				context.convertAnother(Boolean.TRUE);
				writer.endNode();
			}
		}
		// TODO Passer en revue les attributes de valeur simple (Boolean, Integer);
		ArrayList<Entry<String, Object>> list = new ArrayList<Entry<String, Object>>();
		// Ajouter sous la forme d'attributs XML.
		for (Entry<String, Object> entry : ((BeanMap) source).entrySet()) {
			Object value = entry.getValue();
			String key = entry.getKey().replace('.', '_');
			if (value == null) {
				if (addNulls && useAttributes) {
					// For JSON generate key:null
					writer.addAttribute(key, null);
				}
			} else if (!(value instanceof ITransientAttribute)) {
				if (useAttributes && //
						!(BeanMap.KEY_TYPE.equals(key) || //
								BeanMap.KEY_DATE.equals(key) || //
								BeanMap.KEY_ID.equals(key) || //
								BeanMap.KEY_DELETED.equals(key))) {
					if (value instanceof Integer) {
						writer.addAttribute(key, value.toString());
					} else if (value instanceof Boolean) {
						if (((Boolean) value).booleanValue()) {
							writer.addAttribute(key, "true"); //$NON-NLS-1$
						} else {
							writer.addAttribute(key, "false"); //$NON-NLS-1$
						}
					} else if (value instanceof Date) {
						writer.addAttribute(key, ISODateFormater.toString((Date) value));
					} else {
						list.add(entry);
					}
				} else {
					list.add(entry);
				}
			}
		}
		if (addNulls) {
			// Only add nulls to the first BeanMap of a list (to allow to estimate the columns number.
			addNulls = false;
		}
		// puis seulement après enregistrer les sous-tags
		for (Entry<String, Object> entry : list) {
			Object value = entry.getValue();
			if (!(value instanceof ITransientAttribute)) {
				String key = entry.getKey().replace('.', '_');
				((ExtendedHierarchicalStreamWriter) writer).startNode(key, value.getClass());
				if (value instanceof BeanMap) {
					type = ((BeanMap) value).getType();
					if ((!writeType) && (type != null) && (!type.equals(key))) {
						if (useAttributes) {
							writer.addAttribute(BeanMap.KEY_TYPE, type);
						} else {
							((ExtendedHierarchicalStreamWriter) writer).startNode(BeanMap.KEY_TYPE, String.class);
							context.convertAnother(type);
							writer.endNode();
						}
					}
					context.convertAnother(value, thisConverter);
				} else if (value instanceof BeanMapList) {
					if (listConverter != null) {
						context.convertAnother(value, listConverter);
					} else {
						context.convertAnother(value);
					}
				} else if (value instanceof Collection) {
					if (useAttributes) {
						writer.addAttribute(KEY_LIST, Integer.toString(((Collection) value).size()));
					} else {
						writer.startNode(KEY_LIST);
						context.convertAnother(((Collection) value).size());
						writer.endNode();
					}
					context.convertAnother(value);
				} else {
					context.convertAnother(value);
				}
				writer.endNode();
			}
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		// Création BeanMap
		String type = reader.getAttribute(BeanMap.KEY_TYPE);
		if (type == null) {
			type = reader.getNodeName().replace(BeanMapList.SLASH_TAG, BeanMapList.SLASH_TYPE);
		}
		BeanMap result = new BeanMap(type);
		// Recuperation valeur passées en attributs
		for (int i = 0; i < reader.getAttributeCount(); i++) {
			String key = reader.getAttributeName(i);
			String val = reader.getAttribute(i);
			if (BeanMap.KEY_ID.equals(key)) {
				try {
					result.setId(Integer.parseInt(val));
				} catch (NumberFormatException e) {}
			} else if (BeanMap.KEY_DELETED.equals(key)) {
				result.setDeleted("true".equalsIgnoreCase(val)); //$NON-NLS-1$
			} else if (BeanMap.KEY_DATE.equals(key)) {
				try {
					result.setDate(ISODateFormater.toDate(val));
				} catch (ParseException e) {
					result.put(key, val);
				}
			} else if (!BeanMap.KEY_TYPE.equals(key)) {
				result.put(key.replace('_', '.'), xmlToValue(key,val));
			}
		}
		// Recuperation autres valeurs
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			String key = reader.getNodeName().replace('_', '.');
			result.put(key, readAttribute(reader, context, key, result));
			reader.moveUp();
		}
		return result;
	}

	protected Object readAttribute(HierarchicalStreamReader reader, UnmarshallingContext context, String key,
			BeanMap result) {
		// Tout élément avec des attributs est un BeanMap, ou une liste de BeanMap.
		if (reader.getAttributeCount() > 0) {
			// BeanMapList....
			if ((reader.getAttribute(BeanMap.KEY_TYPE) == null) && (reader.getAttribute(BeanMap.KEY_ID) == null)) {
				// FIXME There is a bug if a BeanMap can have an Attribute with name "count"...
				if (reader.getAttribute(BeanMapListConverter.TAG_COUNT) != null) {
					if (listConverter != null) {
						return context.convertAnother(result, BeanMapList.class, listConverter);
					}
					return context.convertAnother(result, BeanMapList.class);
				}
				// Lists...
				if (reader.getAttribute(KEY_LIST) != null) {
					return context.convertAnother(result, ArrayList.class);
				}
			}
			// We have an sub-beanMap object...
			return context.convertAnother(result, BeanMap.class, thisConverter);
		}
		// Tentative de détection d'un nom de classe ou d'un alias...
		/*
		try { 
			Class clazz = mapper.realClass(key); 
			if (clazz != null) {
				return context.convertAnother(result, clazz); 
			} 
		} catch (Exception e) {}
		*/
		// Dernière chance on l'encapsule dans un BeanMap.
		if (reader.hasMoreChildren()) {
			return context.convertAnother(result, BeanMap.class, thisConverter);
		}
		// Valeur basique.
		return xmlToValue(key,reader.getValue());
	}

	protected Object xmlToValue(String name, String value) {
		if ((value == null) || (value.length() == 0)) {
			return value;
		}
		// Tentative de détection d'une Date...
		if (ISODateFormater.mayIsoDate(value)) {
			// Attention au bug de l'an 10000 ! et de l'an 999 !
			try {
				return ISODateFormater.toDate(value);
			} catch (ParseException e) {}
		}
		// Remove tag that looks like number but are not numbers.
		if (!name.startsWith("phone")) { //$NON-NLS-1$
			// Tentative de détection d'un integer...
			try {
				return new Integer(value);
			} catch (NumberFormatException e) {}
		}
		// Renvois d'une String...
		return value;
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return BeanMap.class.equals(type);
	}

	protected BeanMapConverter getThisConverter() {
		return thisConverter;
	}

	protected void setThisConverter(BeanMapConverter thisConverter) {
		this.thisConverter = thisConverter;
	}

	public void setListConverter(BeanMapListConverter listConverter) {
		this.listConverter = listConverter;
	}

	public void setAddNulls(boolean addNulls) {
		this.addNulls = addNulls;
	}

}
