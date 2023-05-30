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
package com.arcadsoftware.metadata.internal.xml;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.xml.BeanMapUpdateConverter;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

public class MetaDataElementConverter implements Converter {

	private static final String TAG_CODE = "code"; //$NON-NLS-1$
	private static final String TAG_NAME = "name"; //$NON-NLS-1$
	private static final String TAG_LISTABLE = "listable"; //$NON-NLS-1$
	private static final String TAG_MANDATORY = "mandatory"; //$NON-NLS-1$
	private static final String VAL_TRUE = "true"; //$NON-NLS-1$
	private static final String VAL_FALSE = "false"; //$NON-NLS-1$
	private static final String TAG_PRECISION = "precision"; //$NON-NLS-1$
	private static final String TAG_LENGTH = "length"; //$NON-NLS-1$
	
	private final Mapper mapper;
	
	public MetaDataElementConverter(Mapper mapper) {
		super();
		this.mapper = mapper;
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return Element.class.isAssignableFrom(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		Element e = (Element)source;
		if ((e.getType() != null) && (e.getType().length() > 0)) {
			writer.addAttribute(XStreamCompact.TAG_TYPE, e.getType());
		}
		if ((e.getCode() != null) && (e.getCode().length() > 0)) {
			writer.addAttribute(TAG_CODE, e.getCode());
		}
		if ((e.getName() != null) && (e.getName().length() > 0)) {
			writer.addAttribute(TAG_NAME, e.getName());
		}
		if (e.getReadOnly() != null) {
			if (e.getReadOnly()) {
				writer.addAttribute(MetaDataEntityConverter.TAG_READONLY, VAL_TRUE);
			} else {
				writer.addAttribute(MetaDataEntityConverter.TAG_READONLY, VAL_FALSE);
			}
		}
		if (source instanceof MetaDataAttribute) {
			MetaDataAttribute a = (MetaDataAttribute)source;
			if (a.getPrecision() != 0) {
				writer.addAttribute(TAG_PRECISION, Integer.toString(a.getPrecision()));
			}
			if (a.getListable() != null) {
				if (a.getListable()) {
					writer.addAttribute(TAG_LISTABLE, VAL_TRUE);
				} else {
					writer.addAttribute(TAG_LISTABLE, VAL_FALSE);
				}
			}
			if (a.getMandatory() != null) {
				if (a.getMandatory()) {
					writer.addAttribute(TAG_MANDATORY, VAL_TRUE);
				} else {
					writer.addAttribute(TAG_MANDATORY, VAL_FALSE);
				}
			}
			if (a.getLength() != 0) {
				writer.addAttribute(TAG_LENGTH, Integer.toString(a.getLength()));
			}
			// Sub xml tags...
			MetaDataAttribute att = (MetaDataAttribute) source;
			if ((att.getRightRead(false) != null) || (att.getRightUpdate(false) != null)) {
				writer.startNode(MetaDataEntityConverter.TAG_RIGHTS);
				if (att.getRightRead(false) != null) {
					storeCriteria(MetaDataEntityConverter.TAG_RIGHT_READ, writer,context,att.getRightRead(false));
				}
				if (att.getRightUpdate(false) != null) {
					storeCriteria(MetaDataEntityConverter.TAG_RIGHT_UPDATE, writer,context,att.getRightUpdate(false));
				}
				writer.endNode();
			}
		} else if (source instanceof MetaDataLink) {
			MetaDataLink link = (MetaDataLink) source;
			if ((link.getRightList(false) != null) || (link.getRightCreate(false) != null)) {
				writer.startNode(MetaDataEntityConverter.TAG_RIGHTS);
				if (link.getRightList(false) != null) {
					storeCriteria(MetaDataEntityConverter.TAG_RIGHT_LIST, writer, context, link.getRightList(false));
				}
				if (link.getRightCreate(false) != null) {
					storeCriteria(MetaDataEntityConverter.TAG_RIGHT_CREATE, writer, context, link.getRightCreate(false));
				}
				writer.endNode();
			}
		}
		if (e.getDescription() != null) {
			writer.startNode(MetaDataEntityConverter.TAG_DESCRIPTION);
			writer.setValue(e.getDescription().trim());
			writer.endNode();
		}
		if ((e.getTest() != null) && e.getTest().trim().length() > 0) {
			writer.startNode(XStreamCompact.TAG_TEST);
			writer.setValue(e.getTest().trim());
			writer.endNode();
		}
		writer.startNode(MetaDataEntityConverter.TAG_METADATA);
		context.convertAnother(e.getMetadata());
		writer.endNode();
	}

	private void storeCriteria(String tag, HierarchicalStreamWriter writer, MarshallingContext context, ISearchCriteria criteria) {
		writer.startNode(tag);
		writer.startNode(mapper.serializedClass(criteria.getClass()));
		context.convertAnother(criteria);
		writer.endNode();
		writer.endNode();
	}
	
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		Element result;
		String s = reader.getNodeName();
		if (XStreamCompact.TAG_ATTRIBUTE.equals(s)) {
			result = new MetaDataAttribute((MetaDataEntity)null);
			s = reader.getAttribute(TAG_PRECISION);
			if ((s != null) && (s.length() > 0)) {
				try {
					((MetaDataAttribute)result).setPrecision(Integer.parseInt(s));
				} catch (NumberFormatException e) {
					Activator.getInstance().debug(e);
				}
			}
			s = reader.getAttribute(TAG_LENGTH);
			if ((s != null) && (s.length() > 0)) {
				try {
					((MetaDataAttribute)result).setLength(Integer.parseInt(s));
				} catch (NumberFormatException e) {
					Activator.getInstance().debug(e);
				}
			}
			s = reader.getAttribute(TAG_LISTABLE);
			if ((s != null) && (s.length() > 0)) {
				((MetaDataAttribute)result).setListable(VAL_TRUE.equalsIgnoreCase(s));
			}
			s = reader.getAttribute(TAG_MANDATORY);
			if ((s != null) && (s.length() > 0)) {
				((MetaDataAttribute)result).setMandatory(VAL_TRUE.equalsIgnoreCase(s));
			}
		} else if (XStreamCompact.TAG_LINK.equals(s)) {
			result = new MetaDataLink((MetaDataEntity)null);
		} else { // Blindage
			return null;
		}
		s = reader.getAttribute(TAG_CODE);
		if ((s != null) && (s.length() > 0)) {
			result.setCode(s);
		}
		s = reader.getAttribute(TAG_NAME);
		if ((s != null) && (s.length() > 0)) {
			result.setName(s);
		}
		s = reader.getAttribute(XStreamCompact.TAG_TYPE);
		if ((s != null) && (s.length() > 0)) {
			result.setType(s);
		}
		s = reader.getAttribute(MetaDataEntityConverter.TAG_READONLY);
		if ((s != null) && (s.length() > 0)) {
			result.setReadonly(Boolean.parseBoolean(s));
		}
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			s = reader.getNodeName();
			if (XStreamCompact.TAG_TEST.equals(s)) {
				String t = reader.getValue();
				if (t != null) {
					t = t.trim();
					if (t.length() > 0) {
						result.setTest(t);
					}
				}
			} else if (MetaDataEntityConverter.TAG_METADATA.equals(s)) {
				BeanMap bean = result.getMetadata();
				if (bean  == null) {
					bean = new BeanMap(MetaDataEntityConverter.TAG_METADATA);
					result.setMetadata(bean);
				}
				context.convertAnother(result, BeanMap.class, new BeanMapUpdateConverter(mapper, bean));
			} else if (MetaDataEntityConverter.TAG_RIGHTS.equals(s)) {
				while (reader.hasMoreChildren()) {
					reader.moveDown();
					s = reader.getNodeName();
					if (result instanceof MetaDataAttribute) {
						if (MetaDataEntityConverter.TAG_RIGHT_READ.equals(s)) {
							((MetaDataAttribute)result).setRead(getCriteria(reader, context, result));
						} else if (MetaDataEntityConverter.TAG_RIGHT_UPDATE.equals(s)) {
							((MetaDataAttribute)result).setUpdate(getCriteria(reader, context, result));
						}
					} else if (result instanceof MetaDataLink) {
						if (MetaDataEntityConverter.TAG_RIGHT_LIST.equals(s)) {
							((MetaDataLink)result).setList(getCriteria(reader, context, result));
						} else if (MetaDataEntityConverter.TAG_RIGHT_CREATE.equals(s)) {
							((MetaDataLink)result).setCreate(getCriteria(reader, context, result));
						}
					}
					reader.moveUp();
				}
			} else if (MetaDataEntityConverter.TAG_DESCRIPTION.equalsIgnoreCase(s)) {
				result.setDescription(reader.getValue());
			}
			reader.moveUp();
		}
		return result;
	}

	private ISearchCriteria getCriteria(HierarchicalStreamReader reader, UnmarshallingContext context, Element parent) {
		if (reader.hasMoreChildren()) {
			reader.moveDown();
			Class<?> c = mapper.realClass(reader.getNodeName());
			if ((c != null) && ISearchCriteria.class.isAssignableFrom(c)) {
				Object o = context.convertAnother(parent, c);
				if (o instanceof ISearchCriteria) {
					reader.moveUp();
					return (ISearchCriteria)o;
				}
			}
			reader.moveUp();
		}
		return null;
	}
}
