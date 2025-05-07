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
package com.arcadsoftware.metadata.internal.xml;

import java.text.ParseException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.xml.BeanMapUpdateConverter;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.UpdateMetaDataEntity;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.xml.XmlMetaDataStream;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriter;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

public class MetaDataEntityConverter implements Converter {

	protected static final String TAG_DOMAIN = "domain"; //$NON-NLS-1$
	protected static final String TAG_METADATA = "metadata"; //$NON-NLS-1$
	protected static final String TAG_RIGHTS = "rights"; //$NON-NLS-1$
	protected static final String TAG_VERSION = "version"; //$NON-NLS-1$
	protected static final String TAG_RIGHT_CREATE = "create"; //$NON-NLS-1$
	protected static final String TAG_RIGHT_DELETE = "delete"; //$NON-NLS-1$
	protected static final String TAG_RIGHT_UPDATE = "update"; //$NON-NLS-1$
	protected static final String TAG_RIGHT_READ = "read"; //$NON-NLS-1$
	protected static final String TAG_RIGHT_LIST = "list"; //$NON-NLS-1$
	protected static final String TAG_READONLY = "readonly"; //$NON-NLS-1$
	protected static final String TAG_LOCK = "lock" ; //$NON-NLS-1$
	protected static final String TAG_NAME = "name" ; //$NON-NLS-1$
	protected static final String TAG_DESCRIPTION = "description" ; //$NON-NLS-1$

	private final Mapper mapper;
	private final boolean useArray;
	
	public MetaDataEntityConverter(Mapper mapper, boolean useArray) {
		super();
		this.mapper = mapper;
		this.useArray = useArray;
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return MetaDataEntity.class.isAssignableFrom(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (!(source instanceof MetaDataEntity)) {
			return;
		}
		MetaDataEntity e = (MetaDataEntity) source;
		if (e.getDomain() != null) {
			writer.addAttribute(TAG_DOMAIN, e.getDomain());
		}
		writer.addAttribute(TAG_VERSION, Integer.toString(e.getVersion()));
		if (e.getDate() != null) {
			writer.addAttribute(XStreamCompact.TAG_DATE, ISODateFormater.toStringSec(e.getDate()));
		}
		if (e.getType() != null) {
			writer.addAttribute(XStreamCompact.TAG_TYPE, e.getType());
		}
		if ((e.getName() != null) && (e.getName().length() > 0)) {
			writer.addAttribute(TAG_NAME, e.getName());
		}
		if (e.isReadOnly()) {
			writer.addAttribute(TAG_READONLY, "true"); //$NON-NLS-1$
		}
		if (e.isLockable()) {
			writer.addAttribute(TAG_LOCK, "true"); //$NON-NLS-1$
		}
		if (e.getDescription() != null) {
			writer.startNode(TAG_DESCRIPTION);
			writer.setValue(e.getDescription());
			writer.endNode();
		}
		if (e.hasRights()) {
			writer.startNode(TAG_RIGHTS);
			if (e.getCreate() != null) {
				writer.startNode(TAG_RIGHT_CREATE);
				storeCriteria(writer,context,e.getCreate());
				writer.endNode();
			}
			if (e.getDelete() != null) {
				writer.startNode(TAG_RIGHT_DELETE);
				storeCriteria(writer,context,e.getDelete());
				writer.endNode();
			}
			if (e.getUpdate() != null) {
				writer.startNode(TAG_RIGHT_UPDATE);
				storeCriteria(writer,context,e.getUpdate());
				writer.endNode();
			}
			if (e.getRead() != null) {
				writer.startNode(TAG_RIGHT_READ);
				storeCriteria(writer,context,e.getRead());
				writer.endNode();
			}
			if (e.getList() != null) {
				writer.startNode(TAG_RIGHT_LIST);
				storeCriteria(writer,context,e.getList());
				writer.endNode();
			}
			writer.endNode();
		}
		writer.startNode(TAG_METADATA);
		context.convertAnother(e.getMetadata());
		writer.endNode();
		if (useArray) {
			// This format is used for JSON serialization.
			if (!e.getAttributes().isEmpty()) {
				((ExtendedHierarchicalStreamWriter) writer).startNode(XStreamCompact.TAG_ATTRIBUTE, MetaDataAttribute[].class);
				context.convertAnother(e.getAttributes().values().toArray());
				writer.endNode();
			}
			if (!e.getLinks().isEmpty()) {
				((ExtendedHierarchicalStreamWriter) writer).startNode(XStreamCompact.TAG_LINK, MetaDataLink[].class);
				context.convertAnother(e.getLinks().values().toArray());
				writer.endNode();
			}
			if (!e.getTests().isEmpty()) {
				((ExtendedHierarchicalStreamWriter) writer).startNode(XStreamCompact.TAG_TEST, MetaDataTest[].class);
				context.convertAnother(e.getTests().values().toArray());
				writer.endNode();
			}
		} else {
			// This format is the simplifier XML format there is no "attributes", "links" or "tests" node.
			// only many "attribute" nodes, this a valid format according to the XSD.
			for(MetaDataAttribute att:e.getAttributes().values()) {
				writer.startNode(XStreamCompact.TAG_ATTRIBUTE);
				context.convertAnother(att);
				writer.endNode();
			}
			for(MetaDataLink link:e.getLinks().values()) {
				writer.startNode(XStreamCompact.TAG_LINK);
				context.convertAnother(link);
				writer.endNode();
			}
			for(MetaDataTest test:e.getTests().values()) {
				writer.startNode(XStreamCompact.TAG_TEST);
				context.convertAnother(test);
				writer.endNode();
			}
		}
	}

	private void storeCriteria(HierarchicalStreamWriter writer, MarshallingContext context, ISearchCriteria criteria) {
		writer.startNode(mapper.serializedClass(criteria.getClass()));
		context.convertAnother(criteria);
		writer.endNode();
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String s = reader.getAttribute(TAG_VERSION);
		int version = 0;
		if (s != null) {
			try {
				version = Integer.parseInt(s);
			} catch (NumberFormatException e) {
				Activator.getInstance().debug(e);
			}
		}
		s = reader.getAttribute(XStreamCompact.TAG_TYPE);
		MetaDataEntity result;
		if (XmlMetaDataStream.TAG_UPDATEENTITY.equals(reader.getNodeName())) {
			result = new UpdateMetaDataEntity(s,version);
		} else {
			result = new MetaDataEntity(s, version);
		}
		s = reader.getAttribute(TAG_NAME);
		if (s != null) {
			result.setName(s);
		}
		s = reader.getAttribute(TAG_DOMAIN);
		if (s != null) {
			result.setDomain(s);
		}
		s = reader.getAttribute(XStreamCompact.TAG_DATE);
		if (ISODateFormater.mayIsoDate(s)) {
			try {
				result.setDate(ISODateFormater.toDate(s));
			} catch (ParseException e) {}
		}
		s = reader.getAttribute(TAG_READONLY);
		if ("true".equalsIgnoreCase(s)) { //$NON-NLS-1$
			result.setReadOnly(true);
		} else if ("false".equalsIgnoreCase(s)) { //$NON-NLS-1$
			result.setReadOnly(false);
		}
		s = reader.getAttribute(TAG_LOCK);
		if ("true".equalsIgnoreCase(s)) { //$NON-NLS-1$
			result.setLockable(true);
		} else if ("false".equalsIgnoreCase(s)) { //$NON-NLS-1$
			result.setLockable(false);
		}
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			s = reader.getNodeName();
			if ("attributes".equalsIgnoreCase(s)) { //$NON-NLS-1$
				while (reader.hasMoreChildren()) {
					reader.moveDown();
					Object o = context.convertAnother(result, MetaDataAttribute.class);
					if (o instanceof MetaDataAttribute) {
						((MetaDataAttribute)o).setParent(result);
						result.getAttributes().put(((MetaDataAttribute)o).getCode(),(MetaDataAttribute)o);
					}
					reader.moveUp();
				}			
			} else if ("links".equalsIgnoreCase(s)) { //$NON-NLS-1$
				while (reader.hasMoreChildren()) {
					reader.moveDown();
					Object o = context.convertAnother(result, MetaDataLink.class);
					if (o instanceof MetaDataLink) {
						((MetaDataLink)o).setParent(result);
						result.getLinks().put(((MetaDataLink)o).getCode(),(MetaDataLink)o);
					}
					reader.moveUp();
				}			
			} else if ("tests".equalsIgnoreCase(s)) { //$NON-NLS-1$
				while (reader.hasMoreChildren()) {
					reader.moveDown();
					Object o = context.convertAnother(result, MetaDataTest.class);
					if (o instanceof MetaDataTest) {
						((MetaDataTest)o).setParent(result);
						result.getTests().put(((MetaDataTest)o).getCode(), (MetaDataTest)o);
					}
					reader.moveUp();
				}			
			} else if (XStreamCompact.TAG_ATTRIBUTE.equalsIgnoreCase(s)) {
				Object o = context.convertAnother(result, MetaDataAttribute.class);
				if (o instanceof MetaDataAttribute) {
					((MetaDataAttribute)o).setParent(result);
					result.getAttributes().put(((MetaDataAttribute)o).getCode(),(MetaDataAttribute)o);
				}
			} else if (XStreamCompact.TAG_LINK.equalsIgnoreCase(s)) {
				Object o = context.convertAnother(result, MetaDataLink.class);
				if (o instanceof MetaDataLink) {
					((MetaDataLink)o).setParent(result);
					result.getLinks().put(((MetaDataLink)o).getCode(),(MetaDataLink)o);
				}
			} else if (XStreamCompact.TAG_TEST.equalsIgnoreCase(s)) {
				Object o = context.convertAnother(result, MetaDataTest.class);
				if (o instanceof MetaDataTest) {
					((MetaDataTest)o).setParent(result);
					result.getTests().put(((MetaDataTest)o).getCode(), (MetaDataTest)o);
				}
			} else if (TAG_METADATA.equalsIgnoreCase(s)) {
				BeanMap bean = result.getMetadata();
				if (bean  == null) {
					bean = new BeanMap(TAG_METADATA);
					result.setMetadata(bean);
				}
				context.convertAnother(result, BeanMap.class, new BeanMapUpdateConverter(mapper, bean));
			} else if (TAG_RIGHTS.equalsIgnoreCase(s)) {
				while (reader.hasMoreChildren()) {
					reader.moveDown();
					s = reader.getNodeName();
					if (TAG_RIGHT_CREATE.equals(s)) {
						result.setCreate(getCriteria(reader,context,result));
					} else if (TAG_RIGHT_DELETE.equals(s)) {
						result.setDelete(getCriteria(reader,context,result));
					} else if (TAG_RIGHT_UPDATE.equals(s)) {
						result.setUpdate(getCriteria(reader,context,result));
					} else if (TAG_RIGHT_READ.equals(s)) {
						result.setRead(getCriteria(reader,context,result));
					} else if (TAG_RIGHT_LIST.equals(s)) {
						result.setList(getCriteria(reader,context,result));
					}
					reader.moveUp();
				}
			} else if (TAG_DESCRIPTION.equalsIgnoreCase(s)) {
				result.setDescription(reader.getValue());
			}
			reader.moveUp();
		}
		return result;
	}

	private ISearchCriteria getCriteria(HierarchicalStreamReader reader, UnmarshallingContext context,
			MetaDataEntity parent) {
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
