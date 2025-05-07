package com.arcadsoftware.metadata.internal.xml;

import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.ISubCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriterHelper;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

public class AndOrCriteriaConverter implements Converter {

	private final Mapper mapper;
	
	public AndOrCriteriaConverter(Mapper mapper) {
		super();
		this.mapper = mapper;
	}
	
	@Override
	public boolean canConvert(@SuppressWarnings("rawtypes") Class clazz) {
		return OrCriteria.class.equals(clazz) || AndCriteria.class.equals(clazz);
	}

	@Override
	public void marshal(Object obj, HierarchicalStreamWriter writer, MarshallingContext context) {
		for (ISearchCriteria c: ((ISubCriteria) obj).getCriterias()) {
			Class<?> clazz = c.getClass();
            ExtendedHierarchicalStreamWriterHelper.startNode(writer, mapper.serializedClass(clazz), clazz);
            context.convertAnother(c);
            writer.endNode();
		}
	}

	@Override
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		final ISubCriteria result;
		if ("and".equalsIgnoreCase(reader.getNodeName())) {
			result = new AndCriteria();
		} else {
			result = new OrCriteria();
		}
		while (reader.hasMoreChildren()) {
            reader.moveDown();
            try {
	            String classAttribute = reader.getAttribute(mapper.aliasForAttribute("class")); //$NON-NLS-1$
	            @SuppressWarnings("rawtypes")
				Class type;
	            if (classAttribute == null) {
	                type = mapper.realClass(reader.getNodeName());
	            } else {
	                type = mapper.realClass(classAttribute);
	            }
	            Object item = context.convertAnother(result, type);
	            if (item instanceof ISearchCriteria) {
	            	result.add((ISearchCriteria) item);
	            }
            } finally {
	            reader.moveUp();
            }
		}
		return result;
	}

}
