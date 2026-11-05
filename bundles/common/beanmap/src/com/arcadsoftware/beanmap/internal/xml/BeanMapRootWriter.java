package com.arcadsoftware.beanmap.internal.xml;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.WriterWrapper;

public class BeanMapRootWriter extends WriterWrapper {

	private int depth = 0;
	private final String rootName;
	private final String beanName;
	
	public BeanMapRootWriter(final HierarchicalStreamWriter wrapped, final String rootName, final String beanName) {
		super(wrapped);
		this.rootName = rootName;
		this.beanName = beanName;
	}

    @Override
    public void startNode(String name) {
    	depth++;
    	if (depth == 1) {
            super.startNode(rootName);
    	} else if ((beanName != null) && XStreamCompact.TAG_BEAN.equals(name)) {
    		super.startNode(beanName);
    	} else {
    		super.startNode(name);
    	}
    }

    @Override
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public void startNode(String name, Class clazz) {
    	depth++;
    	if (depth == 1) {
            super.startNode(rootName, clazz);
    	} else if ((beanName != null) && clazz.isAssignableFrom(BeanMap.class) && XStreamCompact.TAG_BEAN.equals(name)) {
    		super.startNode(beanName);
    	} else {
    		super.startNode(name, clazz);
    	}
    }

    @Override
    public void endNode() {
        depth--;
        super.endNode();
    }
}
