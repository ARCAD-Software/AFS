package com.arcadsoftware.beanmap.internal.xml;

import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.ReaderWrapper;

public class BeanMapRootReader extends ReaderWrapper {

	private final String beanName;
	private final String listName;
    private int depth = 0;
	
	public BeanMapRootReader(HierarchicalStreamReader reader, final String beanName, final String listName) {
		super(reader);
		this.beanName = beanName;
		this.listName = listName;
	}

    @Override 
    public void moveDown() {
    	super.moveDown(); 
    	depth++; 
    }
    
    @Override 
    public void moveUp() {
    	super.moveUp();
    	depth--; 
    }

    @Override
    public String getNodeName() {
    	String name = super.getNodeName();
    	if ((depth == 0) && (name != null)) {
    		if (name.equals(listName)) {
    			return XStreamCompact.TAG_LIST;
    		}
    		if (name.equals(beanName)) {
    			return XStreamCompact.TAG_BEAN;
    		}
    	}
    	return name;
    }
}
