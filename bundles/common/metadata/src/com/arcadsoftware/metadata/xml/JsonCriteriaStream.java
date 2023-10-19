package com.arcadsoftware.metadata.xml;

import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;

public class JsonCriteriaStream extends JSonBeanMapStream {

	public JsonCriteriaStream() {
		super(JsonMetaDataStream.class.getClassLoader(), true, true, false);
	}
	
	@Override
	protected void InitializeBase() {
		super.InitializeBase();
		XmlCriteriaStream.initialize(this);
		setMode(NO_REFERENCES);
	}

	@Override
	public Object fromXML(String xml) {
		return super.fromXML(xml);
	}

	@Override
	public String toXML(Object object) {
		return super.toXML(object);
	}
}