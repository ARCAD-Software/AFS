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
package com.arcadsoftware.metadata;

import java.io.Serializable;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.ITypedBean;
import com.arcadsoftware.metadata.internal.Activator;

/**
 * Define the structure of an element.
 * 
 * <p>
 * And element is and attribute or a link of an Entity.
 * 
 * @see com.arcadsoftware.metadata.MetaDataAttribute
 * @see com.arcadsoftware.metadata.MetaDataLink
 */
public abstract class Element implements Cloneable, Serializable, ITypedBean, Comparable<Element> {

	private static final long serialVersionUID = 8485220399789006161L;

	private transient MetaDataEntity parent;
	private String code;
	private String name;
	private String description;
	private String type;
	private Boolean readonly;
	private String test;
	private BeanMap metadata;

	public Element() {
		super();
		metadata = new BeanMap();
	}

	public Element(MetaDataEntity parent) {
		this();
		this.parent = parent;
	}

	public Element(Element element, MetaDataEntity parent) {
		this(parent);
		if (element.getCode() != null) {
			setCode(element.getCode());
		}
		if (element.getName() != null) {
			setName(element.getName());
		}
		if (element.getDescription() != null) {
			setDescription(element.getDescription());
		}
		if (element.getReadOnly() != null) {
			setReadonly(element.isReadonly());
		}
		if (element.getTest() != null) {
			setTest(element.getTest());
		}
		if (element.getType() != null) {
			setType(element.getType());
		}
		if (element.getMetadata() != null) {
			try {
				setMetadata((BeanMap)element.getMetadata().clone());
			} catch (Exception e) {
				Activator.getInstance().debug(e);
			}
		}
	}

	public Element(MetaDataEntity parent, String code, String type) {
		this(parent);
		this.code = code;
		this.type = type;
	}

	public Element(MetaDataEntity parent, String code) {
		this(parent);
		this.code = code;
	}

	public Element(String code, String type) {
		this();
		this.code = code;
		this.type = type;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Element) {
			if (parent == null) {
				return (((Element)obj).parent == null) && ((code == null) || code.equals(((Element)obj).code));
			}
			return parent.equals(((Element)obj).parent) && ((code == null) || code.equals(((Element)obj).code));
		}
		return false;
	}

	@Override
	public String toString() {
		return code + " [" + type + ']'; //$NON-NLS-1$
	}

	public int compareTo(Element o) {
		if (parent == null) {
			if (o.parent != null) {
				return -1;
			}
		} else {
			if (o.parent == null) {
				return 1;
			}
			int i = parent.compareTo(o.parent);
			if (i != 0) {
				return i;
			}
		}
		if (code == null) {
			if (o.code == null) {
				return 0;
			}
			return -1;
		}
		if (o.code == null) {
			return 1;
		}
		return code.compareTo(o.code);
	}

	/**
	 * @param metadata the metadata of this element
	 */
	public void setMetadata(BeanMap metadata) {
		this.metadata = metadata;
	}

	/**
	 * @return the metadata of this element.
	 */
	public BeanMap getMetadata() {
		return metadata;	
	}
	
	/**
	 * The type define what is the element value.
	 * 
	 * @param type
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * The Element code is the key, string identifier of this element into the BeanMap.
	 * 
	 * @return the code.
	 */
	public String getCode() {
		return code;
	}

	/**
	 * The Element code is the key, string identifier of this element into the BeanMap.
	 * 
	 * @param code
	 */
	public void setCode(String code) {
		if (code != this.code) {
			final String oldCode = this.code;
			this.code = code;
			if (parent != null) {
				parent.reIndexElement(oldCode, this);
			}
		}
	}

	/**
	 * The element Name is the printable, translated name of this element.
	 * 
	 * <p>
	 * You must use this value in any conversation with the user, the <code>code</code> is an internal representation of
	 * this element.
	 * 
	 * <p>
	 * <b>Note that on server side the Entity are not translated. You need to clone it with desired language first.</b>
	 * 
	 * @return the translated name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * The element Name is the printable, translated name of this element.
	 * 
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * The Description of the Element is a printable, user readable, text that can explain the nature of this Element.
	 * 
	 * <p>
	 * You may use this value in any conversation with the user.
	 * 
	 * <p>
	 * <b>Note that on server side the Entity are not translated. You need to clone it with desired language first.</b>
	 * 
	 * 
	 * @return the translated description.
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * The Description of the Element is a printable, user readable, text that can explain the nature of this Element.
	 * 
	 * @param description
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * The type define what is the element value.
	 * 
	 * <p>
	 * This can also be a Entity type, in this case the attribute is a reference to an item of this entity and its
	 * actual value is an integer equal the the item id.
	 * 
	 * <p>
	 * Links should only use reference to entity.
	 * 
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * Specify that this attribute, or link can not be modified by the current user.
	 * 
	 * @return true if this attribute value can not be modified by this user.
	 */
	public boolean isReadonly() {
		return (readonly != null) && readonly;
	}

	/**
	 * Specify that this attribute, or link can not be modified by the current user.
	 * 
	 * @param readOnly
	 */
	public void setReadonly(boolean readOnly) {
		this.readonly = readOnly;
	}

	/**
	 * Return the javascript test program that can be used to test the validity of a new value for this attribute or
	 * link.
	 * 
	 * @param test
	 */
	public void setTest(String test) {
		this.test = test;
	}

	/**
	 * Return the groovy test program that can be used to test the value modification of this attribute or
	 * link.
	 * 
	 * @return an groovy script or null.
	 */
	public String getTest() {
		return test;
	}

	/**
	 * <em>For serialization only... equal to <code>getType()</code></em>
	 * 
	 * @return
	 */
	public String getValuetype() {
		return getType();
	}

	/**
	 * <em>For serialization only... equal to <code>setType(String)</code></em>
	 * 
	 * @param type
	 */
	public void setValuetype(String type) {
		setType(type);
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.utils.ITypedBean#equalsType(com.arcadsoftware.utils.ITypedBean)
	 */
	public boolean equalsType(ITypedBean bm) {
		if (type == null) {
			return bm.getType() == null;
		}
		return type.equals(bm.getType());
	}

	/**
	 * @param parent the new parent of this element. 
	 */
	public void setParent(MetaDataEntity parent) {
		this.parent = parent;
	}

	/**
	 * @return the parent of this element.
	 */
	public MetaDataEntity getParent() {
		return parent;
	}
	
	/**
	 * Read only elements can not be modified by client programs (and associated web services).
	 * 
	 * <p>
	 * These data are not 
	 * 
	 * @return true if this element is read-only.
	 */
	public Boolean getReadOnly() {
		return readonly;
	}
}
