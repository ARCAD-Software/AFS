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
/**
 * 
 */
package com.arcadsoftware.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.metadata.internal.Activator;

/**
 * A line is a chained list of EntityElement where the type of the element i is the same type that the parent of the element i+1
 *
 */
public class ReferenceLine extends ArrayList<Element> implements Comparable<ReferenceLine> {

	/**
	 * Empty list of references (immutable).
	 */
//	@SuppressWarnings("unchecked")
//	private static final List<ReferenceLine> EMPTY_LIST = Collections.EMPTY_LIST;
	
	private static final long serialVersionUID = -1224881054112676872L;

	/**
	 * Fill the result list with all the references lines that bring to another domain.
	 * 
	 * <p>
	 * The result List (the parameter!), that must be not null, will be filled with references line limited to the main domain name.
	 * Duplicated sub references are removed from this list.
	 * 
	 * <p>
	 * The result Map will contains the sub references that belong to other domains, each reference line (key) is associated to the
	 * following references lines that need to be obtained from the foreign domain Mapper.
	 * 
	 * <p>
	 * This methods does not check if the references line includes multi-links nor multiple sub-domains.
	 * 
	 * @param domain the main domain to refer to.
	 * @param refs the references lines to test.
	 * @param result the complete sub-references list that belong to the main domain
	 * @return the sub list of references that bring to another domain and the associated sub references. 
	 */
	public static Map<ReferenceLine,List<ReferenceLine>> fillDomainReferences(List<ReferenceLine> refs, List<ReferenceLine> result) {
		if ((refs == null) || (result == null)) {
			return null;
		}
		HashMap<ReferenceLine,List<ReferenceLine>> extraDomains = new HashMap<ReferenceLine,List<ReferenceLine>>();
		for(ReferenceLine ref:refs) {
			int x = ref.getSecondMapperIndex();
			if (x == 0) {
				result.add(ref);
			} else {
				ReferenceLine r = ref.getSubReference(x);
				List<ReferenceLine> list = extraDomains.get(r);
				if (list == null) {
					list = new ArrayList<ReferenceLine>(1);
					result.add(r);
				}
				list.add(ref.getFollowingReference(x));
				extraDomains.put(r, list);
			}
		}
		if (extraDomains.size() == 0) {
			return null;
		}
		return extraDomains;
	}

	/**
	 * Fill the result list with all the references lines that bring to another domain.
	 * 
	 * <p>
	 * The result Map will contains the sub references that belong to other domains, each reference line (key) is associated to the
	 * following references lines that need to be obtained from the foreign domain Mapper.
	 * 
	 * <p>
	 * This methods does not check if the references line includes multi-links nor multiple sub-domains.
	 * 
	 * @param refs the references lines to test.
	 * @param result the complete sub-references list that belong to the main domain
	 * @return the sub list of references that bring to another domain and the associated sub references. 
	 */
	public static Map<ReferenceLine,List<ReferenceLine>> fillDomainReferences(Collection<ReferenceLine> refs) {
		if (refs == null) {
			return null;
		}
		HashMap<ReferenceLine,List<ReferenceLine>> extraDomains = new HashMap<ReferenceLine,List<ReferenceLine>>();
		for(ReferenceLine ref:refs) {
			int x = ref.getSecondMapperIndex();
			if (x > 0) {
				ReferenceLine r = ref.getSubReference(x);
				List<ReferenceLine> list = extraDomains.get(r);
				if (list == null) {
					list = new ArrayList<ReferenceLine>(1);
				}
				list.add(ref.getFollowingReference(x));
				extraDomains.put(r, list);
			}
		}
		if (extraDomains.size() == 0) {
			return null;
		}
		return extraDomains;
	}
	
	/**
	 * Return the list of references lines from the given list that are not references to local <code>domain</code>.
	 * 
	 * @param domain
	 * @param refs 
	 * @return the map of Code from <code>refs</code> and the local associated references. The references lines from <code>refs</code> witch codes is not into the result map are local references.
	 */
	public static Map<String,ReferenceLine> extraDomainReferences(Collection<ReferenceLine> refs) {
		if (refs == null) {
			return null;
		}
		HashMap<String,ReferenceLine> extraDomains = new HashMap<String,ReferenceLine>();
		for(ReferenceLine ref:refs) {
			int x = ref.getSecondMapperIndex();
			if (x > 0) {
				extraDomains.put(ref.getCode(), ref.getSubReference(x));
			}
		}
		return extraDomains;
	}

	/**
	 * Return the list of references that start from the sub reference.
	 * 
	 * @param subReference
	 * @param references
	 * @return
	 */
	public static List<ReferenceLine> getSubReferences(ReferenceLine subReference, List<ReferenceLine> references) {
		if ((subReference == null) || (references == null)) {
			return null;
		}
		ArrayList<ReferenceLine> result = new ArrayList<ReferenceLine>(references.size());
		for(ReferenceLine ref:references) {
			ReferenceLine r = ref.getSubReferenceLine(subReference);
			if (r != null) {
				result.add(r);
			}
		}
		return result;
	}
	
	/**
	 * Get the list of codes of the given list of reference lines, the code are separated by white spaces.
	 * @param lines
	 * @return
	 */
	public static String getCodes(List<ReferenceLine> lines) {
		if ((lines == null) || (lines.size() == 0)) {
			return null;
		}
		if (lines.size() == 1) {
			return lines.get(0).getCode();
		}
		StringBuilder sb = new StringBuilder();
		for(ReferenceLine line:lines) {
			if (sb.length() > 0) {
				sb.append(' ');
			}
			sb.append(line.getCode());
		}
		return sb.toString();
	}
	
	private boolean multiLink;
	private boolean link;
	private String code;
	private boolean flaged;
	
    /**
     * Constructs an empty list with an initial capacity of ten.
     */
	public ReferenceLine(String code) {
		super();
		this.code = code;
	}

    /**
     * Constructs an empty list with the specified initial capacity.
     *
     * @param code the reference line code.
     * @param   initialCapacity   the initial capacity of the list
     * @exception IllegalArgumentException if the specified initial capacity
     *            is negative
     */
	public ReferenceLine(String code, int initialCapacity) {
		super(initialCapacity);
		this.code = code;
	}

    /**
     * Constructs a list containing the elements of the specified
     * collection, in the order they are returned by the collection's
     * iterator.
     *
     * @param c the collection whose elements are to be placed into this list
     * @throws NullPointerException if the specified collection is null
     */
	public ReferenceLine(Collection<? extends Element> c) {
		super(c);
	}

    /**
     * Constructs a list containing the specified elements, in the order they are 
	 * passed to this method.
     *
     * @param elements The elements that are to be placed into this list
     */
	public ReferenceLine(Element... elements) {
		super(elements.length);
		for(Element element:elements) {
			add(element);
		}
		if (elements.length == 1) {
			code = elements[0].getCode();
		}
	}

    /**
     * Constructs an empty list with the specified initial capacity.
     *
     * @param   initialCapacity   the initial capacity of the list
     * @exception IllegalArgumentException if the specified initial capacity
     *            is negative
     */
	public ReferenceLine(int initialCapacity) {
		super(initialCapacity);
	}

	public ReferenceLine(String code, int length, boolean flaged) {
		this(code,length);
		this.flaged = flaged;
	}

	/**
	 * @return The first element of the line.
	 */
	public Element getOrigin() {
		if (size() == 0) {
			return null;
		}
		return get(0);
	}
	
	public MetaDataEntity getOriginEntity() {
		if (size() == 0) {
			return null;
		}
		try {
			return get(0).getParent();
		} catch (ClassCastException e) {
			Activator.getInstance().debug(e);
			return null;
		}
	} 
	
	/**
	 * @return The last element of the line.
	 */
	public Element getLast() {
		if (size() == 0) {
			return null;
		}
		return get(size() -1);
	}
	
	/**
	 * @return true if the last element of the line is not a reference to another entity.
	 */
	public boolean isFinal() {
		Element element = getLast();
		return (element != null) && (element instanceof MetaDataAttribute) && !((MetaDataAttribute)element).isReference();
	}

	/**
	 * Determine if the Reference line is terminated with an numerical attribute.
	 * 
	 * <p>
	 * Note that reference to another Entity is considered as a numerical reference line (as IDs are integers).  
	 *  
	 * @return
	 */
	public boolean isNumericType() {
		Element element = getLast();
		return (element != null) && (element instanceof MetaDataAttribute) && (((MetaDataAttribute)element).isReference() || ((MetaDataAttribute)element).isNumeric());
	}

	/**
	 * @return true if and only if this references line contains a unique attribute code. 
	 */
	public boolean isSimple() {
		return size() == 1;
	}
	
	public boolean makeFinal() {
		if (size() == 0) {
			return false;
		}
		Element element = getLast();
		if ((element instanceof MetaDataAttribute) && !((MetaDataAttribute)element).isReference()) {
			return true;
		}
		MetaDataEntity entity;
		try {
			entity = element.getParent();
		} catch (ClassCastException e) {
			Activator.getInstance().debug(e);
			return false;
		}
		MetaDataAttribute attribute = entity.getAttribute(IIdentifiedBean.KEY_NAME);
		if ((attribute != null) && !attribute.isReference()) {
			add(attribute);
			return true;
		}
		attribute = entity.getAttribute(IIdentifiedBean.KEY_CODE);
		if ((attribute != null) && !attribute.isReference()) {
			add(attribute);
			return true;
		}
		attribute = entity.getAttribute(IIdentifiedBean.KEY_FULLNAME);
		if ((attribute != null) && !attribute.isReference()) {
			add(attribute);
			return true;
		}
		attribute = entity.getAttribute(IIdentifiedBean.KEY_TEXT);
		if ((attribute != null) && !attribute.isReference()) {
			add(attribute);
			return true;
		}
		return false;
	}
	
	/**
	 * @return true if this line contains one or more Link association references.
	 */
	public boolean isLinkList() {
		if (link || multiLink) {
			return true;
		}
		for (Element e: this) {
			if (e instanceof MetaDataLink) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * @return true if this line contains more than one association references.
	 */
	public boolean isMultiLinkList() {
		if (link && multiLink) {
			return true;
		}
		int res = 0;
		for (Element e: this) {
			if (e instanceof MetaDataLink) {
				res++;
				if (res > 1) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * @return true if this list contains only EntityAttributes references.
	 */
	public boolean isAttributeList() {
		if (link || multiLink) {
			return false;
		}
		for (Element e: this) {
			if (e instanceof MetaDataLink) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * This method test if this reference line can be sent to client.
	 * 
	 * @return True if one of the elements of this line is hidden.
	 * @see #isPublic()
	 */
	public boolean isHidden() {
		for (Element e: this) {
			if ((e != null) && e.getMetadata().getBoolean(MetaDataEntity.METADATA_HIDDEN)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * This method test if this reference line can be sent to client.
	 * 
	 * @return False if one of the elements of this line is hidden.
	 * @see #isHidden() 
	 */
	public boolean isPublic() {
		for (Element e: this) {
			if ((e != null) && e.getMetadata().getBoolean(MetaDataEntity.METADATA_HIDDEN)) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Test the line and validate the assumption (i.e. each entity type follow the previous element type).
	 * @return
	 */
	public boolean validate() {
		Element prev = null;
		for(Element e: this) {
			if ((prev != null) && !e.getType().equals(prev.getParent().getType())) {
				return false;
			}
			prev = e;
		}
		return true;
	}
	
	/**
	 * Add the element to the line only if it validate the list line.
	 * @param element
	 * @return
	 */
	public boolean addIfValid(Element element) {
		if (element == null) {
			return false;
		}
		if ((size() == 0) || getLast().getType().equals(element.getParent().getType())) {
			return add(element);
		}
		return false;
	} 

	/**
	 * @return Construct the code of this line.
	 */
	public String getCode() {
		if (code != null) {
			return code;
		}
		StringBuilder sb = new StringBuilder();
		for (Element element: this) {
			if (sb.length() > 0) {
				sb.append('.');
			}
			sb.append(element.getCode());
		}
		code = sb.toString();
		return code;
	}

	/**
	 * @param multiLink the multiLink to set
	 */
	public void setIsMultiLink(boolean multiLink) {
		this.multiLink = multiLink;
	}

	/**
	 * @param link the link to set
	 */
	public void setIsLink(boolean link) {
		this.link = link;
	}

	/**
	 * Get the latest attribute of the reference line.
	 * 
	 * @return null if this line is empty or ends with a link.
	 */
	public MetaDataAttribute getLastAttribute() {
		Element last = getLast();
		if (last instanceof MetaDataAttribute) {
			return (MetaDataAttribute) last;
		}
		return null;
	}

	/**
	 * Get the Entity associated to the latest attribute or link of this reference line.
	 * 
	 * @return null if this line is empty.
	 */
	public MetaDataEntity getLastEntity() {
		Element last = getLast();
		if (last instanceof MetaDataAttribute) {
			return ((MetaDataAttribute) last).getParent();
		}
		if (last instanceof MetaDataLink) {
			return ((MetaDataLink) last).getParent();
		}
		return null;
	}
	
	/**
	 * Test if this reference line starts with the given reference line.
	 * @param ref Another reference line, may be null.
	 * @return
	 */
	public boolean startWith(ReferenceLine ref) {
		if (ref == null) {
			return true;
		}
		if (ref.size() > size()) {
			return false;
		}
		int i = 0;
		for(Element e:ref) {
			if (!e.equals(get(i++))) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Return the remaining part of this reference line only if it start with.
	 * 
	 * @param ref the starting point of the reference line.
	 * @return The sub-reference line starting after the last attribute of the parameter. this line can be null if this line end at the same point or if it does not start this de reference paramter.
	 */
	public ReferenceLine getSubReferenceLine(ReferenceLine ref) {
		if ((ref == null) || (ref.size() == 0)) {
			return this;
		}
		if ((!startWith(ref)) || (ref.size() == size())) {
			return null;
		}
		ReferenceLine result = new ReferenceLine(size());
		for(int i = ref.size(); i < size();i++) {
			result.add(get(i++));
		}
		return result;
	}
	
	/**
	 * Return the starting reference line that stop to the first reference attribute where referenced entity does not belong to the same domain mapper.
	 * @return null if this reference line is empty or belong to a unique domain.
	 */
	public ReferenceLine getMainMapperReference() {
		if (size() <= 1) {
			return null;
		}
		MetaDataEntity entity = getOriginEntity();
		if (entity == null) {
			return null;
		}
		ReferenceLine result = new ReferenceLine(size());
		for(Element e:this) {
			if (entity.sameMapper(e.getParent())) {
				result.add(e);
			} else {
				return result;
			}
		}
		return null;
	}

	/**
	 * Return the index of the first element that is bound to a different domain than previous ones.
	 * 
	 * <p>
	 * Return zero if all the element are bound to the same domain or if this reference line is empty.
	 * 
	 * @return the index of the first object that is not bound to the first domain. 
	 */
	public int getSecondMapperIndex() {
		if (size() <= 1) {
			return 0;
		}
		MetaDataEntity entity = getOriginEntity();
		if (entity == null) {
			return 0;
		}
		int s = size();
		for(int i = 0; i < s; i++) {
			if (!(get(i).getParent()).sameMapper(entity)) {
				return i;
			}
		}
		return 0;
	}
	
	/**
	 * Return the reference line that is limited to the <code>index</code>-1 first elements.
	 * 
	 * @param index
	 * @return
	 * @see #getSecondMapperIndex()
	 */
	public ReferenceLine getSubReference(int index) {
		if (index <= 0) {
			return new ReferenceLine();
		}
		if (index > size()) {
			return this;
		}
		ReferenceLine result = new ReferenceLine(index);
		for (int i = 0; i < index; i++) {
			result.add(get(i));
		}		return result;
	}

	/**
	 * Return the reference line stating to the <code>index</code> elements to the end.
	 * @param index
	 * @return
	 */
	public ReferenceLine getFollowingReference(int index) {
		if (index <= 0) {
			return this;
		}
		if (index > size()) {
			return null;
		}
		ReferenceLine result = new ReferenceLine(index);
		int s = size();
		for (int i = index; i < s; i++) {
			result.add(get(i));
		}
		return result;
	}

	public int compareTo(ReferenceLine o) {
		if (o == null) {
			return 1;
		}
		int i = size() - o.size();
		if (i != 0) {
			return i;
		}
		ListIterator<Element> e = listIterator();
		ListIterator<Element> eo = o.listIterator();
		while(eo.hasNext() && eo.hasNext()) {
		    i = e.next().compareTo(eo.next());
		    if (i != 0) {
		    	return i;
		    }
		}
		return 0;
	}

	/**
	 * Compute the domain name of this line, and check that it is unique.
	 * @return null if this line is empty or if more than one domain is used.
	 */
	public String getDomain() {
		String result = null;
		for(Element e:this) {
			String domain = (e.getParent()).getDomain();
			if (domain != null) {
				if (result == null) {
					result = domain;
				} else if (!result.equalsIgnoreCase(domain)) {
					return null;
				}
			}
		}
		return result;
	}

	/**
	 * @param flaged The Flag state of the reference line.
	 */
	public void setFlaged(boolean flaged) {
		this.flaged = flaged;
	}

	/**
	 * Return the current flag state of this reference line.
	 * 
	 * @return true/false.
	 */
	public boolean isFlaged() {
		return flaged;
	}

	@Override
	public String toString() {
		return code;
	}

	/**
	 * @return true 
	 */
	public boolean isTranslatable() {
		MetaDataAttribute att = getLastAttribute();
		return (att != null) && att.isTranslatable();
	}

	/**
	 * Translate the BeanMap value corresponding to this reference line according to the latest attribute translation.
	 * 
	 * @param bm
	 * @param language
	 */
	public void translate(BeanMap bm, Language language) {
		ReferenceLine trl = getTranslateCode();
		String code;
		if (trl != null) {
			code = bm.getString(trl.getCode());
		} else { 
			code = Integer.toString(bm.getId());
		}
		MetaDataAttribute att = getLastAttribute();
		// Translation rule: {type}.{attCode}.{code/id}
		bm.put(getCode(), Activator.getInstance().translate(Activator.TRANLATEDOMAIN_DATA, att.getParent().getType() + '.' + att.getCode() + '.' + code, language));
	}

	/**
	 * Translate the last Attribute value corresponding to this reference line according to the given code (which may be a "code" value or an ID depending on the data).
	 * 
	 * @param aid a non null id
	 * @param language
	 */
	public String translate(String code, Language language) {
		MetaDataAttribute att = getLastAttribute();
		// Translation rule: {type}.{attCode}.{code/id}
		return Activator.getInstance().translate(Activator.TRANLATEDOMAIN_DATA, att.getParent().getType() + '.' + att.getCode() + '.' + code, language);
	}

	/**
	 * Get the reference line required to ensure that this ReferenceLine will be translated (after selection).
	 * 
	 * <p>
	 * This reference line must replace this one into any Mapper selection request.
	 * 
	 * @return null if this reference line is not translatable, empty or will use the result ID to get the translation.
	 * @see MetaDataAttribute. 
	 */
	public ReferenceLine getTranslateCode() {
		MetaDataAttribute att = getLastAttribute();
		if ((att == null) || !att.isTranslatable()) {
			return null;
		}
		att = att.getParent().getAttribute(IIdentifiedBean.KEY_CODE);
		ReferenceLine result = getSubReference(size()-1);
		if (att != null) {
			result.add(att);
		} else if (result.isEmpty()) {
			return null;
		}
		return result;
	}

	public String getFirstLinkCode() {
		for (Element element: this) {
			if (element instanceof MetaDataLink) {
				return element.getCode();
			}
		}
		return null;
	}

	public String getPreLinkCodes() {
		StringBuilder sb = new StringBuilder();
		for (Element element: this) {
			if (element instanceof MetaDataLink) {
				break;
			}
			if (sb.length() > 0) {
				sb.append('.');
			}
			sb.append(element.getCode());
		}
		return sb.toString();
	}

	public String getPostLinkCodes() {
		StringBuilder sb = new StringBuilder();
		for (int i = size() - 1; i >= 0; i--) {
			Element element = get(i);
			if (element instanceof MetaDataLink) {
				break;
			}
			if (sb.length() > 0) {
				sb.insert(0, '.');
			}
			sb.insert(0, element.getCode());
		}
		return sb.toString();
	}
	
}
