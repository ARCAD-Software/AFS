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
package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * The Search criteria are structured conditions that can be assembled into a complex conditional clause to select specific
 * entities. As a given criteria can be applied to any kind of entity, as it must test the effective type of the entity
 * and if it is not applicable it must reduce itself to a ConstantCriteria.FALSE criteria.
 */
public interface ISearchCriteria {

	/**
	 * Run a local evaluation of the condition according to the given object.
	 * 
	 * @param bean the object to test.
	 * @return true if this object validate this condition.
	 */
	public boolean test(BeanMap bean, IConnectionUserBean currentUser);
	
	/**
	 * Run a local selection.
	 * 
	 * @param list the selectables objects.
	 * @return the list of selected objects.
	 */
	public BeanMapList select(BeanMapList list, IConnectionUserBean currentUser);
	
	/**
	 * Optimization of this search criteria, the criteria properties need to be changed during the reduction process 
	 * then the returned object will be different from the current instance.
	 * 
	 * If a new search Criteria is return it must possess the semantic than the current one applied to the current context.
	 * If context change then a new reduction process should be done with original object.
	 * 
	 * @return a simpler, or at least equal search criteria.
	 */
	public ISearchCriteria reduce(ICriteriaContext context);

    /**
     * Creates and returns a copy of this object.  The precise meaning 
     * of "copy" may depend on the class of the object. The general 
     * intent is that, for any object <tt>x</tt>, the expression:
     * <blockquote>
     * <pre>
     * x.clone() != x</pre></blockquote>
     * will be true, and that the expression:
     * <blockquote>
     * <pre>
     * x.clone().getClass() == x.getClass()</pre></blockquote>
     * will be <tt>true</tt>, but these are not absolute requirements. 
     * While it is typically the case that:
     * <blockquote>
     * <pre>
     * x.clone().equals(x)</pre></blockquote>
     * will be <tt>true</tt>, this is not an absolute requirement. 
     * <p>
     * By convention, the returned object should be obtained by calling
     * <tt>super.clone</tt>.  If a class and all of its superclasses (except
     * <tt>Object</tt>) obey this convention, it will be the case that
     * <tt>x.clone().getClass() == x.getClass()</tt>.
     * <p>
     * By convention, the object returned by this method should be independent
     * of this object (which is being cloned).  To achieve this independence,
     * it may be necessary to modify one or more fields of the object returned
     * by <tt>super.clone</tt> before returning it.  Typically, this means
     * copying any mutable objects that comprise the internal "deep structure"
     * of the object being cloned and replacing the references to these
     * objects with references to the copies.  If a class contains only
     * primitive fields or references to immutable objects, then it is usually
     * the case that no fields in the object returned by <tt>super.clone</tt>
     * need to be modified.
     * <p>
     * The method <tt>clone</tt> for class <tt>Object</tt> performs a 
     * specific cloning operation. First, if the class of this object does 
     * not implement the interface <tt>Cloneable</tt>, then a 
     * <tt>CloneNotSupportedException</tt> is thrown. Note that all arrays 
     * are considered to implement the interface <tt>Cloneable</tt>. 
     * Otherwise, this method creates a new instance of the class of this 
     * object and initializes all its fields with exactly the contents of 
     * the corresponding fields of this object, as if by assignment; the
     * contents of the fields are not themselves cloned. Thus, this method 
     * performs a "shallow copy" of this object, not a "deep copy" operation.
     * <p>
     * The class <tt>Object</tt> does not itself implement the interface 
     * <tt>Cloneable</tt>, so calling the <tt>clone</tt> method on an object 
     * whose class is <tt>Object</tt> will result in throwing an
     * exception at run time.
     *
     * @return     a clone of this instance.
     * @exception  CloneNotSupportedException  if the object's class does not
     *               support the <code>Cloneable</code> interface. Subclasses
     *               that override the <code>clone</code> method can also
     *               throw this exception to indicate that an instance cannot
     *               be cloned.
     * @see java.lang.Cloneable
     */
	public Object clone() throws CloneNotSupportedException;
}
