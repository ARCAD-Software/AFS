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
package com.arcadsoftware.editor.swt.renderer;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.implementation.swt.binding.BeanMapObservableValue;
import com.arcadsoftware.editor.swt.IBeanMapContainer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * This interface permits renderer manages binding.
 */
public interface IRendererBinding {

	/**
	 * Refresh a linked BeanMap.
	 *
	 * @param item
	 *            the beanMap item.
	 */
	public void refreshBean(BeanMap item);

	/**
	 * Populate the enumaration list. See <code>getObservableList</code> for details.
	 * <p>
	 * Populate an already populated list will cause a list refresh.
	 *
	 * @deprecated Must be verify the method is correct.
	 * @param type
	 *            The enumaration list Type to be populated.
	 */
	@Deprecated
	public void populateList(String type);

	/**
	 * Return the root sub list of the specified links list. *
	 *
	 * @deprecated Must be verify the method is correct.
	 * @param link
	 *            the entity link.
	 * @param fatherCode
	 *            the father code
	 * @return an IObservable implementation.
	 */
	@Deprecated
	public IObservable getTreeRootCollection(MetaDataLink link, String fatherCode);

	/**
	 * Get an IObservable usable with the <code>ContextBinding</code>.
	 *
	 * @param element
	 *            the attribute entity
	 * @return an IObservableValue implementation.
	 */
	public IObservableValue getObservableAttribute(MetaDataAttribute element);

	/**
	 * Get an IObservable usage with the <code>ContextBinding</code> to link a selection into an enumeration list.
	 * <p>
	 * This list represent the whole enumeration list. It is updated only if the enumeration list is updated on the
	 * server.
	 * <p>
	 * This list can be affected to a Viewer if the content provider of this viewer is an instance of
	 * <code>org.eclipse.jface.databinding.viewers.ObservableListContentProvider</code>.
	 *
	 * @param type
	 *            the element type.
	 * @param autoPopulate
	 *            the autoPopulate value.
	 * @return an IObservableList implementation.
	 */
	public IObservableList getObservableList(String type, boolean autoPopulate);

	/**
	 * Populate the link list element. See <code>getObservableLink</code> for details.
	 * <p>
	 * Populate an already populated link will cause a list refresh.
	 *
	 * @deprecated Must be verify the method is correct.
	 * @param linkName
	 *            The link element name to be populated.
	 */
	@Deprecated
	public void populateLink(String linkName);

	/**
	 * @param code
	 *            the attribute code
	 * @return a BeanMapObservableValue implementation.
	 */
	public BeanMapObservableValue getObservableAttribute(String code);

	/**
	 * Execute a Rihno (JavaScript) script. The list of attributes codes is the impacted attributes or links that may be
	 * used into the script.
	 *
	 * @param elements
	 *            a list on Attributes and Links codes.
	 * @param script
	 *            the script
	 * @return true is execution is complete, false otherwise.
	 */
	public boolean execute(String[] elements, String script);

	/**
	 * Import the value of the given BeanMap into the current editing one.
	 *
	 * @param beanMap
	 */
	public void importAttributes(IBeanMap beanMap);

	/**
	 * @return The JFace Data Binding Context usable in this editor.
	 */
	public DataBindingContext getBinding();

	/**
	 * Get an IObservable usable with the <code>ContextBinding</code>.
	 *
	 * @param element
	 *            The link to observe.
	 * @param autoPopulate
	 *            If true the list will be populate as soon as the first binding need it. If false, le list will rest
	 *            empty until <code>populateLink</code> is called.
	 * @param attributeList
	 *            a list of attributes that must be read. If null all the attributes defined in the metadata will be
	 *            read.
	 * @return An IObservableList implementation
	 * @see #getBinding()
	 */
	public IObservableList getObservableLink(MetaDataLink element, boolean autoPopulate, String attributeList);

	/**
	 * Create a default Data binding between a Data element (attribute or list) and a widget.
	 *
	 * @param element
	 * @param widget
	 * @throws IllegalArgumentException
	 */
	public void bindElement(Element element, Control widget) throws IllegalArgumentException;

	/**
	 * Create a default Data binding between a Data element (attribute or list) and a Selection Provider.
	 *
	 * @param element
	 * @param selection
	 * @throws IllegalArgumentException
	 */
	public void bindElement(Element element, ISelectionProvider selection) throws IllegalArgumentException;

	/**
	 * Create a default Data Binding between a Data element (attribute or list) and a BeanMap Container.
	 *
	 * @param element
	 * @param container
	 * @throws IllegalArgumentException
	 */
	public void bindElement(Element element, IBeanMapContainer container) throws IllegalArgumentException;

	/**
	 * Create a default Data Binding between a Data element (attribute or list) and a BeanMap Container.
	 *
	 * @param element
	 * @param container
	 * @param parameters
	 * @throws IllegalArgumentException
	 */
	public void bindElement(Element element, IBeanMapContainer container, ILayoutParameters parameters)
			throws IllegalArgumentException;

	/**
	 * Create a default Data binding between a Data element (attribute or list) and a widget.
	 *
	 * @param element
	 *            the element to bind.
	 * @param widget
	 *            the widget to bind.
	 * @throws IllegalArgumentException
	 */
	public void bindElement(Element element, IWidgetValue widget) throws IllegalArgumentException;

}
