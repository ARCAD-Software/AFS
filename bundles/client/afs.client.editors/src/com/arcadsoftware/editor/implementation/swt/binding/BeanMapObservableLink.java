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
package com.arcadsoftware.editor.implementation.swt.binding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.list.WritableList;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapListEvent;
import com.arcadsoftware.beanmap.BeanMapListTypedEvent;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.implementation.swt.LinkMapWarper;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * Make a link list observable.
 */
public class BeanMapObservableLink extends WritableList<BeanMap> implements IBeanMapListListener {

	private MetaDataLink link;
	private LinkMapWarper warper;
	private boolean populated;
	private boolean autoPopulate;
	private SWTRenderer renderer;
	private String attributeList = null;
	private String orderList = null;
	private ILayoutParameters parameters;
	
	private static String PROP_PAGE_COUNT = "pageCount";

	/**
	 * Create a Link observable.
	 * 
	 * @param link
	 * @param warper
	 */
	public BeanMapObservableLink(MetaDataLink link, LinkMapWarper warper, SWTRenderer renderer,String attributeList) {
		super(new ArrayList<BeanMap>(), BeanMap.class);
		this.link = link;
		this.warper = warper;
		this.renderer = renderer;
		populated = false;
		autoPopulate = true;
		this.attributeList = attributeList;
		this.parameters = null;
	}
	
	public BeanMapObservableLink(MetaDataLink link, LinkMapWarper warper, SWTRenderer renderer, boolean autoPopulate,String attributeList, String orderList) {
		this(link, warper, renderer,autoPopulate, attributeList);
		this.orderList = orderList;
	}
	
	public BeanMapObservableLink(MetaDataLink link, LinkMapWarper warper, SWTRenderer renderer,String attributeList, String orderList) {
		this(link, warper, renderer, attributeList);
		this.orderList = orderList;
	}

	public BeanMapObservableLink(MetaDataLink link, LinkMapWarper warper, SWTRenderer renderer, boolean autoPopulate,String attributeList) {
		this(link, warper, renderer,attributeList);
		this.autoPopulate = autoPopulate;
		this.parameters = null;
	}
	
	public BeanMapObservableLink(MetaDataLink link, LinkMapWarper warper, SWTRenderer renderer, boolean autoPopulate,String attributeList,String orderList, ILayoutParameters parameters) {
		this(link, warper, renderer,autoPopulate, attributeList);
		this.parameters = parameters;
		this.orderList = orderList;
	}

	public MetaDataLink getLink() {
		return link;
	}

	public boolean isPopulated() {
		return populated;
	}

	public boolean isAutoPopulate() {
		return autoPopulate;
	}

	public void setAutoPopulate(boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}

	public void populate() {
		populated = true;
		String pageCount = null;
		if(parameters != null){
			pageCount = parameters.getParameter(PROP_PAGE_COUNT);
		}
		if(pageCount != null){
			warper.loadLinkList(link.getCode(), link.getType(), this,attributeList, orderList,Integer.valueOf(pageCount).intValue());
		}else{
			warper.loadLinkList(link.getCode(), link.getType(), this,attributeList, orderList);	
		}
	}

	public void reset() {
		if (populated) {
			populate();
		}
	}

	private void autoPop() {
		if ((!populated) && autoPopulate) {
			populate();
		}
	}

	@Override
	public boolean contains(Object o) {
		autoPop();
		return super.contains(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		autoPop();
		return super.containsAll(c);
	}

	@Override
	public BeanMap get(int index) {
		autoPop();
		return super.get(index);
	}

	@Override
	public int indexOf(Object o) {
		autoPop();
		return super.indexOf(o);
	}

	@Override
	public boolean isEmpty() {
		autoPop();
		return super.isEmpty();
	}

	@Override
	public Iterator<BeanMap> iterator() {
		autoPop();
		return super.iterator();
	}

	@Override
	public int lastIndexOf(Object o) {
		autoPop();
		return super.lastIndexOf(o);
	}

	@Override
	public ListIterator<BeanMap> listIterator() {
		autoPop();
		return super.listIterator();
	}

	@Override
	public ListIterator<BeanMap> listIterator(int index) {
		autoPop();
		return super.listIterator(index);
	}

	@Override
	public int size() {
		autoPop();
		return super.size();
	}

	@Override
	public List<BeanMap> subList(int fromIndex, int toIndex) {
		autoPop();
		return super.subList(fromIndex, toIndex);
	}

	@Override
	public Object[] toArray() {
		autoPop();
		return super.toArray();
	}

	@Override
	public Object[] toArray(Object[] a) {
		autoPop();
		return super.toArray(a);
	}

	@Override
	public String toString() {
		autoPop();
		return super.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.list.WritableList#add(int, java.lang.Object)
	 */
	@Override
	public void add(int index, BeanMap element) {
		autoPop();
		super.add(index, element);
		if (element != null) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_ADD, (BeanMap) element);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.list.WritableList#add(java.lang.Object)
	 */
	@Override
	public boolean add(BeanMap element) {
		autoPop();
		boolean result = super.add(element);
		if (element != null) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_ADD, (BeanMap) element);
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean addAll(Collection c) {
		autoPop();
		boolean result = super.addAll(c);
		if (c != null) {
			for (Object o : c) {
				warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_ADD, (BeanMap) o);
			}
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean addAll(int index, Collection c) {
		autoPop();
		boolean result = super.addAll(index, c);
		if (c != null) {
			for (Object o : c) {
				warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_ADD, (BeanMap) o);
			}
		}
		return result;
	}

	@Override
	public BeanMap set(int index, BeanMap element) {
		autoPop();
		BeanMap result = super.set(index, element);
		if (result != null) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_REMOVE, (BeanMap) result);
		}
		if (element != null) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_ADD, (BeanMap) element);
		}
		return result;
	}

	@Override
	public BeanMap move(int oldIndex, int newIndex) {
		autoPop();
		return super.move(oldIndex, newIndex);
	}

	@Override
	public BeanMap remove(int index) {
		autoPop();
		BeanMap result = super.remove(index);
		if (result != null) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_REMOVE, (BeanMap) result);
		}
		return result;
	}

	@Override
	public boolean remove(Object o) {
		autoPop();
		boolean result = super.remove(o);
		if (result) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_REMOVE, (BeanMap) o);
		}
		return result;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		autoPop();
		boolean result = super.removeAll(c);
		if (result) {
			for (Object o : c) {
				warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_REMOVE, (BeanMap) o);
			}
		}
		return result;
	}

	@Override
	public void clear() {
		autoPop();
		for (Object o : wrappedList) {
			warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_REMOVE, (BeanMap) o);
		}
		super.clear();
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		autoPop();
		for (Object o : c) {
			if (wrappedList.indexOf(o) == -1)
				warper.updateLinkList(link.getCode(), LinkMapWarper.LINKLIST_REMOVE, (BeanMap) o);
		}
		return super.retainAll(c);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMapListListener#changed(com.arcadsoftware.utils.BeanMapListEvent)
	 */
	public void changed(BeanMapListEvent event) {
		final BeanMapList list = event.getSource();
		final String listType  = (event instanceof BeanMapListTypedEvent) ? ((BeanMapListTypedEvent)event).getType() : null;
		getRealm().asyncExec(new Runnable() {
			@SuppressWarnings("synthetic-access")
			public void run() {
				BeanMapObservableLink.this.updateWrappedList(list);
				if (list != null && list.size() > 0)
					renderer.loadListCompleted(list.get(0).getType());
				else if (listType != null)
					renderer.loadListCompleted(listType);
			}
		});
	}

	/**
	 * @param id
	 */
	public BeanMap find(int id) {
		if (wrappedList != null) {
			for (Object o : wrappedList) {
				if ((o instanceof BeanMap) && (((BeanMap) o).getId() == id)) {
					return (BeanMap) o;
				}
			}
		}
		return null;
	}

	/**
	 * @param item
	 */
	public void updateItem(BeanMap item) {
		for (int i = wrappedList.size() - 1; i >= 0; i--) {
			if (item.getId() == wrappedList.get(i).getId()) {
				checkRealm();
				// Ensure that all Required Attributes are loaded 
				item.addAll(warper.loadLinkedItem(item.getType(), item.getId(), attributeList));		
				
				BeanMap oldElement = wrappedList.set(i, item);
				fireListChange(Diffs.createListDiff(Diffs.createListDiffEntry(i, false, oldElement), Diffs
						.createListDiffEntry(i, true, item)));
				renderer.loadListCompleted(item.getType());
				return;
			}
		}
	}

}
