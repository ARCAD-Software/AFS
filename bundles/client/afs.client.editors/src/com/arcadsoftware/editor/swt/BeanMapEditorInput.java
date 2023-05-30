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
package com.arcadsoftware.editor.swt;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * 
 */
public class BeanMapEditorInput implements IBeanMapEditorInput {

	public static final String LAYOUT = "layout"; //$NON-NLS-1$
	public static final String REALM = "realm"; //$NON-NLS-1$
	private static final String ELEMENT_FACTORY_ID = "com.arcadsoftware.editor.ElementFactory"; //$NON-NLS-1$
	private static final String EMPTY = ""; //$NON-NLS-1$

	private BeanMap beanMap;
	protected String realm;
	protected String layoutName;
	protected boolean readOnly;

	/**
	 * @param beanMap
	 */
	public BeanMapEditorInput(BeanMap beanMap) {
		this(beanMap, null, null);
	}

	/**
	 * @param beanMap
	 * @param layoutName
	 */
	public BeanMapEditorInput(BeanMap beanMap, String layoutName) {
		this(beanMap, null, layoutName);
	}

	/**
	 * @param beanMap
	 * @param realm
	 * @param layoutName
	 */
	public BeanMapEditorInput(BeanMap beanMap, String realm, String layoutName) {
		super();
		this.setBeanMap(beanMap);
		this.realm = realm;
		this.layoutName = layoutName;
		this.readOnly = false;
	}

	public int getId() {
		return getBeanMap().getId();
	}

	public String getType() {
		return getBeanMap().getType();
	}

	public boolean exists() {
		return true;
	}

	public ImageDescriptor getImageDescriptor() {
		return ImageDescriptor.getMissingImageDescriptor();
	}

	public String getName() {
		if (getBeanMap() == null) {
			return ""; //$NON-NLS-1$
		}
		return getBeanMap().toString();
	}

	public IPersistableElement getPersistable() {
		return new IPersistableElement() {
			public String getFactoryId() {
				return ELEMENT_FACTORY_ID;
			}

			public void saveState(IMemento memento) {
				MementoSaveBeanMap.save(memento, getBeanMap());
				memento.putString(REALM, realm);
				memento.putString(LAYOUT, layoutName);
			}
		};
	}

	public String getToolTipText() {
		return EMPTY;
	}
	
	@Override
	public <T> T getAdapter(Class<T> adapter) {	
		return null;
	}
	
	public String getRealm() {
		return realm;
	}

	public String getLayoutName() {
		return layoutName;
	}

	public void setBeanMap(BeanMap beanMap) {
		this.beanMap = beanMap;
	}

	public BeanMap getBeanMap() {
		return beanMap;
	}
	@Override
	public boolean equals(Object obj) {
		if ((obj instanceof BeanMapEditorInput) && (((BeanMapEditorInput) obj).getBeanMap() != null)) {
			return ((BeanMapEditorInput) obj).getBeanMap().equals(beanMap);
		}
		return super.equals(obj);
	}

	@Override
	public boolean isReadOnly() {
		return readOnly;
	}
	
	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
