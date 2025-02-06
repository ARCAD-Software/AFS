/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
		setBeanMap(beanMap);
		this.realm = realm;
		this.layoutName = layoutName;
		readOnly = false;
	}

	@Override
	public int getId() {
		return getBeanMap().getId();
	}

	@Override
	public String getType() {
		return getBeanMap().getType();
	}

	@Override
	public boolean exists() {
		return true;
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		return ImageDescriptor.getMissingImageDescriptor();
	}

	@Override
	public String getName() {
		if (getBeanMap() == null) {
			return ""; //$NON-NLS-1$
		}
		return getBeanMap().toString();
	}

	@Override
	public IPersistableElement getPersistable() {
		return new IPersistableElement() {
			@Override
			public String getFactoryId() {
				return ELEMENT_FACTORY_ID;
			}

			@Override
			public void saveState(IMemento memento) {
				MementoSaveBeanMap.save(memento, getBeanMap());
				memento.putString(REALM, realm);
				memento.putString(LAYOUT, layoutName);
			}
		};
	}

	@Override
	public String getToolTipText() {
		return EMPTY;
	}

	@Override
	public <T> T getAdapter(Class<T> adapter) {
		return null;
	}

	@Override
	public String getRealm() {
		return realm;
	}

	@Override
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
