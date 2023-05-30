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

import java.text.ParseException;
import java.util.Date;

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.value.AbstractVetoableValue;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.beanmap.IBeanMapListener;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.editor.implementation.swt.BeanMapWarper;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.osgi.ISODateFormater;

/**
 * Observable warper around a single BeanMap attribute.
 */
public class BeanMapObservableValue extends AbstractVetoableValue implements IBeanMapListener {

	private static final String _0 = "0"; //$NON-NLS-1$
	private static final String TRUE = "true"; //$NON-NLS-1$
	private static final String FALSE = "false"; //$NON-NLS-1$
	public static final String TYPE_BASIC_INTEGER = "integer"; //$NON-NLS-1$
	public static final String TYPE_BASIC_STRING = "string"; //$NON-NLS-1$
	public static final String TYPE_BASIC_BOOLEAN = "boolean"; //$NON-NLS-1$
	public static final String TYPE_BASIC_FLOAT = "float"; //$NON-NLS-1$
	public static final String TYPE_BASIC_DATE = "date"; //$NON-NLS-1$
	public static final String TYPE_BASIC_ICON = "icon"; //$NON-NLS-1$

	private MetaDataAttribute attribute;
	private BeanMapWarper warper;
	private String type;
	private SWTRenderer renderer;
	private BeanMap loadedBeanMap = null;
	private BeanMap tempLoadedBeanMap = null;
	private int currentLoading = 0;

	/**
	 * @param attribute
	 * @param warper
	 */
	public BeanMapObservableValue(SWTRenderer renderer, MetaDataAttribute attribute, BeanMapWarper warper) {
		super();
		this.attribute = attribute;
		type = attribute.getType();
		if (type == null) {
			type = TYPE_BASIC_STRING;
		}
		this.warper = warper;
		this.renderer = renderer;
		// Listener the validating condition.
		if ((attribute.getTest() != null) && (attribute.getTest().length() > 0)) {
			addValueChangingListener(new ScriptableChangingAttribute(attribute));
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.databinding.observable.value.AbstractVetoableValue# doSetApprovedValue(java.lang.Object)
	 */
	@Override
	protected void doSetApprovedValue(Object value) {
		if (value instanceof BeanMap) {
			// The edited beanMap only store the reference ID !
			warper.put(attribute.getCode(), Integer.valueOf(((BeanMap) value).getId()));
			tempLoadedBeanMap = (BeanMap) value;
		} else {

			warper.put(attribute.getCode(), value);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.value.AbstractObservableValue #doGetValue()
	 */
	@Override
	protected Object doGetValue() {
		Object obj = warper.get(attribute.getCode());
		if (obj == null) {
			if (type.equals(TYPE_BASIC_BOOLEAN)) {
				return Boolean.valueOf(false);
			}
			return null;
		}
		// ISO Dates conversion...
		if ((type.equals(TYPE_BASIC_DATE)) && (obj instanceof String)) {
			try {
				return ISODateFormater.toDate((String) obj);
			} catch (ParseException e) {
				// Nothing to do here.
			}
		}
		// Boolean conversion...
		if (type.equals(TYPE_BASIC_BOOLEAN)) {
			if (obj instanceof Integer) {
				return Boolean.valueOf(((Integer) obj).intValue() != 0);
			}
			if (obj instanceof String) {
				if (((String) obj).equalsIgnoreCase(TRUE)) {
					return Boolean.valueOf(true);
				}
				if (((String) obj).equalsIgnoreCase(FALSE)) {
					return Boolean.valueOf(false);
				}
				return Boolean.valueOf(!_0.equals(obj));
			}
		}
		// Rounded float values
		if ((obj instanceof Float) && type.equals(TYPE_BASIC_INTEGER)) {
			return Integer.valueOf(((Float) obj).intValue());
		}
		// string values
		if (type.equals(TYPE_BASIC_STRING) || type.equals(TYPE_BASIC_ICON)) {
			return obj.toString();
		}
		// Reference to another entity (BeanMap).
		// We should only return the entity reference Id but to accelerate the
		// widget rendering we replace this reference with a BeanMap object.
		
		MetaDataEntity refEntity = renderer.getEditorLoader().loadMetaDataEntity(attribute.getType());
		if (refEntity!=null) {		
		//if (attribute.isReference()) {
			int id = 0;
			if (obj instanceof IIdentifiedBean) {
				id = ((IIdentifiedBean) obj).getId();
			} else if (obj instanceof Integer) {
				id = ((Integer) obj).intValue();
			} else if (obj instanceof String) {
				try {
					id = Integer.parseInt((String) obj);
				} catch (NumberFormatException e) {
					// Nothing to do here.
				}
			}
			if (id == 0) {
				return null;
			}
			if ((loadedBeanMap != null) && (loadedBeanMap.getId() == id)) {
				return loadedBeanMap;
				// TODO Sometimes we should refresh this local cache !!
			}
			if (currentLoading != id) {
				currentLoading = id;
				renderer.loadBeanMap(type, id, this);
			}
			if ((loadedBeanMap != null) && (loadedBeanMap.getId() == id)) {
				return loadedBeanMap;
			}
			// Return an temporary BeanMap (while we load it !)
			if ((tempLoadedBeanMap != null) && (tempLoadedBeanMap.getId() == id)) {
				return tempLoadedBeanMap;
			}
			return new BeanMap(attribute.getType(), id);
		}
		// The data binding will try to convert the value...
		return obj;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.value.IObservableValue#getValueType ()
	 */
	public Object getValueType() {
		if (type.equals(TYPE_BASIC_INTEGER)) {
			return Integer.class;
		}
		if (type.equals(TYPE_BASIC_STRING) || type.equals(TYPE_BASIC_ICON)) {
			return String.class;
		}
		if (type.equals(TYPE_BASIC_DATE)) {
			return Date.class;
		}
		if (type.equals(TYPE_BASIC_FLOAT)) {
			return Double.class;
		}
		if (type.equals(TYPE_BASIC_BOOLEAN)) {
			return Boolean.class;
		}
		// TODO WikiText !!!
		// TODO Icones et objets binaires !!!
		return BeanMap.class;
	}

	public void fireInitialization() {
		getRealm().asyncExec(new Runnable() {
			@SuppressWarnings("synthetic-access")
			public void run() {
				Object nv = doGetValue();
				Object ov = null;
				if (nv == null) {
					ov = new Object();
				}
				fireValueChange(Diffs.createValueDiff(ov, nv));
			}
		});
	}

	public MetaDataAttribute getAttribute() {
		return attribute;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMapListener#changed(com.arcadsoftware.utils .BeanMapEvent)
	 */
	public void changed(BeanMapEvent event) {
		final BeanMap beanMap;
		if (event != null) {
			if (event.getSource() instanceof BeanMap) {
				beanMap = (BeanMap) event.getSource();
			} else {
				beanMap = new BeanMap(event.getSource());
			}
		} else {
			beanMap = null;
		}
		getRealm().asyncExec(new Runnable() {

			@SuppressWarnings("synthetic-access")
			public void run() {
				BeanMap old = BeanMapObservableValue.this.loadedBeanMap;
				BeanMapObservableValue.this.loadedBeanMap = beanMap;
				BeanMapObservableValue.this.tempLoadedBeanMap = beanMap;
				BeanMapObservableValue.this.fireValueChange(Diffs.createValueDiff(old, beanMap));
			}

		});
	}

}
