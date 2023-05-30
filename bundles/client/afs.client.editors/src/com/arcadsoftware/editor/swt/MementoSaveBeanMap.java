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

import java.text.ParseException;
import java.util.Date;
import java.util.Map.Entry;

import org.eclipse.ui.IMemento;


import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.implementation.Activator;
import com.arcadsoftware.osgi.ISODateFormater;

/**
 * This class permit to save and load a beanMap on a memento.
 */
public class MementoSaveBeanMap {

	private static final String DATE = "date"; //$NON-NLS-1$
	private static final String TYPETYPE = "typetype"; //$NON-NLS-1$
	private static final String IDID = "idid"; //$NON-NLS-1$

	private MementoSaveBeanMap() {
		// Do nothing
	}

	/**
	 * Save the <code>beanMap</code> on a <code>memento</code>.
	 * 
	 * @param memento
	 *            The memento.
	 * @param beanMap
	 *            The beanMap to be saved.
	 */
	public static void save(IMemento memento, BeanMap beanMap) {
		if (beanMap != null) {
			memento.putString(TYPETYPE, beanMap.getType());
			memento.putInteger(IDID, beanMap.getId());
			if (beanMap.getDate() != null) {
				memento.putString(DATE, ISODateFormater.toString(beanMap.getDate()));
			}
			for (Entry<String, Object> entry : beanMap.entrySet()) {
				if (entry.getValue() instanceof BeanMap) {
					memento.putString(entry.getKey(), Integer.toString(((BeanMap) entry.getValue()).getId()));
				} else if (entry.getValue() instanceof Date) {
					memento.putString(entry.getKey(), ISODateFormater.toString((Date) entry.getValue()));
				} else if (entry.getValue() != null) {
					memento.putString(entry.getKey(), entry.getValue().toString());
				}
			}
		}
	}

	/**
	 * Returns the beanMap saved in the given <code>memento</code>.
	 * 
	 * @param memento
	 *            The memento.
	 * @return The saved beanMap.
	 */
	public static BeanMap load(IMemento memento) {
		BeanMap beanMap = null;
		if (memento != null) {
			String type = memento.getString(TYPETYPE);
			Integer id = memento.getInteger(IDID);
			if (type != null && id != null) {
				beanMap = new BeanMap(type, id.intValue());
				String dateString = memento.getString(DATE);
				if (dateString != null) {
					try {
						Date date = ISODateFormater.toDate(dateString);
						beanMap.setDate(date);
					} catch (ParseException e) {
						Activator.getInstance().log(e);
					}
				}
				for (String key: memento.getAttributeKeys()) {
					beanMap.put(key, memento.getString(key));
				}
			}
		}
		return beanMap;
	}
	
}
