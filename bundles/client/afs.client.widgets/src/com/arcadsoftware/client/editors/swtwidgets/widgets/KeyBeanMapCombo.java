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
/**
 * This class manages list of BeanMaps that have no IDs.
 * These BranMaps come from services that manage dynamic beanMaps and where 1 attribute is kused as an identifier.
 */
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

public class KeyBeanMapCombo extends BeanMapCombo{

	private static final String ATTRIBUTE_KEY = "key";
	private String keyAttribute;

	public KeyBeanMapCombo(Composite parent, int style, ILayoutParameters parameters, ISWTRenderer renderer,
			MetaDataEntity structure, int horizontalSpan) {
		super(parent, style, parameters, renderer, structure, horizontalSpan);
		keyAttribute = parameters.getParameter(ATTRIBUTE_KEY);
	}
		
	/**
	 * Create a BeanMapCombo out of dynamic editors: no renderer, no translate
	 * @param parent
	 * @param style
	 * @param format
	 * @param translate
	 * @param renderer
	 * @param element
	 * @param horizontalSpan
	 */
	public KeyBeanMapCombo(Composite parent, int style, String format, MetaDataEntity structure,
			String label) {
		super(parent, style, format, structure, label);
	}

	@Override
	public void setBeanMapList(BeanMapList list) {
		if (list == null)
			list = new BeanMapList();
		
		this.list = list;
		String[] items = new String[list.size() + 1];
		items[0] = ""; //$NON-NLS-1$
		Object curid = 0;
		if (current != null) {
			curid = current.get(keyAttribute);
		}
		int selection = -1;
		for (int i = 0; i < list.size(); i++) {
			BeanMap beanMap = list.get(i);
			items[i + 1] = beanMaptoString(beanMap);
			if (beanMap.get(keyAttribute)!=null && beanMap.get(keyAttribute).equals(curid)) {
				selection = i + 1;
			}
		}
		combo.setItems(items);
		// Force the selected value to current...
		if (selection > -1) {
			combo.select(selection);
		} else if (current != null) {
			combo.setText(beanMaptoString(current));
		}
	}
	
	@Override
	public void setBeanMapValue(BeanMap beanMap) {
		if (beanMap == null) {
			current = null;
			combo.setText(""); //$NON-NLS-1$
		} else if (!beanMap.equals(current)) {
			current = beanMap;
			if (list != null) {				
				int i = list.getFirstIndex(keyAttribute, beanMap.get(keyAttribute));
				if (i >= 0) {
					combo.select(i + 1);
					return;
				}
			}
			combo.setText(beanMaptoString(beanMap));
		}
	}
}
