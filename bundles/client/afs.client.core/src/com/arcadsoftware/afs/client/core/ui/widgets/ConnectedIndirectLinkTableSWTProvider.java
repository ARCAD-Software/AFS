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
/**
 * This class manages widget table to display indirect links.
 * For instance, if current beanMap has an attribute 'type' and this type has a link 'properties',
 * this widget table is able to show the list of properties linked to the type.
 */
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTableViewer;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

public class ConnectedIndirectLinkTableSWTProvider extends ConnectedTableWithUserDefinedButtonBarSWTProvider
		implements IWidgetValue {

	public static final String ATTRIBUTE_REFLINK = "refLink"; //$NON-NLS-1$
	MetaDataAttribute attribute;

	@Override
	public void create(ISWTRenderer swtRenderer,
			ILayoutParameters layoutParameters, Element element,
			MetaDataEntity structure) {
		attribute = (MetaDataAttribute) element;
		renderer = swtRenderer;
		readOnlyEdition = true;

		final String refLink = layoutParameters.getParameter(ATTRIBUTE_REFLINK);
		final MetaDataEntity entity = getHelper().getEntity(attribute.getType());
		final MetaDataLink linkElement = entity.getLink(refLink);
		super.create(swtRenderer, layoutParameters, linkElement, entity);

		bindElement();
	}

	@Override
	protected void createControlAfterTable(Composite parent) {
		super.createControlAfterTable(parent);
	}

	@Override
	protected void bindElement() {
		renderer.getRendererBinding().bindElement(attribute, this);
	}

	@Override
	public Control getWidget() {
		return getList().getTable();
	}

	@Override
	public Object getValue() {
		final BeanMap currentBeanMap = renderer.getCurrentBean();
		final int attributeId = currentBeanMap.getInt(attribute.getCode());
		if (attributeId > 0) {
			return new BeanMap(attribute.getType(), attributeId);
		}
		return null;
	}

	@Override
	public void addSelectionListener(SelectionListener selectionListener) {

	}

	@Override
	public void setValue(Object newValue) {
		if (newValue instanceof BeanMap) {
			final BeanMap bm = (BeanMap) newValue;
			final MetaDataLink link = getLink();
			final BeanMapTableViewer viewer = getViewer();
			// load links
			final BeanMapList list = getHelper().getLinkList(bm.getType(), bm.getId(), link.getCode(), link.getType(),
					viewer.getAttributeList(), viewer.getOrderList());
			getList().setBeanMapList(list);
		}
	}

	@Override
	public Object getValueType() {
		return BeanMap.class;
	}

}
