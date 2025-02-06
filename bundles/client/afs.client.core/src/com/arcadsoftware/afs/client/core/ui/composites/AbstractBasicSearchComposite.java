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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.DynamicEditorComposite;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.ContainCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;

public abstract class AbstractBasicSearchComposite extends AbstractSearchComposite {

	private static final String FALSE_INT_VALUE = "0"; //$NON-NLS-1$
	private static final String TRUE_INT_VALUE = "1"; //$NON-NLS-1$
	protected static final String AND_END_TAG = "</and>"; //$NON-NLS-1$
	protected static final String AND_TAG = "<and>"; //$NON-NLS-1$
	protected DynamicEditorComposite composite;
	protected Composite searchCompositeParent;

	public AbstractBasicSearchComposite(Composite parent, MetaDataEntity structure, ServerConnection connection) {
		super(parent, structure, connection);
	}

	protected boolean addSearchClause(String attributeCode) {
		return true;
	}

	public BeanMap getSearchBeanMap() {
		return composite.getRenderer().getCurrentBean();
	}

	public void putSearchAttribute(String key, Object value) {
		composite.getRenderer().put(key, value);
	}

	@Override
	protected String createSearchClause() {
		final BeanMap beanMap = getSearchBeanMap();
		if (beanMap != null) {
			final ArrayList<ISearchCriteria> stdCriteriaList = new ArrayList<>();
			final ArrayList<String> stringCriteriaList = new ArrayList<>();
			for (final Map.Entry<String, Object> entry : beanMap.entrySet()) {
				if (addSearchClause(entry.getKey())) {
					final Object o = entry.getValue();
					if (o instanceof String) {
						final String value = ((String) entry.getValue()).trim();
						if (value.length() > 0) {
							final ContainCriteria c = new ContainCriteria(entry.getKey(), value);
							stdCriteriaList.add(c);
						}
					} else if (o instanceof Integer) {
						final String value = (((Integer) entry.getValue()).toString());
						if (value.length() > 0) {
							final EqualCriteria c = new EqualCriteria(entry.getKey(), value);
							stdCriteriaList.add(c);
						}
					} else if (o instanceof Boolean) {
						final Boolean value = (Boolean) entry.getValue();
						final EqualCriteria c = new EqualCriteria(entry.getKey(), value.booleanValue() ? TRUE_INT_VALUE
								: FALSE_INT_VALUE);
						stdCriteriaList.add(c);
					} else if (o instanceof BeanMap) {
						final String value = completeClauseForLinkedBeanMap((BeanMap) entry.getValue());
						if (!value.isEmpty()) {
							stringCriteriaList.add(value);
						}
					} else if (o == null) {
						final IsNullCriteria c = new IsNullCriteria(entry.getKey());
						stdCriteriaList.add(c);
					}
				}
			}

			StringBuilder builder = null;
			final XmlCriteriaStream x = new XmlCriteriaStream();
			for (final ISearchCriteria c : stdCriteriaList) {
				if (builder == null) {
					builder = new StringBuilder();
				}
				builder.append(x.toXML(c));
			}
			for (final String element : stringCriteriaList) {
				if (builder == null) {
					builder = new StringBuilder();
				}
				builder.append(element);
			}
			final StringBuilder result = new StringBuilder();
			if (builder != null) {
				result.append(AND_TAG);
				result.append(builder);
				result.append(AND_END_TAG);
			}
			return result.toString();
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	protected void createSearchContents(Composite parent) {
		searchCompositeParent = parent;
		createSearchPart(parent);
	}

	/**
	 * Search Part creation If this method is overriden and DynamicEditorComposite composite is not use, clearSearchPart
	 * must also be overriden
	 */
	protected void createSearchPart(Composite parent) {
		if (composite != null) {
			composite.dispose();
		}
		composite = new ConnectedDynamicEditorComposite(connection, parent, SWT.NONE, getType(), getLayoutName()) {
			@Override
			protected ServerConnection getConnection() {
				return connection;
			}
		};
		composite.loadEmptyEntity();
		if (composite.getParent().getLayout() instanceof GridLayout) {
			final GridData gridData = new GridData(GridData.FILL_BOTH);
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			gridData.horizontalSpan = 3;
			composite.setLayoutData(gridData);
		}
	}

	@Override
	public void clearAll() {
		clearSearchPart();
		createSearchContents(searchCompositeParent);
		searchCompositeParent.layout(true);
	}

	/**
	 * This method will be overriden if createSearchPart is overriden and DynamicEditorComposite composite is not used
	 * as the main composite for search editor
	 */
	protected void clearSearchPart() {
	}

	protected String completeClauseForLinkedBeanMap(BeanMap beanMap) {
		return ""; //$NON-NLS-1$
	}

	public abstract String getType();

	public abstract String getLayoutName();

}
