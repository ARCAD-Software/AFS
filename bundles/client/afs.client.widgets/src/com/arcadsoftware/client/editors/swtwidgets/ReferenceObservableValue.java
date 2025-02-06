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
package com.arcadsoftware.client.editors.swtwidgets;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.IBeanMapListener;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.client.editors.swtwidgets.decorators.ReferenceLabelSWTProvider;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class ReferenceObservableValue extends AbstractObservableValue<Object> {

	private class RecursiveLoadingListener implements Runnable, IBeanMapListener {

		private int level;
		private final int currentLoading;
		private final ReferenceLabelSWTProvider currentProvider;
		private IBeanMap current;

		public RecursiveLoadingListener(ReferenceLabelSWTProvider provider, int loading) {
			currentLoading = loading;
			currentProvider = provider;
		}

		@Override
		public void changed(BeanMapEvent event) {
			if (loading == currentLoading) {
				current = event.getSource();
				new Thread(this).start();
			}
		}

		@Override
		public void run() {
			while (!currentProvider.isStructureLoaded()) {
				try {
					Thread.sleep(500);
				} catch (final InterruptedException e) {
					MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
				}
				if (currentProvider.isDisposed() || (loading != currentLoading)) {
					return;
				}
			}
			if (loading != currentLoading) {
				return;
			}
			if (currentProvider.isReady()) {
				if (level >= (currentProvider.getStructures().length - 1)) {
					// last attribute from the chain.
					currentProvider.format(current);
				} else {
					final MetaDataEntity s = currentProvider.getStructures()[level];
					if (s == null) {
						return;
					}
					level++;
					final MetaDataAttribute a = s.getAttribute(currentProvider.getAttributes()[level]);
					if (a == null) {
						return;
					}
					final Object o = (current).get(a.getCode());
					if (o != null) {
						int i = 0;
						if (o instanceof Integer) {
							i = ((Integer) o).intValue();
						} else if (o instanceof String) {
							try {
								i = Integer.parseInt((String) o);
							} catch (final NumberFormatException e) {
								MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
							}
						} else if (o instanceof IIdentifiedBean) {
							i = ((IIdentifiedBean) o).getId();
						}
						if (i != 0) {
							currentProvider.getRenderer().loadBeanMap(a.getType(), i, this);
						}
					}
				}
			}
		}
	}

	private Object value;
	private final ReferenceLabelSWTProvider provider;
	int loading;
	private String rootType;

	public ReferenceObservableValue(ReferenceLabelSWTProvider provider) {
		this(Realm.getDefault(), provider);
	}

	public ReferenceObservableValue(Realm realm, ReferenceLabelSWTProvider provider) {
		super(realm);
		this.provider = provider;
		final MetaDataAttribute a = provider.getRootStructure().getAttribute(provider.getAttributes()[0]);
		if (a != null) {
			rootType = a.getType();
		} else {
			rootType = null;
		}
	}

	@Override
	protected Object doGetValue() {
		return value;
	}

	@Override
	public Object getValueType() {
		return BeanMap.class;
	}

	@Override
	protected void doSetValue(Object newValue) {
		value = newValue;
		if (value instanceof BeanMap) {
			doRunReferencesLoading((BeanMap) value);
		}
	}

	private void doRunReferencesLoading(BeanMap beanMap) {
		if (rootType == null) {
			return;
		}
		loading++; // sill shutdown any pending currentLoading.
		provider.resetLabel();
		provider.getRenderer().loadBeanMap(rootType, beanMap.getId(), new RecursiveLoadingListener(provider, loading));
	}
}
