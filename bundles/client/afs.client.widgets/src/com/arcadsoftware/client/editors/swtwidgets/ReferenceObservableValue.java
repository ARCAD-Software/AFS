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

	private Object value;
	private ReferenceLabelSWTProvider provider;
	int loading = 0;
	private String rootType;

	public ReferenceObservableValue(ReferenceLabelSWTProvider provider) {
		this(Realm.getDefault(), provider);
	} 

	public ReferenceObservableValue(Realm realm, ReferenceLabelSWTProvider provider) {
		super(realm);
		this.provider = provider;
		MetaDataAttribute a = provider.getRootStructure().getAttribute(provider.getAttributes()[0]);
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.value.IObservableValue#getValueType()
	 */
	public Object getValueType() {
		return BeanMap.class;
	}

	@Override
	protected void doSetValue(Object newValue) {
		this.value = newValue;
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

	private class RecursiveLoadingListener implements Runnable, IBeanMapListener {

		private int level = 0;
		private int currentLoading;
		private ReferenceLabelSWTProvider currentProvider;
		private IBeanMap current;

		public RecursiveLoadingListener(ReferenceLabelSWTProvider provider, int loading) {
			this.currentLoading = loading;
			this.currentProvider = provider;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.arcadsoftware.utils.IBeanMapListener#changed(com.arcadsoftware.utils.BeanMapEvent)
		 */
		public void changed(BeanMapEvent event) {
			if (ReferenceObservableValue.this.loading == currentLoading) {
				current = event.getSource();
				new Thread(this).start();
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Runnable#run()
		 */
		public void run() {
			while (!currentProvider.isStructureLoaded()) {
				try {
					Thread.sleep(500);
				}
				catch (InterruptedException e) {
					MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
				}
				if (currentProvider.isDisposed()) {
					return;
				}
				if (ReferenceObservableValue.this.loading != currentLoading) {
					return;
				}
			}
			if (ReferenceObservableValue.this.loading != currentLoading) {
				return;
			}
			if (currentProvider.isReady()) {
				if (level >= (currentProvider.getStructures().length - 1)) {
					// last attribute from the chain.
					currentProvider.format(current);
				} else {
					MetaDataEntity s = currentProvider.getStructures()[level];
					if (s == null) {
						return;
					}
					level++;
					MetaDataAttribute a = s.getAttribute(currentProvider.getAttributes()[level]);
					if (a == null) {
						return;
					}
					Object o = (current).get(a.getCode());
					if (o != null) {
						int i = 0;
						if (o instanceof Integer) {
							i = ((Integer) o).intValue();
						} else if (o instanceof String) {
							try {
								i = Integer.parseInt((String) o);
							} catch (NumberFormatException e) {
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

}
