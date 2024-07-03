/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.afs.client.core;

import org.eclipse.core.runtime.ListenerList;

import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionProvider;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMap;

public class BeanMapSelectionManager implements IBeanMapSelectionProvider {

	private static BeanMapSelectionManager instance = new BeanMapSelectionManager();

	public static BeanMapSelectionManager getInstance() {
		return instance;
	}

	private final ListenerList<IBeanMapSelectionListener> beanMapSelectionListener = new ListenerList<>();

	@Override
	public void addBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		beanMapSelectionListener.add(listener);
	}

	@Override
	public void removeBeanMapSelectionListener(IBeanMapSelectionListener listener) {
		beanMapSelectionListener.remove(listener);
	}

	public void fireBeanMapSelection(BeanMap selected) {
		final Object[] list = beanMapSelectionListener.getListeners();
		for (final Object element : list) {
			final IBeanMapSelectionListener listener = (IBeanMapSelectionListener) element;
			try {
				final String[] filters = listener.filterType();
				for (final String filter : filters) {
					if (selected.getType().equalsIgnoreCase(filter)) {
						listener.beanMapSelected(selected);
						break;
					}
				}
			} catch (final Exception e) {
				LogUITools.logError(Activator.getDefault().getBundle(), e);
				beanMapSelectionListener.remove(listener);
			}
		}
	}

}
