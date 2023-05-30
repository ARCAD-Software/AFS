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

import org.eclipse.core.databinding.observable.Realm;

import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapListEvent;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.editor.implementation.swt.IListContainer;

/**
 * Listener used to load a list of beanMap.
 * 
 * <p>
 * the loading process is realized into another thread to not hang the GUI.  
 */
public class BindingListLoadRunnable implements IBeanMapListListener {

	private final Realm realm;
	private final IListContainer container;
	
	public BindingListLoadRunnable(Realm realm, IListContainer container) {
		super();
		this.realm = realm;
		this.container = container;
	}

	public void changed(BeanMapListEvent event) {
		// Called in the thread of the http request.
		final BeanMapList list = event.getSource();
		realm.asyncExec(new Runnable(){
			public void run() {
				// Called in the thread of the Binding !
				container.load(list);
			}
		});
	}

}
