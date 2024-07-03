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
package com.arcadsoftware.editor.implementation.swt.binding;

import java.util.ArrayList;

import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.observable.masterdetail.IObservableFactory;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;

/**
 *
 */
public class BeanMapObservableLinkTreeFactory implements IObservableFactory {

	SWTRenderer renderer;
	BeanMapObservableLink observableLink;
	String fatherCode;

	/**
	 * @param renderer
	 * @param observableLink
	 * @param fatherCode
	 */
	public BeanMapObservableLinkTreeFactory(SWTRenderer renderer, BeanMapObservableLink observableLink,
			String fatherCode) {
		super();
		this.observableLink = observableLink;
		this.renderer = renderer;
		this.fatherCode = fatherCode;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.core.databinding.observable.masterdetail.IObservableFactory#createObservable(java.lang.Object)
	 */
	@Override
	public IObservable createObservable(Object target) {
		if (target instanceof IIdentifiedBean) {
			final WritableList result = new WritableList(new ArrayList<BeanMap>(), BeanMap.class);
			final ArrayList<Integer> cl = new ArrayList<>();
			renderer.getRendererBinding().getBinding().bindList(result, observableLink,
					new TargettoModelTreeUpdateStrategy(cl), new ModeltoTargetTreeUpdateStrategy(observableLink,
							fatherCode, ((IIdentifiedBean) target).getId(), cl));
			return result;
		}
		return null;
	}

}
