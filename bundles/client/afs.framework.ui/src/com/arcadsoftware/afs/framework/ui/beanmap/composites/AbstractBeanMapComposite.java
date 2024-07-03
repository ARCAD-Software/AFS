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
package com.arcadsoftware.afs.framework.ui.beanmap.composites;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSStandardComposite;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractBeanMapComposite extends AbstractAFSStandardComposite {

	private final BeanMapControler controler;

	public AbstractBeanMapComposite(Composite parent, int style) {
		super(parent, style);
		controler = new BeanMapControler(this);
	}

	public AbstractBeanMapComposite(Composite parent, int style, BeanMap original) {
		this(parent, style);
		setBeanMap(original);
	}

	protected BeanMapControler getControler() {
		return controler;
	}

	public void setBeanMap(BeanMap original) {
		controler.setBeanMap(original);
	}

	public void bindControl(String attribute, Control c) {
		controler.bindControl(attribute, c);
	}

	public BeanMap getUpdated() {
		return controler.getUpdated();
	}

	public abstract void bindControls();

}
