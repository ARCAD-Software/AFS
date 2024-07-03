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
package com.arcadsoftware.editor.swt;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.IElementFactory;
import org.eclipse.ui.IMemento;

import com.arcadsoftware.beanmap.BeanMap;

public class ElementFactory implements IElementFactory {

	@Override
	public IAdaptable createElement(IMemento memento) {
		final BeanMap beanMap = MementoSaveBeanMap.load(memento);
		if (beanMap != null) {
			return new BeanMapEditorInput(beanMap, memento.getString(BeanMapEditorInput.REALM),
					memento.getString(BeanMapEditorInput.LAYOUT));
		}
		return null;
	}

}
