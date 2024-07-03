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
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorMatchingStrategy;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.PartInitException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.implementation.Activator;

/**
 * Special Matching strategy for Dynamic Editors.
 */
public class DynamicEditorMatchingStrategy implements IEditorMatchingStrategy {

	public DynamicEditorMatchingStrategy() {
		super();
	}

	@Override
	public boolean matches(IEditorReference editorRef, IEditorInput input) {
		String type = null;
		int id = 0;
		if (input instanceof IBeanMapEditorInput) {
			type = ((IBeanMapEditorInput) input).getType();
			id = ((IBeanMapEditorInput) input).getId();
		} else {
			final BeanMap bm = ((IAdaptable) input).getAdapter(BeanMap.class);
			if (bm != null) {
				type = bm.getType();
				id = bm.getId();
			}
		}
		if ((type == null) || (id == 0)) {
			return false;
		}
		IEditorInput input2;
		try {
			input2 = editorRef.getEditorInput();
		} catch (final PartInitException e) {
			Activator.getInstance().debug(e);
			return false;
		}
		String type2 = null;
		int id2 = 0;
		if (input2 instanceof IBeanMapEditorInput) {
			type2 = ((IBeanMapEditorInput) input2).getType();
			id2 = ((IBeanMapEditorInput) input2).getId();
		} else {
			final BeanMap bm = ((IAdaptable) input2).getAdapter(BeanMap.class);
			if (bm != null) {
				type2 = bm.getType();
				id2 = bm.getId();
			}
		}
		if ((type2 == null) || (id2 == 0)) {
			return false;
		}
		return type.equals(type2) && (id == id2);
	}

}
