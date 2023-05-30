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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainerLinkedList;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;

public class BeanMapLinkedCombo extends BeanMapCombo implements IBeanMapContainerLinkedList {

	private String linkCode;
	private MetaDataAttribute sourceAttribute;

	public BeanMapLinkedCombo(Composite parent, int style, String linkCode, MetaDataAttribute sourceAttribute,
			ILayoutParameters parameters, ISWTRenderer renderer, Element element, int horizontalSpan) {
		super(parent, style, parameters, renderer, element, horizontalSpan);
		this.linkCode = linkCode;
		this.sourceAttribute = sourceAttribute;
	}

	public String getLinkCode() {
		return linkCode;
	}

	public MetaDataAttribute getSourceAttribute() {
		return sourceAttribute;
	}

}
