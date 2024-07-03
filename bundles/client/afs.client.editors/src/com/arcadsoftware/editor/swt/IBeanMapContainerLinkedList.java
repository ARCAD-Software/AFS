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

import com.arcadsoftware.metadata.MetaDataAttribute;

/**
 * The linked list is a BeanMapList that is build from an association "X" taken form the value of another attribute "Y".
 * In other words the value of this attribute is deducted from the values associated to another attribute.
 */
public interface IBeanMapContainerLinkedList extends IBeanMapContainerList {

	/**
	 * @return the referenced attribute from the current edited entity.
	 */
	public MetaDataAttribute getSourceAttribute();

	/**
	 * @return the link code from the given source attribute. If this association link does not exist then an empty list
	 *         will be binded.
	 */
	public String getLinkCode();
}
