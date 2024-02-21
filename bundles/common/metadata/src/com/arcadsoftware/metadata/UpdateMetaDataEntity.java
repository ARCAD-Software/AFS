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
package com.arcadsoftware.metadata;

/**
 * This class is declared into this bundle for serializing reasons.
 * 
 * <p>
 * It is used to store updating information about existing Entities.
 * 
 * @author ARCAD Software
 */
public final class UpdateMetaDataEntity extends MetaDataEntity {

	private static final long serialVersionUID = 5032361254115394953L;

	public UpdateMetaDataEntity(String type) {
		super(type);
	}

	public UpdateMetaDataEntity(String type, int version) {
		super(type, version);
	}
}
