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
package com.arcadsoftware.afs.client.core.servers.model;

import java.util.ArrayList;

import com.arcadsoftware.aev.core.contentproviders.IObjectArrayProvider;


public class Servers extends ArrayList<Server> implements IObjectArrayProvider{

	private static final long serialVersionUID = -5129435433777882698L;

	public Servers(int initialCapacity) {
		super(initialCapacity);
	}

	public Servers() {
		super();
	}

	public Object[] getObjectArray() {
		return toArray();
	}
}
