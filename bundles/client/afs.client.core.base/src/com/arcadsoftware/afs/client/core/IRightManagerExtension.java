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
package com.arcadsoftware.afs.client.core;

import java.util.List;

public interface IRightManagerExtension {

	public List<Integer> getExpectedRights(String action);
	public List<Integer> getExpectedRights(String action, boolean ignoreIfUndefined);
	public void missingRight(List<Integer> expected);
	
}
