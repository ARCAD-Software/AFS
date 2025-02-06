/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
package com.arcadsoftware.afs.client.core.ui.actions;

import java.util.List;

import org.eclipse.jface.action.Action;

/**
 * A list actions interface.
 */
public interface IListActions {

	/**
	 * The list can contains null value, corresponding to separators.
	 *
	 * @return the actions list.
	 */
	public List<Action> getActions();

}
