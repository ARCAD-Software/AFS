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
package com.arcadsoftware.editor.implementation.swt.binding;

import java.util.ArrayList;

import org.eclipse.core.databinding.UpdateListStrategy;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.runtime.IStatus;

/**
 *
 */
public class TargettoModelTreeUpdateStrategy extends UpdateListStrategy {

	ArrayList<Integer> completed;

	/**
	 * 
	 */
	public TargettoModelTreeUpdateStrategy(ArrayList<Integer> completed) {
		super(true, POLICY_UPDATE);
		this.completed = completed;
	}

	@Override
	protected IStatus doAdd(IObservableList observableList, Object element, int index) {
		int ci = completed.size();
		for (int i = ci - 1; i >= 0; i++) {
			int j = completed.get(i).intValue();
			if (j == index) {
				ci = j;
				completed.set(i, Integer.valueOf(j + 1));
				break;
			} else if (j > index) {
				completed.set(i, Integer.valueOf(j + 1));
			}
		}
		completed.add(ci, Integer.valueOf(index));
		return super.doAdd(observableList, element, ci);
	}

	@Override
	protected IStatus doRemove(IObservableList observableList, int index) {
		int ci = completed.indexOf(Integer.valueOf(index));
		completed.remove(ci);
		for (int i = completed.size() - 1; i >= index; i--) {
			int j = completed.get(i).intValue();
			if (j > -1) {
				completed.set(i, Integer.valueOf(j - 1));
			}
		}
		return super.doRemove(observableList, ci);
	}

}
