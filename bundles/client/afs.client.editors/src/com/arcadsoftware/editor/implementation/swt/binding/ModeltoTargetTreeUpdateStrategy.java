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
import org.eclipse.core.runtime.Status;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;

/**
 * Filter the Observable list.
 */
public class ModeltoTargetTreeUpdateStrategy extends UpdateListStrategy {

	private String fatherCode;
	private int fatherId;
	private BeanMapObservableLink sourceLink;
	ArrayList<Integer> completed;

	public ModeltoTargetTreeUpdateStrategy(BeanMapObservableLink sourceLink, String fatherCode, int fatherId,
			ArrayList<Integer> completed) {
		super(true, POLICY_UPDATE);
		this.fatherCode = fatherCode;
		this.fatherId = fatherId;
		this.sourceLink = sourceLink;
		this.completed = completed;
	}

	private boolean accept(BeanMap element) {
		Object father = element.get(fatherCode);
		int fid = 0;
		if (father != null) {
			if (father instanceof Integer) {
				fid = ((Integer) father).intValue();
			} else if (father instanceof String) {
				try {
					fid = Integer.parseInt((String) father);
				} catch (NumberFormatException e) {
					MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
				}
			} else if (father instanceof IIdentifiedBean) {
				fid = ((IIdentifiedBean) father).getId();
			}
		}
		if (fatherId == 0) {
			if (fid == 0) {
				return true;
			}
			// We must test element that believe to have a father and don't have one in the source list.
			return sourceLink.find(fid) == null;
		}
		return fid == fatherId;
	}

	@Override
	protected IStatus doAdd(IObservableList observableList, Object element, int index) {
		if ((element instanceof BeanMap) && accept((BeanMap) element)) {
			// Get first element really added from position index.
			int i = -1;
			for (int j = index; j >= 0; j--) {
				int x = completed.get(j).intValue();
				if (x > -1) {
					i = x;
					break;
				}
			}
			i++;
			completed.add(index, Integer.valueOf(i));
			return super.doAdd(observableList, element, i);
		}
		completed.add(index, Integer.valueOf(-1));
		return Status.OK_STATUS;
	}

	@Override
	protected IStatus doRemove(IObservableList observableList, int index) {
		int windex = -1;
		if ((index >= 0) && (index < completed.size())) {
			windex = completed.get(index).intValue();
		}
		if (windex > -1) {
			completed.remove(index);
			for (int i = completed.size() - 1; i >= index; i--) {
				int j = completed.get(i).intValue();
				if (j > -1) {
					completed.set(i, Integer.valueOf(j - 1));
				}
			}
			return super.doRemove(observableList, windex);
		}
		completed.remove(index);
		return Status.OK_STATUS;
	}

}
