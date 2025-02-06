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
package com.arcadsoftware.editor.implementation.swt.binding;

import org.eclipse.jface.databinding.viewers.TreeStructureAdvisor;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;

public class BeanMapStructureAdvisor extends TreeStructureAdvisor {

	BeanMapObservableLink link;
	String fatherCode;

	public BeanMapStructureAdvisor(BeanMapObservableLink link, String fatherCode) {
		super();
		this.link = link;
		this.fatherCode = fatherCode;
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof IBeanMap) {
			final Object father = ((IBeanMap) element).get(fatherCode);
			if (father != null) {
				int fid = 0;
				if (father instanceof Integer) {
					fid = ((Integer) father).intValue();
				} else if (father instanceof String) {
					try {
						fid = Integer.parseInt((String) father);
					} catch (final NumberFormatException e) {
						MessageManager.addException(e, MessageManager.LEVEL_PRODUCTION);
					}
				} else if (father instanceof IIdentifiedBean) {
					return father;
				}
				// Look for this beanmap into the global list !
				if (fid != 0) {
					for (final Object o : link) {
						if ((o instanceof IIdentifiedBean) && (((IIdentifiedBean) o).getId() == fid)) {
							return o;
						}
					}
				}
			}
		}
		return null;
	}

	@Override
	public Boolean hasChildren(Object element) {
		if (element instanceof IIdentifiedBean) {
			final int id = ((IIdentifiedBean) element).getId();
			if (id == 0) {
				return Boolean.valueOf(false);
			}
			for (final Object e : link) {
				if (e instanceof IBeanMap) {
					final Object o = ((IBeanMap) e).get(fatherCode);
					int fid = 0;
					if (o instanceof Integer) {
						fid = ((Integer) o).intValue();
					} else if (o instanceof String) {
						try {
							fid = Integer.parseInt((String) o);
						} catch (final NumberFormatException ee) {
							MessageManager.addException(ee, MessageManager.LEVEL_PRODUCTION);
						}
					} else if (o instanceof IIdentifiedBean) {
						fid = ((IIdentifiedBean) o).getId();
					}
					if (id == fid) {
						return Boolean.valueOf(true);
					}
				}
			}
		}
		return null;
	}

}
