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
package com.arcadsoftware.editor.implementation.swt;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapListEvent;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.editor.implementation.Activator;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;

public class LinkMapWarper {

	public static final int LINKLIST_ADD = 0;
	public static final int LINKLIST_REMOVE = 1;
	public static final int LINKLIST_EDIT = 2;

	private final SWTRenderer renderer;
	private int id;

	public class Operations {
		public HashMap<Integer, BeanMap> addList = new HashMap<>();
		public HashMap<Integer, BeanMap> removeList = new HashMap<>();

		public boolean isEmpty() {
			return addList.isEmpty() && removeList.isEmpty();
		}
	}

	HashMap<String, Operations> ops = new HashMap<>();

	public LinkMapWarper(SWTRenderer renderer) {
		super();
		this.renderer = renderer;
	}

	private class WarperListener implements IBeanMapListListener {

		LinkMapWarper linkwarper;
		IBeanMapListListener listener;
		String code;

		public WarperListener(LinkMapWarper linkwarper, IBeanMapListListener listener, String code) {
			super();
			this.linkwarper = linkwarper;
			this.listener = listener;
			this.code = code;
		}

		@Override
		public void changed(BeanMapListEvent event) {
			final BeanMapList result = event.getSource();
			// We (try to) apply pendings operations...
			final Operations op = linkwarper.ops.get(code);
			if (op != null) {
				try {
					for (final BeanMap bm : op.removeList.values()) {
						result.remove(bm);
					}
				} catch (final Exception e) {
					Activator.getInstance().debug("Error during remove operation...", e); //$NON-NLS-1$
				}
				for (final BeanMap bm : op.addList.values()) {
					if (result.indexOf(bm) == -1) {
						result.add(bm);
					}
				}
			}
			listener.changed(event);
		}

	}

	/**
	 * Load a BeanMap sublist.
	 * <p>
	 * Caching the list is the responsibility of the loader.
	 */
	public void loadLinkList(String code, String type, IBeanMapListListener listener, String attributeList) {
		loadLinkList(code, type, listener, attributeList, null, 0);
	}

	public void loadLinkList(String code, String type, IBeanMapListListener listener, String attributeList,
			String orderList) {
		loadLinkList(code, type, listener, attributeList, orderList, 0);
	}

	public void loadLinkList(String code, String type, IBeanMapListListener listener, String attributeList,
			String orderList, int pageCount) {
		final IBeanMapListListener warperListener = new WarperListener(this, listener, code);
		if (id == 0) {
			// Null id state for newly created BeanMap (so we should got an empty link list !)
			warperListener.changed(new BeanMapListEvent(new BeanMapList()));
		} else {
			if ((attributeList != null) && (orderList != null)) {
				renderer.getDataLoader().loadSubList(
						renderer.getStructure().getType(), id, code, type, warperListener, attributeList, orderList,
						pageCount);
			} else if (attributeList != null) {
				renderer.getDataLoader().loadSubList(
						renderer.getStructure().getType(), id, code, type, warperListener, attributeList, pageCount);
			} else {
				renderer.getDataLoader().loadSubList(
						renderer.getStructure().getType(), id, code, type, warperListener);
			}
		}

	}

	/**
	 * Load a BeanMap Item with required attribute list.
	 */
	public BeanMap loadLinkedItem(String type, int itemId, String attributeList) {
		BeanMap loadedItem = null;
		if (id > 0) {
			if (attributeList != null) {
				loadedItem = renderer.getDataLoader().loadBeanMap(type, itemId, attributeList);
			} else {
				loadedItem = renderer.getDataLoader().loadBeanMap(type, itemId);
			}
		}
		if (loadedItem == null) {
			loadedItem = new BeanMap(type, itemId);
		}
		return loadedItem;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	/**
	 * @param linklistAdd
	 * @param o
	 */
	public void updateLinkList(String code, int linklistAdd, BeanMap o) {
		Operations op = ops.get(code);
		final Integer currentId = Integer.valueOf(o.getId());
		if (op == null) {
			op = new Operations();
			switch (linklistAdd) {
			case LINKLIST_ADD:
				op.addList.put(currentId, o);
				break;
			case LINKLIST_REMOVE:
				op.removeList.put(currentId, o);
				break;
			}
			ops.put(code, op);
		} else {
			switch (linklistAdd) {
			case LINKLIST_ADD:
				if (op.removeList.get(currentId) == null) {
					op.addList.put(currentId, o);
				} else {
					op.removeList.remove(currentId);
					if (op.isEmpty()) {
						ops.remove(code);
					}
				}
				break;
			case LINKLIST_REMOVE:
				if (op.addList.get(currentId) == null) {
					op.removeList.put(currentId, o);
				} else {
					op.addList.remove(currentId);
					if (op.isEmpty()) {
						ops.remove(code);
					}
				}
				break;
			}
		}
		// blindage !
		renderer.fireChangedEvent();
	}

	/**
	 * Save the recorded operations.
	 */
	public void save() {
		for (final Entry<String, Operations> op : ops.entrySet()) {
			for (final Integer i : op.getValue().addList.keySet()) {
				renderer.getDataLoader().putSubListItem(renderer.getStructure().getType(), id, op.getKey(),
						i.intValue());
			}
			for (final Integer i : op.getValue().removeList.keySet()) {
				renderer.getDataLoader().deleteSubListItem(renderer.getStructure().getType(), id, op.getKey(),
						i.intValue());
			}
		}
		clear();
	}

	public void clear() {
		ops.clear();
	}

	public Set<Entry<String, Operations>> getPendingsOperations() {
		return ops.entrySet();
	}

	public boolean isDirty() {
		return !ops.isEmpty();
	}
}
