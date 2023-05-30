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
package com.arcadsoftware.afs.client.core.ui.loaders;

import java.io.File;
import java.io.InputStream;
import java.util.Hashtable;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.UIJob;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.images.ImageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapListEvent;
import com.arcadsoftware.beanmap.BeanMapListTypedEvent;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.beanmap.IBeanMapListener;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;

public class CoreContentLoader implements ISWTDataLoader {

	private Hashtable<ServerConnection, DataAccessHelper> helpers;
	private ServerConnection connection;
	private Display parentDisplay;
	private boolean useUIJob = true;

	private abstract class UIJobWrapper {

		private UIJob job;

		public UIJobWrapper(String name) {
			if (parentDisplay == null) {
				job = new UIJob(name) {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						return wrappedRun(monitor);
					}
				};
			} else {
				job = new UIJob(parentDisplay, name) {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						return wrappedRun(monitor);

					}
				};
			}
		}

		public void schedule() {
			job.schedule();
		}

		public void setSystem(boolean value) {
			job.setSystem(value);
		}

		public abstract IStatus wrappedRun(IProgressMonitor monitor);

	}

	private abstract class ThreadWraper extends UserConnectionInfoProviderThread {

		public ThreadWraper(String name) {
			super(name);
			setLogin(connection.getLogin());
			setPassword(connection.getPassword());
		}
	}

	public CoreContentLoader() {
		// useUIJob = Activator.getInstance().useUIJob();
		useUIJob = false;
		helpers = new Hashtable<>();
	}

	public void setServerConnection(ServerConnection connexion) {
		connection = connexion;
	}

	public DataAccessHelper getHelper(ServerConnection connexion) {
		DataAccessHelper helper = helpers.get(connexion);
		if (helper == null) {
			helper = new DataAccessHelper(connection);
			helpers.put(connection, helper);
		}
		return helper;
	}

	@Override
	public void setDisplay(Display display) {
		parentDisplay = display;
	}

	@Override
	public boolean deleteSubListItem(String type, int id, String linkCode, int subId) {
		if (connection != null) {
			final DataAccessHelper helper = getHelper(connection);
			return helper.deleteLink(type, id, linkCode, subId);
		}
		return false;
	}

	@Override
	public void loadBeanMap(final String type, final int id, final IBeanMapListener listener) {
		if (id <= -1) {
			listener.changed(new BeanMapEvent(new BeanMap(type, id)));
		} else {
			new ThreadWraper("Dynamic Editor: Load a BeanMap") { //$NON-NLS-1$
				@Override
				public void run() {
					if (connection != null) {
						final DataAccessHelper helper = getHelper(connection);
						final BeanMap beanMap = helper.read(type, id);
						if (useUIJob) {
							final UIJobWrapper job = new UIJobWrapper("Load BeanMap") {//$NON-NLS-1$
								@Override
								public IStatus wrappedRun(IProgressMonitor monitor) {
									listener.changed((beanMap != null) ? new BeanMapEvent(beanMap) : null);
									return Status.OK_STATUS;
								}
							};
							job.setSystem(true);
							job.schedule();
						} else {
							listener.changed((beanMap != null) ? new BeanMapEvent(beanMap) : null);
						}
					}
				}
			}.start();
		}
	}

	@Override
	public BeanMap loadBeanMap(String type, int id) {
		if (id <= 0) {
			return new BeanMap(type, id);
		} else {
			if (connection != null) {
				final DataAccessHelper helper = getHelper(connection);
				return helper.read(type, id);
			}
			return null;
		}
	}

	@Override
	public BeanMap loadBeanMap(String type, int id, String attributeList) {
		if (id <= 0) {
			return new BeanMap(type, id);
		}
		if (connection != null) {
			final DataAccessHelper helper = getHelper(connection);
			return helper.read(type, id, attributeList);
		}
		return null;
	}

	@Override
	public ImageDescriptor loadImage(String key) {
		final ImageDescriptor result = ImageManager.getInstance().getImageDescriptor(key);
		return result;
	}

	@Override
	public void loadList(final String type, final IBeanMapListListener listener) {
		new ThreadWraper("Dynamic Editor: Load a BeanMap list") { //$NON-NLS-1$
			@Override
			public void run() {
				if (connection != null) {
					final DataAccessHelper helper = getHelper(connection);
					final BeanMapList list = helper.getList(type);
					if (useUIJob) {
						new UIJobWrapper("Load List") { //$NON-NLS-1$
							@Override
							public IStatus wrappedRun(IProgressMonitor monitor) {
								if (list != null) {
									listener.changed(new BeanMapListEvent(list));
								} else {
									listener.changed(null);
								}
								return Status.OK_STATUS;
							}
						}.schedule();
					} else if (list != null) {
						listener.changed(new BeanMapListEvent(list));
					} else {
						listener.changed(null);
					}
				}
			}
		}.start();
	}

	@Override
	public void loadList(final String type, final String attribute, final boolean equals, final Object value,
			final IBeanMapListListener listener) {
		new ThreadWraper("Dynamic Editor: Load a BeanMap list filtered on attribute value") { //$NON-NLS-1$
			@Override
			public void run() {
				if (connection != null) {
					final DataAccessHelper helper = getHelper(connection);
					ISearchCriteria criteria = null;
					if (value == null) {
						criteria = new IsNullCriteria(attribute);
					} else {
						criteria = new EqualCriteria(attribute, value.toString());
						if (!equals) {
							criteria = new OrCriteria(new IsNullCriteria(attribute), new NotCriteria(criteria));
						}
					}
					final BeanMapList list = helper.getList(type, criteria);
					if (useUIJob) {
						new UIJobWrapper("Load List") { //$NON-NLS-1$
							@Override
							public IStatus wrappedRun(IProgressMonitor monitor) {
								if (list != null) {
									listener.changed(new BeanMapListEvent(list));
								} else {
									listener.changed(null);
								}
								return Status.OK_STATUS;
							}
						}.schedule();
					} else if (list != null) {
						listener.changed(new BeanMapListEvent(list));
					} else {
						listener.changed(null);
					}
				}
			}
		}.start();

	}

	@Override
	public void loadSubList(final String type, final int id, final String linkCode,
			final String subType, final IBeanMapListListener listener, final String attributeList) {
		loadSubList(type, id, linkCode, subType, listener, attributeList, 0, null);
	}

	@Override
	public void loadSubList(final String type, final int id, final String linkCode,
			final String subType, final IBeanMapListListener listener, final String attributeList,
			final String orderList) {
		loadSubList(type, id, linkCode, subType, listener, attributeList, 0, orderList);
	}

	@Override
	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener,
			String attributeList, int pageCount) {
		loadSubList(type, id, linkCode, subtype, listener, attributeList, pageCount, null);

	}

	@Override
	public void loadSubList(String type, int id, String linkCode,
			String subtype, IBeanMapListListener listener,
			String attributeList, String orderList, int pageCount) {
		loadSubList(type, id, linkCode, subtype, listener, attributeList, pageCount, orderList);
	}

	public void loadSubList(final String type, final int id, final String linkCode,
			final String subType, final IBeanMapListListener listener, final String attributeList, final int pageCount,
			final String orderList) {
		if (id <= -1) {
			listener.changed(new BeanMapListEvent(new BeanMapList()));
		} else {
			new ThreadWraper("Dynamic Editor: Load a BeanMap sub-list (with specific attributes") { //$NON-NLS-1$
				@Override
				public void run() {
					if (connection != null) {
						final DataAccessHelper helper = getHelper(connection);
						final BeanMapList list = helper.getLinkList(type, id, linkCode, subType, attributeList,
								orderList, pageCount);
						if (useUIJob) {
							new UIJobWrapper("Load Links") { //$NON-NLS-1$
								@Override
								public IStatus wrappedRun(IProgressMonitor monitor) {
									listener.changed((list != null) ? new BeanMapListTypedEvent(list, subType) : null);
									return Status.OK_STATUS;
								}
							}.schedule();
						} else {
							listener.changed((list != null) ? new BeanMapListTypedEvent(list, subType) : null);
						}
					}
				}
			}.start();
		}
	}

	@Override
	public void loadSubList(final String type, final int id, final String linkCode, final String subType,
			final IBeanMapListListener listener) {
		loadSubList(type, id, linkCode, subType, listener, 0);
	}

	@Override
	public void loadSubList(final String type, final int id, final String linkCode, final String subType,
			final IBeanMapListListener listener, final int pageCount) {
		if (id <= -1) {
			listener.changed(new BeanMapListEvent(new BeanMapList()));
		} else {
			new ThreadWraper("Dynamic Editor: Load a BeanMap sub-list") { //$NON-NLS-1$
				@Override
				public void run() {
					if (connection != null) {
						final DataAccessHelper helper = getHelper(connection);
						final BeanMapList list = helper.getLinkList(type, id, linkCode, subType, null, null, pageCount);
						if (useUIJob) {
							new UIJobWrapper("Load Links") { //$NON-NLS-1$
								@Override
								public IStatus wrappedRun(IProgressMonitor monitor) {
									listener.changed(new BeanMapListTypedEvent(list, subType));
									return Status.OK_STATUS;
								}
							}.schedule();
						} else {
							listener.changed(new BeanMapListTypedEvent(list, subType));
						}
					}
				}
			}.start();
		}
	}

	/**
	 * Create an entry into a list of association
	 */
	@Override
	public boolean putSubListItem(String type, int id, String linkCode, int subId) {
		if (id > 0) {
			if (connection != null) {
				final DataAccessHelper helper = getHelper(connection);
				return helper.createLink(type, id, linkCode, subId);
			}
		}
		return false;
	}

	@Override
	public boolean updateBeanMap(BeanMap beanMap) {
		boolean result = false;
		if (connection != null) {
			final DataAccessHelper helper = getHelper(connection);
			if (beanMap.getId() > 0) {
				result = helper.update(beanMap);
			} else if (beanMap.getId() == 0) {
				result = helper.create(beanMap);
			}
		}
		return result;
	}

	@Override
	public InputStream loadStream(String type, int id) {
		if (connection != null) {
			return getHelper(connection).getBeanStream(type, id);
		}
		return null;
	}

	@Override
	public boolean updateStream(String type, int id, File file) {
		if (connection != null) {
			return getHelper(connection).uploadBeanStream(type, id, file);
		}
		return false;
	}

	@Override
	public String getUploadBeanStreamAddress(String type, int id) {
		if (connection != null) {
			return getHelper(connection).getUploadRedirection(type, id);
		}
		return null;
	}

	@Override
	public BeanMap createBeanMap(BeanMap beanMap) {
		if (connection != null) {
			final BeanMap b = beanMap.duplicate();
			b.addAll(beanMap);
			if (getHelper(connection).create(b)) {
				return b;
			}
		}
		return null;
	}

	@Override
	public String getLastErrorMessage() {
		if (connection != null) {
			return connection.getErrorMessage().getTextLevel1();
		}
		return null;
	}

	@Override
	public UserMessage getLastErrorUserMessage() {
		if (connection != null) {
			return connection.getErrorMessage();
		}
		return null;
	}

	public ServerConnection getConnection() {
		return connection;
	}

	/**
	 * Load content from url and type
	 */
	@Override
	public BeanMap loadContent(String url, String type) {
		final BeanMap result = new BeanMap(type);
		if ((url != null) && (url.length() > 0)) {
			if (connection != null) {
				final DataAccessHelper helper = getHelper(connection);
				helper.get(url, result);
			}
		}
		return result;
	}

}
