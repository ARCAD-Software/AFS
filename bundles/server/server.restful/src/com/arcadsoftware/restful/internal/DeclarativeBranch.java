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
package com.arcadsoftware.restful.internal;

import java.util.ArrayList;

import org.osgi.framework.Bundle;
import org.osgi.framework.ServiceRegistration;
import org.restlet.Context;
import org.restlet.resource.ServerResource;
import org.restlet.routing.Router;
import org.restlet.routing.Template;

import com.arcadsoftware.rest.AbstractBranch;
import com.arcadsoftware.rest.BranchTracker;
import com.arcadsoftware.rest.RouteList;

public class DeclarativeBranch extends AbstractBranch {

	protected class ResourceDeclaration {
		
		private String path;
		private String className;
		
		public void setPath(String path) {
			this.path = path;
		}

		public String getPath() {
			return path;
		}

		public void setClassName(String className) {
			this.className = className;
		}

		public String getClassName() {
			return className;
		}
	}
	
	private transient Bundle bundle;
	private transient int count = 0;
	private transient ServiceRegistration<?> serviceRegistration;
	private String uri;
	private String path;
	private boolean secured;
	private String defaultResource;
	private BranchTracker branchTracker;
	private ArrayList<ResourceDeclaration> resources;
	private ArrayList<DeclarativeBranch> subbranches;
	private Activator activator;
	
	@SuppressWarnings("unchecked")
	public Object attach(Context context, Router router) {
		RouteList rl = new RouteList();
		if ((subbranches != null) && (subbranches.size() > 0)) {
			synchronized (this) {
				if (count == 0) {
					for (DeclarativeBranch b:subbranches) {
						if (b.uri != null) {
							b.branchTracker = new BranchTracker(getApplication(), b.uri); //$NON-NLS-1$
							b.branchTracker.setRouter(new Router());
							b.branchTracker.getRouter().setDefaultMatchingMode(Template.MODE_STARTS_WITH);
							b.branchTracker.getRouter().setRoutingMode(Router.MODE_BEST_MATCH);
							if (b.defaultResource != null) {
								try {
									b.branchTracker.getRouter().attachDefault((Class<? extends ServerResource>) bundle.loadClass(b.defaultResource));
								} catch (ClassNotFoundException e) {
									activator.error(e.getLocalizedMessage(), e);
								}
							}
							if (b.resources != null) {
								for(ResourceDeclaration r:b.resources) {
									if ((r.path != null) && (r.className != null)) {
										try {
											b.branchTracker.getRouter().attach(r.path, (Class<? extends ServerResource>) bundle.loadClass(r.className));
										} catch (ClassNotFoundException e) {
											activator.error(e);
										}
									}
								}
							}
							b.branchTracker.open();
						}
					}					
				}				
			}
			for(DeclarativeBranch b:subbranches) {
				if ((b.path != null) && (b.branchTracker != null)) {
					rl.add(router.attach(b.path, b.branchTracker.getRouter())); //$NON-NLS-1$
				}
			}
		}
		if (resources != null) {
			for(ResourceDeclaration r:resources) {
				if ((r.path != null) && (r.className != null)) {
					try {
						rl.add(router.attach(r.path, (Class<? extends ServerResource>) bundle.loadClass(r.className)));
					} catch (ClassNotFoundException e) {
						activator.error(e);
					}
				}
			}
		}
		return rl;
	}

	public void detach(Context context, Router router, Object reference) {
		if (reference != null) {
			((RouteList)reference).detachAll(router);
		}
		if ((subbranches != null) && (subbranches.size() > 0)) {
			synchronized (this) {
				count--;
				if (count == 0) {
					for(DeclarativeBranch b:subbranches) {
						if (b.branchTracker != null) {
							b.branchTracker.close();
							b.branchTracker = null;
						}
					}
				}
			}
		}
	}

	public Bundle getBundle() {
		return bundle;
	}

	public String getUri() {
		if (uri == null) {
			if (secured) {
				return SECUREDBRANCH;
			}
			return ROOTBRANCH;
		}
		return uri;
	}

	public void setBundle(Bundle bundle) {
		this.bundle = bundle;
	}

	public void setServiceRegistration(ServiceRegistration<?> serviceRegistration) {
		this.serviceRegistration = serviceRegistration;
	}

	public ServiceRegistration<?> getServiceRegistration() {
		return serviceRegistration;
	}

	public void setActivator(Activator activator) {
		this.activator = activator;
		if (subbranches != null) {
			for(DeclarativeBranch b:subbranches) {
				b.setActivator(activator);
			}
		}
	}

}
