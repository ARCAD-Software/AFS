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
package com.arcadsoftware.rest;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.restlet.Restlet;
import org.restlet.routing.TemplateRoute;
import org.restlet.routing.Router;

/**
 * Helper class used to link to attached routes to a Router.
 * 
 * <p>This class accept routes or restlet and detach them according to their type.
 * 
 * <p>If you attach a <code>Restlet</code> object to a <code>Router</code> you just need
 * this object to detach it from the router. You can them place this object into the list.
 * 
 * <p>If you attach resource the only id object you can obtain form this operation 
 * is a <code>Route</code> object. In this case this is this object that you can
 * store into this list.
 * 
 */
public class RouteList extends ArrayList<Restlet> {

	private static final long serialVersionUID = 7091807872735365736L;

	/**
	 * Create a Route list with a first route.
	 * 
	 * @param firstRoute the first Route to be added.
	 */
	public RouteList(Restlet firstRoute) {
		super(1);
		Collections.addAll(this, firstRoute);
	}

	/**
	 * Create a Route list with some routes.
	 * 
	 * @param firstRoute the first Route to be added.
	 */
	public RouteList(Restlet... firstRoute) {
		super(firstRoute.length);
		Collections.addAll(this, firstRoute);
	}

    /**
     * Constructs an empty list with an initial capacity of five.
     */
	public RouteList() {
		super(5);
	}

    /**
     * Constructs a list containing the elements of the specified
     * collection, in the order they are returned by the collection's
     * iterator.  The <tt>ArrayList</tt> instance has an initial capacity of
     * 110% the size of the specified collection.
     *
     * @param c the collection whose elements are to be placed into this list.
     * @throws NullPointerException if the specified collection is null.
     */
	public RouteList(Collection<? extends Restlet> c) {
		super(c);
	}

    /**
     * Constructs an empty list with the specified initial capacity.
     *
     * @param   initialCapacity   the initial capacity of the list.
     * @exception IllegalArgumentException if the specified initial capacity
     *            is negative
     */
	public RouteList(int initialCapacity) {
		super(initialCapacity);
	}

	/**
	 * Remove any route or detach any restlet attached to this Router and
	 * clear the list.
	 * 
	 * @param router the Router to detach from.
	 */
	public void detachAll(Router router) {
		if (router != null) {
			for(Restlet restlet:this) {
				if (restlet instanceof TemplateRoute) {
					router.getRoutes().remove(restlet);
			        if (router.getDefaultRoute() == restlet) {
			            router.setDefaultRoute(null);
			        }
				} else {
					router.detach(restlet);
				}
			}
			clear();
		}
	}
}
