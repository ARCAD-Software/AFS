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
package com.arcadsoftware.rest;

import java.util.stream.Stream;

import org.restlet.resource.ServerResource;
import org.restlet.routing.Router;

/**
 * Utility class to create a {@link RouteList} with a simple call.<br />
 * Attached {@link org.restlet.resource.Resource} must declare the {@link Path} annotation that will define their URL.<br/><br/>
 * 
 * Example:<br />
 * <code>
 * protected RouteList createAttachedResources(final Context context, final Router router) {<br />
 * &nbsp;&nbsp;&nbsp;new RouteListBuilder(router)<br />
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.attach(Resource1.class)<br />
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.attach(Resource2.class)<br />
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.attach(Resource3.class)<br />
 * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.attach(Resource4.class);<br />
 * }
 * </code>
 * 
 * @author ARCAD Software
 *
 */
public class RouteListBuilder {

	private final RouteList result = new RouteList();
	private final Router router;

	public RouteListBuilder(final Router router) {
		this.router = router;
	}

	public RouteListBuilder attach(final Class<? extends ServerResource> resource) {
		final Path annotation = resource.getAnnotation(Path.class);
		Stream.of(annotation.value()).forEach(url -> result.add(router.attach(url, resource)));
		return this;
	}

	public RouteList toRouteList() {
		return result;
	}
}