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
package com.arcadsoftware.rest.internal;

import com.thoughtworks.xstream.XStream;

public class OSGiXStreamClassLoader extends ClassLoader {

	private final ClassLoader parent;
	
	public OSGiXStreamClassLoader(ClassLoader parent) {
		super();
		this.parent = parent;
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
    public final Class loadClass(String name) throws ClassNotFoundException {
        final ClassLoader contextClassLoader = Thread.currentThread().getContextClassLoader();
        if ((parent != null) && (parent != contextClassLoader)) {
            try {
                return parent.loadClass(name);
            } catch (ClassNotFoundException notFound) {}
        }
        if (getClass().getClassLoader() != contextClassLoader) {
            try {
                return getClass().getClassLoader().loadClass(name);
            } catch (ClassNotFoundException notFound) {}
        }
        if (XStream.class.getClassLoader() != contextClassLoader) {
            try {
                return XStream.class.getClassLoader().loadClass(name);
            } catch (ClassNotFoundException notFound) {}
        }
        if (contextClassLoader != null) {
            return contextClassLoader.loadClass(name);
        }
        throw new ClassNotFoundException(name);
    }
}
