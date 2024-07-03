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
package com.arcadsoftware.afs.client.core.singletons;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;

import com.arcadsoftware.afs.client.core.internal.BaseActivator;

/**
 * This class should be used whenever a non stateless singleton must be get (e.g. a singleton holding a connection). By
 * default, this singleton will act as a default {@link SingletonProvider}, that will instantiate a singleton an cache
 * the instance.
 * <p>
 * Bundles can register a {@link SingletonProvider} service to provide their own mechanism (e.g. see
 * <b>com.arcadsoftware.afs.client.core.rap</b> that provides a service to get unique singletons for each UI sessions).
 * 
 * @author ARCAD software
 */
public class SingletonManager implements SingletonProvider {

	private static final SingletonManager INSTANCE = new SingletonManager();

	public static <T> T get(final Class<T> singletonClass) {
		return INSTANCE.getProvider().getSingleton(singletonClass);
	}

	private final BundleContext bundleContext;
	private SingletonProvider provider;
	private final Map<Class<?>, ?> singletonInstances;

	private SingletonManager() {
		bundleContext = FrameworkUtil.getBundle(SingletonManager.class).getBundleContext();
		singletonInstances = new HashMap<>();
	}

	private SingletonProvider getProvider() {
		if (provider == null) {
			provider = loadProvider();
		}
		return Optional.ofNullable(provider).orElse(this);
	}

	private SingletonProvider loadProvider() {
		return Optional.ofNullable(bundleContext.getServiceReference(SingletonProvider.class))
				.map(bundleContext::getService)
				.orElse(null);
	}

	@SuppressWarnings("unchecked")
	@Override
	public synchronized <T> T getSingleton(Class<T> singletonClass) {
		return (T) singletonInstances.computeIfAbsent(singletonClass, this::instantiate);
	}

	@SuppressWarnings("unchecked")
	private <T> T instantiate(Class<?> singletonClass) {
		try {
			final Constructor<?> constructor = singletonClass.getDeclaredConstructor();
			constructor.setAccessible(true);
			return (T) constructor.newInstance();
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
				| IllegalArgumentException | InvocationTargetException e) {
			BaseActivator.getDefault().error("Could not instantiate " + singletonClass.getName(), e);
			return null;
		}
	}
}
