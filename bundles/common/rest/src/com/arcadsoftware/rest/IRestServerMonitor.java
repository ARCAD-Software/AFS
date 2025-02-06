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

import java.util.Date;

import org.restlet.Component;

/**
 * OSGi Service used to monitor the embedded HTTPS REST Server.
 * 
 * @author ARCAD Software
 */
public interface IRestServerMonitor {

	/**
	 * Force a HTTP server restart.
	 * 
	 * @return true if the server is started.
	 */
	public boolean restart();
	
	/**
	 * Get the date of the latest start of the HTTP server. 
	 * @return
	 */
	public Date getStartDate();
	
	/**
	 * Get the current Restlet Component.
	 * 
	 * <p>
	 * The component is discarded and recreated each time the server is started.
	 * 
	 * @return null if the server is currently halt.
	 */
	public Component getComponent();

	/**
	 * Get the HTTP/HTTPS Port number.
	 * @return
	 */
	public int getPort();
}
