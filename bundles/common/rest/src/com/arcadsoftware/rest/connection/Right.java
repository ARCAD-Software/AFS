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
package com.arcadsoftware.rest.connection;

/**
 * A right define if a specific user can access a particular web-service.
 * 
 * <p> The rights are integer identified key that offer special privilege on the server.
 * The semantic of theses privilege is independent from the definition and the storage 
 * of theses rights.
 * 
 * <p>the Rights are stored, and cached, with the user connection informations.
 * 
 * @see IConnectionUserBean
 * @see IConnectionCache
 */
public class Right implements Cloneable {
	
	private int id;
	private int param;
	
	public Right() {
		super();
	}
	
	public Right(Right right) {
		super();
		id = right.id;
		param = right.param;
	}
	
	public Right(int id) {
		super();
		this.id = id;
	}

	public Right(int id, int param) {
		super();
		this.id = id;
		if (param > 0) {
			this.param = param;
		}
	}
	
	public boolean isParametred() {
		return (param != 0);
	}

	
	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public int getParam() {
		return param;
	}

	public void setParam(int param) {
		this.param = param;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof Right) && (((Right)obj).id == id);
	}

	@Override
	public int hashCode() {
		return id;
	}

	@Override
	protected Object clone() {
		return new Right(id, param);
	}

	@Override
	public String toString() {
		return "Right #" + id + " (" + param + ')'; //$NON-NLS-1$ //$NON-NLS-2$
	}
	
}
