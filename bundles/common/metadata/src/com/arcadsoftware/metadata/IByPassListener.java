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
package com.arcadsoftware.metadata;

/**
 * This interface may be used in conjunction with {@link IMetaDataDeleteListener} or {@link IMetaDataModifyListener} 
 * to by pass the default modification implementation.
 * 
 * If the listener return true to any pre-test call then the actual operation (creation, update, deletion) 
 * will be not performed but the Web-Service call will return the correct response.
 * 
 * @author ARCAD Software
 */
public interface IByPassListener {

}
