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
package com.arcadsoftware.editor.swt.renderer;

import com.arcadsoftware.editor.swt.ISWTRenderer;

public interface ILoaderCallback {

	public void process(ISWTRenderer parentRenderer, ISWTRenderer editorRenderer, String editorId, int entityId);

}
