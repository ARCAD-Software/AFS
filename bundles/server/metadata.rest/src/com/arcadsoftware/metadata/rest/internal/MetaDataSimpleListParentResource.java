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
package com.arcadsoftware.metadata.rest.internal;

import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

public class MetaDataSimpleListParentResource extends MetaDataParentResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (isExisting()) {
			setLastModification(Activator.getInstance().getListLastChange(getEntity()));
		}
	}

	@Override
	protected String getType() {
		return "list/" + super.getType(); //$NON-NLS-1$
	}
	
	@Override
	protected int getDefaultPageCount(int first) {
		return -1;
	}
	
	@Override
	protected Representation post(Representation representation, Variant variant) throws ResourceException {
		Representation rep = super.post(representation, variant);
		if (rep != null) {
			// Post completed: record modification date into List entity :
			Activator.getInstance().setListLastChange(getEntity());
		}
		return rep;
	}

}
