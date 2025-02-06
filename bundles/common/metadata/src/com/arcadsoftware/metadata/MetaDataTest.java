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

import java.io.Serializable;

/**
 * control Test is a script that can be executed to test if the values of an entity are coherent.
 */
public class MetaDataTest implements Cloneable, Serializable {

	public static final String EVENTCODE_BEFORECREATE = "beforeCreate"; //$NON-NLS-1$
	public static final String EVENTCODE_AFTERCREATE = "afterCreate"; //$NON-NLS-1$
	public static final String EVENTCODE_BEFOREUPDATE = "beforeUpdate"; //$NON-NLS-1$
	public static final String EVENTCODE_AFTERUPDATE = "afterUpdate"; //$NON-NLS-1$
	public static final String EVENTCODE_BEFOREDELETE = "beforeDelete"; //$NON-NLS-1$
	public static final String EVENTCODE_AFTERDELETE = "afterDelete"; //$NON-NLS-1$
	public static final String EVENTCODE_LIST = "list"; //$NON-NLS-1$
	public static final String EVENTCODE_READ = "read"; //$NON-NLS-1$
	public static final String EVENTCODE_BEFOREUNDELETE = "beforeUndelete"; //$NON-NLS-1$
	public static final String EVENTCODE_AFTERUNDELETE = "afterUndelete"; //$NON-NLS-1$
	
	private static final long serialVersionUID = -72831726696998386L;

	private transient MetaDataEntity parent;
	private String code;
	private String event;
	private String test;
	private transient String[] eventList;

	public MetaDataTest() {
		super();
	}
	
	public MetaDataTest(MetaDataEntity parent, String code, String event, String test) {
		super();
		this.parent = parent;
		this.event = event;
		this.test = test;
		this.code = code;
		if (event != null) {
			eventList = event.split(" "); //$NON-NLS-1$
		}
	}
	
	public MetaDataTest(MetaDataTest test, MetaDataEntity parent) {
		super();
		this.parent = parent;
		injectValues(test);
	}
	
	/**
	 * @return The test Groovy script.
	 */
	public String getTest() {
		return test;
	}
	
	public void setTest(String test) {
		this.test = test;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		if (code != this.code) {
			String oldCode = this.code;
			this.code = code;
			if (parent != null) {
				parent.reIndexElement(oldCode,this);
			}
		}
	}

	public String getEvent() {
		return event;
	}

	public void setEvent(String event) {
		this.event = event;
	}

	/**
	 * @param parent the parent to set
	 */
	public void setParent(MetaDataEntity parent) {
		this.parent = parent;
	}

	/**
	 * @return the parent
	 */
	public MetaDataEntity getParent() {
		return parent;
	}

	/**
	 * Inject all the values of the given Test object except the parent.
	 * @param test
	 */
	public void injectValues(MetaDataTest test) {
		this.code = test.code;
		this.event = test.event;
		this.eventList = test.eventList;
		this.test = test.test;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new MetaDataTest(this,parent);
	}

	/**
	 * State if this test must be executed with this event code.
	 * 
	 * <p>
	 * See EVENTCODE_* constants.
	 * 
	 * @param event
	 * @return
	 */
	public boolean isThrown(String event) {
		if ((event == null) || (this.event == null)) {
			return false;
		}
		if (eventList == null) {
			eventList = this.event.split(" "); //$NON-NLS-1$
		}
		for (String e:eventList) {
			if (event.equalsIgnoreCase(e)) {
				return true;
			}
		}
		return false;
	}
}
