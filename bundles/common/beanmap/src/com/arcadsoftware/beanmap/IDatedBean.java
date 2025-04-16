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
package com.arcadsoftware.beanmap;

import java.util.Date;

/**
 * Dated bean are beans with time stamp used to store last change date.
 * 
 * this feature is used to cache the bean into local caches.
 */
public interface IDatedBean {

	/**
	 * The date attribute is an internal attribute that define the Bean date.
	 * Its value is a Date converted with ISODateFormater.
	 */
	public static final String KEY_DATE = "date"; //$NON-NLS-1$

	/**
	 * The Modification User IDentifier attribute is an internal attribute that 
	 * define the identifier of the owner of the Bean.
	 */
	public static final String KEY_MUID = "muid"; //$NON-NLS-1$

	/**
	 * The date store the last modification date of this bean. 
	 * 
	 * @return a Date or EPOCH if this bean has never been changed.
	 */
	public Date getDate();
	
	/**
	 * Set the last change date of the bean, this Date should not be null.
	 * 
	 * @param date
	 */
	public void setDate(Date date);

	/**
	 * Compare the date of two DatedBean.
	 * @param bm a TypedBean
	 * @return true if this bean id the most recent one. 
	 */
	public boolean moreRecent(IDatedBean bm);

	/**
	 * The IDentifier of the User responsible of the Modification of this bean.
	 *  
	 * <p>
	 * This value is optional an different from the ID of an IIdentifiedBean.
	 *  
	 * @return a negative or null value indicate that there is no known user associated to this data.
	 * @see IIdentifiedBean#getId() 
	 */
	public int getMUID();
	
	/**
	 * Set the Modification User IDentifier of this bean.
	 * @param id
	 */
	public void setMUID(int id);
	
	/**
	 * Set the modification information (the User and the date of the modification).
	 * @param uid
	 * @param date
	 */
	public void setModification(int uid, Date date);
}
