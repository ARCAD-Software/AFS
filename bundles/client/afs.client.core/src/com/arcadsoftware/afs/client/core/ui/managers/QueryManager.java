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
package com.arcadsoftware.afs.client.core.ui.managers;

import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapListContentChangedListener;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class QueryManager {

	private static final int DEFAULT_COUNT = 20;

	protected MetaDataEntity entityStructure;
	protected String selectClause = null;
	protected String clause;
	protected IBeanMapListContentChangedListener resultListener;
	protected int rank = 0;
	private int total;
	protected String orderClause = null;

	
	ServerConnection connection;
	protected DataAccessHelper helper;
	
	public QueryManager(ServerConnection connection) {
		this(connection,null);
	}

	public QueryManager(ServerConnection connection,
			IBeanMapListContentChangedListener resultComposite) {
		super();
		this.resultListener = resultComposite;
		this.connection = connection;
		helper = new DataAccessHelper(connection);
	}

	public void setResultListener(IBeanMapListContentChangedListener rc) {
		this.resultListener = rc;
	}

	public IBeanMapListContentChangedListener getResultListener() {
		return resultListener;
	}

	public void setQuery(MetaDataEntity entityStructure, String clause) {
		this.entityStructure = entityStructure;
		this.clause = clause;
		this.selectClause = null;
		rank = 0;
		executeQuery();
	}

	public void setQuery(MetaDataEntity entityStructure, String selectClause, String clause) {
		this.entityStructure = entityStructure;
		this.selectClause = selectClause;
		this.clause = clause;
		rank = 0;
		executeQuery();
	}

	public void setQuery(MetaDataEntity entityStructure, String selectClause, String clause, String orderClause) {
		this.entityStructure = entityStructure;
		this.selectClause = selectClause;
		this.clause = clause;
		this.orderClause = orderClause;
		rank = 0;
		executeQuery();
	}

	protected void executeQuery() {
		BeanMapList result = null;
		if (selectClause == null) {
			StringBuilder attributes = new StringBuilder();
			for (MetaDataAttribute attribute : entityStructure.getAttributes().values()) {
				if (attributes.length() > 0) {
					attributes.append(' ');
				}
				attributes.append(attribute.getCode());
			}	
			selectClause = attributes.toString();
		}
		result = getList();
		if (result != null) {
			if (result instanceof BeanMapPartialList) {
				total = ((BeanMapPartialList) result).getTotal();
				if (resultListener != null) {
					resultListener.contentChanged(result);
					resultListener.setElementCount(result.size(), rank / getCountNumber() + 1,
							total != getCountNumber() ? (total / getCountNumber()) + 1 : 1);
				}		
			}  else if (result instanceof BeanMapList) {
				if (resultListener != null) {
					resultListener.contentChanged(result);
				}					
			}
		} else {
			handleError();
		}
	}

	protected BeanMapList getList(){
		return helper.getList(entityStructure.getType(), selectClause, clause,orderClause, rank, getCountNumber(),false);
	}
	
	
	protected UserMessage getErrorMessage(){
		return null;
	}
	
	protected Bundle getBundle(){
		return Activator.getDefault().getBundle();
	}
	
	protected void handleError(){
		LogUITools.logError(getBundle(), getErrorMessage());
	}
	
	public void executeCurrentQuery() {
		executeQuery();
	}
	
	public void executePreviousQuery() {
		if (rank >= getCountNumber())
			rank = rank - getCountNumber();
		executeQuery();
	}

	public void executeNextQuery() {
		if (total > rank + getCountNumber())
			rank = rank + getCountNumber();
		executeQuery();
	}

	public boolean canReadPreviousResults() {
		return (rank >= getCountNumber());
	}

	public boolean canReadNextResults() {
		return (total > rank + getCountNumber());
	}

	public int getCountNumber() {
		if (isUserDefinedCount() ) {	
			return getUserDefinedCount();
		} else {
			int preferredCount = SearchPreferenceManager.getInstance().getResultCount();
			return (preferredCount > 0) ? preferredCount : DEFAULT_COUNT;
		}
	}

	public boolean isUserDefinedCount(){
		return false;
	}
	public int getUserDefinedCount(){
		return DEFAULT_COUNT;
	}
		
}
