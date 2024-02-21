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
package com.arcadsoftware.rest;

import java.util.Date;

import com.arcadsoftware.rest.internal.Messages;

/**
 * This exception has to be caught to each call to a web service.
 *  
 */
public class ServerErrorException extends Exception {
	
	private static final long serialVersionUID = 968785338053737987L;

	private ErrorMessageBean errorMessage;
	private int errorCode = 1000;
	
	/**
	 * Build an exception from a <code>ErrorMessageBean</code>. 
	 * @param errorCode
	 * @param errorMessage
	 */
	public ServerErrorException(int errorCode, ErrorMessageBean errorMessage) {
		super();
		this.errorMessage = errorMessage;
		this.errorCode = errorCode;
	}
	
	/**
	 * Build an new Server Exception from an HTTP error status and an xml representation of
	 * an error message.
	 * 
	 * @param errorCode
	 * @param xml
	 */
	public ServerErrorException(int errorCode, String xml) {
		super();
		if (xml == null) {
			errorMessage = new ErrorMessageBean();
		} else {
			errorMessage = ErrorMessageBean.deserialize(xml);
		}
		this.errorCode = errorCode;
	}

	/**
	 * @return the server error date if available
	 */
	public Date getDate() {
		return errorMessage.getDate();
	}
	
	/**
	 * @return if available return the error title.
	 */
	public String getTitle() {
		return errorMessage.getName();
	}
	
	/**
	 * @return Internal error message if available.
	 */
	public String getErrorMessage() {
		return errorMessage.getDescription();
	}

	/**
	 * @return the HTTP error code. See HTTP specifications for details. 
	 */
	public int getHTTPCode() {
		return errorCode;
	}

    /**
     * Indicates if the status is a client error status, meaning "The request
     * contains bad syntax or cannot be fulfilled".
     * 
     * @return True if the status is a client error status.
     */
    public boolean isClientError() {
        return (errorCode >= 400) && (errorCode <= 499);
    }

    /**
     * Indicates if the status is a connector error status, meaning "The
     * connector failed to send or receive an apparently valid message".
     * 
     * @return True if the status is a server error status.
     */
    public boolean isConnectorError() {
        return (errorCode >= 1000);
    }

    /**
     * Indicates if the status is a server error status, meaning "The server
     * failed to fulfill an apparently valid request".
     * 
     * @return True if the status is a server error status.
     */
    public boolean isServerError() {
        return (errorCode >= 500) && (errorCode <= 599);
    }

	/**
	 * @return error description in an human readable form, using a rich text format (html).
	 */
	public String getDescription() {
		return errorMessage.getDescription();
	}
	
	/**
	 * @return true if the description is an HTML formated text.
	 */
	public boolean isHtmlMessage() {
		return (errorMessage.getDescription() != null) && 
			errorMessage.getDescription().startsWith("<html"); //$NON-NLS-1$
	}
	
	/**
	 * @return the embedded ErrorMessageBean.
	 */
	public ErrorMessageBean getBean() {
		return errorMessage;
	}

	@Override
	public String getMessage() {
		String result = super.getMessage();
		if (result == null) {
			result = getDescription();
		}
		if (result == null) {
			result = getErrorMessage();
		}
		if (result == null) {
			return Messages.ServerErrorException_Code + errorCode;
		}
		return result;
	}
	
	
}