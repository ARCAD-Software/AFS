package com.arcadsoftware.rest;

/**
 * Callback used to inform upstream object that the client connection is closed.
 * 
 * @author ARCAD Software
 * @see SSERepresentation
 */
public interface ISSERepresentationClosedCallback {

	/**
	 * Called when the HTTP stream is closed, from the client, or any other reasons.
	 * @param representation
	 */
	public void connectionClosed(SSERepresentation representation);
}
