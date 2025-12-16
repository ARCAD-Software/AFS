package com.arcadsoftware.rest;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.restlet.Response;
import org.restlet.data.CacheDirective;
import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Status;
import org.restlet.representation.OutputRepresentation;
import org.restlet.resource.ResourceException;

/**
 * This Representation is used to send a Stream of Server Send Events. It use a multi-threaded queue of events to
 * communicate with the client.
 * 
 * <p>
 * Create an SSRRepresentation to manage the whole stream and return it from an HTTP call. Them call one of the {@link #pushEvent(String, JSONObject)} methods
 * to store the event from another Thread.
 * 
 * <p>
 * If required, the SSERepresentation can be reused to resume a broken connection, just call the {@link #resume(Response)} method, or {@link #resume(Response, long)}
 * before to use this Representation again. This can be used if the client initiate a HTTP call with the {@link #LAST_EVENT_HEADER}. Note that this
 * implementation does not allow to resume the communication to an event already sent.
 * 
 * @author ARCAD Software
 * @see <a href="https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events">https://html.spec.whatwg.org/multipage/server-sent-events.html</a>
 */
public class SSERepresentation extends OutputRepresentation {

	/**
	 * This HTTP Header may be used by a client to resume a connection with the server.
	 * 
	 * <p>
	 * The value of this header is a Long which is the ID of an event.
	 *  
	 * @see #resume(long)
	 */
	public final static String LAST_EVENT_HEADER = "Last-Event-ID"; //$NON-NLS-1$
	
	/**
	 * The "text/event-stream" is the Media type associated to Server Sent events.
	 */
	public final static MediaType TEXT_EVENTSTREAM = MediaType.register("text/event-stream", "Server Send Event stream"); //$NON-NLS-1$ //$NON-NLS-2$
	
	private static final record Event(String event, Object data) {}
	private static final String EMPTY_JSONOBJECT = "{}";
	private static final byte[] ID = "id: ".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
	private static final byte[] EVENT = "\nevent: ".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
	private static final byte[] DATA = "\ndata: ".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
	private static final byte[] ENDEVENT = "\n\n".getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$
	
	private final AtomicBoolean working;
	private final AtomicBoolean connected;
	private final long pingDelay;
	private final ConcurrentLinkedQueue<Event> queue;
	private final AtomicLong id;
	private volatile long queueMaxSize;
	
	/**
	 * Pre-create a new Server Send Event stream.
	 * 
	 * <p>
	 * In that case the SSErepresentation will have to be resumed when it is used for the first time.
	 * 
	 * @param language The Language used in this stream.
	 */
	public SSERepresentation(Language language) {
		this(null, language, 0);
	}
	
	/**
	 * Create a new Server Send Event stream, just in time, when the client ask for it.
	 * 
	 * @param response The actual HTTP Response object, it require dedicated configuration to correctly process this representation.
	 * @param language The Language used in this stream.
	 */
	public SSERepresentation(Response response, Language language) {
		this(response, language, 0);
	}

	/**
	 * Create a new Server Send Event stream, with a "keep alive" ping mechanism.
	 * 
	 * <p>
	 * By default the "ping" event is an event with "ping" as name and an empty JSON object as data.
	 * 
	 * @param response The actual HTTP Response object, it require dedicated configuration to correctly process this representation.
	 * @param language The Language used in this stream.
	 * @param pingDelay The delay of inactivity between each ping event, a null of negative value disable this message. The delay is given in milli-seconds.
	 */
	public SSERepresentation(Response response, Language language, long pingDelay) {
		super(TEXT_EVENTSTREAM);
		setCharacterSet(CharacterSet.UTF_8);
		setLanguages(Arrays.asList(language));
		working = new AtomicBoolean(true);
		if (pingDelay < 10) {
			this.pingDelay = 0;
		} else {
			this.pingDelay = pingDelay;
		}
		queue = new ConcurrentLinkedQueue<>();
		id = new AtomicLong(1);
		connected = new AtomicBoolean(false);
		setResponse(response);
	}

	/**
	 * Set the response with the required Headers and assign this representation as the entity of it.
	 * 
	 * @param response may be null.
	 */
	protected void setResponse(Response response) {
		if (response != null) {
			response.setCacheDirectives(Arrays.asList(CacheDirective.noCache()));
			response.setEntity(this);
			response.setStatus(Status.SUCCESS_OK);
		}
	}

	@Override
	public void write(OutputStream outputStream) throws IOException {
		connected.set(true);
		try {
			long delay = pingDelay;
			Event event = queue.poll();
			while (working.get() || (event != null)) {
				if (event == null) {
					// Wait a little...
					try {
						Thread.sleep(50);
					} catch (InterruptedException e) {
						working.set(false);
						return;
					}
					if (pingDelay > 0) {
						delay -= 50;
						if (delay <= 0) {
							delay = pingDelay;
							sendEvent(outputStream, "ping", getPingObject()); //$NON-NLS-1$
						}
					}
				} else {
					if (pingDelay > 0) {
						delay = pingDelay;
					}
					sendEvent(outputStream, event.event, event.data);
				}
				event = queue.poll();
			}
		} finally {
			connected.set(false);
		}
	}

	/**
	 * Write the given event message to the OutPutStream.
	 * 
	 * @param outputStream the OutPutStream to write the event message. 
	 * @param event may be null.
	 * @param data may be null.
	 * @throws IOException
	 */
	protected void sendEvent(final OutputStream outputStream, final String event, final Object data) throws IOException {
		try {
			outputStream.write(ID);
			outputStream.write(Long.toString(id.getAndIncrement()).getBytes(StandardCharsets.UTF_8));
			if (event != null) {
				outputStream.write(EVENT);
				outputStream.write(event.getBytes(StandardCharsets.UTF_8));
			}
			outputStream.write(DATA);
			if (data != null) {
				outputStream.write(data.toString().getBytes(StandardCharsets.UTF_8));
			}
			outputStream.write(ENDEVENT);
			outputStream.flush();
		} catch (IOException e) {
			working.set(false);
			throw e;
		}
	}

	/**
	 * Return the JSON Object used in the "ping" keep alive message.
	 * @return
	 */
	protected Object getPingObject() {
		return EMPTY_JSONOBJECT;
	}

	/**
	 * This method allow to terminate the Stream, if the parameter <code>keepWaitingEvents</code> is true
	 * then the stream will wait to send all remaining events before to ends.
	 *  
	 * @param keepWaitingEvents If false the event Queue is cleared now and the stream is terminated.
	 */
	public void terminateEventStream(boolean keepWaitingEvents) {
		if (!keepWaitingEvents) {
			queue.clear();
		}
		working.set(false);
	}
	
	/**
	 * Send an anonymous event with the given data.
	 * 
	 * <p>
	 * The events are always sent asynchronously. 
	 * 
	 * @param data The event data, may be null.
	 */
	public void pushEvent(Object data) {
		queue.offer(new Event(null, data));
		if ((queueMaxSize > 0) && (queue.size() > queueMaxSize)) {
			queue.poll();
		}
	}
	
	/**
	 * Send an event.
	 * 
	 * <p>
	 * The events are always sent asynchronously. 
	 * 
	 * @param event The event name, may be null.
	 * @param data The event data, may be null.
	 */
	public void pushEvent(String event, Object data) {
		queue.offer(new Event(event, data));
		if ((queueMaxSize > 0) && (queue.size() > queueMaxSize)) {
			queue.poll();
		}
	}

	/**
	 * Send a set of anonymous events.
	 * 
	 * <p>
	 * The events are always sent asynchronously. 
	 * 
	 * @param datas An Array of JSONObjects. May be null or empty, but must only content JSONObjects.
	 * @throws ResourceException If the Array content other object than JSONObjects.
	 */
	public void pushEvents(JSONArray datas) throws ResourceException {
		pushEvents(null, datas);
	}
	
	/**
	 * Send a set of events.
	 * 
	 * <p>
	 * The events are always sent asynchronously. 
	 * 
	 * @param event The events name, may be null. The same name is used for all events.
	 * @param datas An Array of JSONObjects. May be null or empty, but must only content JSONObjects.
	 * @throws ResourceException If the Array content other object than JSONObjects.
	 */
	public void pushEvents(String event, JSONArray datas) throws ResourceException {
		if (datas != null) {
			for (int i = 0; i < datas.length(); i++) {
				try {
					queue.offer(new Event(event, datas.get(i)));
				} catch (JSONException e) {
					throw new ResourceException(Status.SERVER_ERROR_INTERNAL, "Only JSON Object are allowed as Server Send Event Data.");
				}
			}
			if (queueMaxSize > 0) {
				while (queue.size() > queueMaxSize) {
					queue.poll();
				}
			}
		}
	}
	
	/**
	 * Resume the Stream. This method must be called before to use it in a new HHTP response. 
	 * 
	 * <p>
	 * A positive number ensure that the first event back will have an ID higher or equals to 
	 * the given number. a negative number indicate that only the latest event will be sent. 
	 * For instance, if the header value is 135, then the first event sent will have at least 
	 * and ID higher or equals to 135. Note that it is not possible to send back an event already 
	 * sent. If the header value is -3 only the lasted three event waiting to be sent will be 
	 * sent back. So to resume the stream to the latest event to be sent you can set the header 
	 * value to -1.
	 * 
	 * @param response The actual HTTP Response object, it require dedicated configuration to 
	 *        correctly process this representation.
	 * @param id if higher than zero ensure that the first event sent will have an ID at least 
	 *        equal to this value. If required currently waiting message will be purged if their ID is to low.
	 * @return the number of event removed from the waiting queue.
	 */
	public int resume(final Response response, final long id) {
		int i = 0;
		if (id > 0) {
			while (this.id.get() < id) {
				this.id.incrementAndGet();
				queue.poll();
				i++;
			}
		} else if (id < 0) {
			long x = -id;
			while (queue.size() > x) {
				queue.poll();
				i++;
			}
		}
		working.set(true);
		setAvailable(true);
		setResponse(response);
		return i;
	}

	/**
	 * Resume the Stream. This method must be called before to use it in a new HHTP response. 
	 * 
	 * @param response The actual HTTP Response object, it require dedicated configuration to correctly process this representation.
	 * @see #resume(Response, long)
	 */
	public void resume(final Response response) {
		working.set(true);
		setResponse(response);
	}

	/**
	 * Return true if the Stream is currently sending events to the client, or waiting a client connection to send them.
	 * 
	 * <p>
	 * This method may have a different value from {@link #isConnected()} is an error occurs during connection.
	 *  
	 * @return true if this Stream is active, or accumulating events to sent.
	 * @see #isConnected()
	 */
	public boolean isWorking() {
		return working.get() || !queue.isEmpty();
	}
	
	/**
	 * Return true if the stream is currently trying to send event. 
	 * 
	 * <p>
	 * If this method return false there is no chance that new events will be sent to the client, except if the client send a new request and this stream is resumed.
	 * 
	 * @return true is this stream is "connected" to the client.
	 * @see #isWorking()
	 * @see #resume(Response, long)
	 */
	public boolean isConnected() {
		return connected.get();
	}

	/**
	 * Get the current limit of event waiting to be send to the client.
	 * 
	 * <p>
	 * Default value is zero, there is no limitation.
	 * 
	 * <p>
	 * Please note that setting no limitation may lead to memory leak.
	 * 
	 * @return
	 */
	public long getQueueMaxSize() {
		return queueMaxSize;
	}

	/**
	 * Set the limit of event stored in the waiting list. When this limit is reach
	 * older message are removed even if they are not sent to the client.
	 * 
	 * @param queueMaxSize any value lower than 10 disable this limitation.
	 */
	public void setQueueMaxSize(long queueMaxSize) {
		if (queueMaxSize < 10) {
			this.queueMaxSize = 0;
		} else {
			this.queueMaxSize = queueMaxSize;
		}
	}
}
