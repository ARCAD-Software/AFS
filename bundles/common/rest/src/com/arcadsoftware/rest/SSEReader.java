package com.arcadsoftware.rest;

import java.io.Closeable;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpClient.Builder;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;

import javax.net.ssl.SSLContext;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.arcadsoftware.osgi.ILoggedPlugin;

public class SSEReader implements Closeable {

	public static interface ISSEventListener {
		
		public void serverSentEvent(long id, String event, JSONObject data);
	}
	
	private static Authenticator getBasicAuthenticator(final String login, final char[] password) {
		if ((login == null) || login.isEmpty()) {
			return null;
		}
		return new Authenticator() {
			@Override
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(login, password);
			}
		};
	}
	
	private static final class Event {
		public long id;
		public String name;
		public JSONObject data;
	}
	
	private final ILoggedPlugin activator;
	private final ArrayList<ISSEventListener> listeners;
	private final ArrayList<Event> waitingList;
	private final HttpClient client;
	private final HttpRequest request;
	private volatile Event current;
	private final CompletableFuture<Void> asyncOperation;
	
	public SSEReader(ILoggedPlugin activator, String url, final Authenticator authenticator, final 	SSLContext sslContext) {
		this(activator, url, authenticator, sslContext, null);
	}
	
	public SSEReader(ILoggedPlugin activator, String url, final String login, final char[] password, final 	SSLContext sslContext) {
		this(activator, url, getBasicAuthenticator(login, password), sslContext, null);
	}
	
	public SSEReader(ILoggedPlugin activator, String url, final String login, final char[] password, final 	SSLContext sslContext, final ISSEventListener listener) {
		this(activator, url, getBasicAuthenticator(login, password), sslContext, listener);
	}
	
	public SSEReader(ILoggedPlugin activator, String url, final Authenticator authenticator, final 	SSLContext sslContext, final ISSEventListener listener) {
		super();
		this.activator = activator;
		listeners = new ArrayList<>();
		Builder builder = HttpClient.newBuilder();
		if (authenticator != null) {
			builder.authenticator(authenticator);
		}
		if (sslContext != null) {
 			builder.sslContext(sslContext);
		}
 		client = builder.build();
		request = HttpRequest.newBuilder().GET()
                .uri(URI.create(url))
                .header("Accept", SSERepresentation.TEXT_EVENTSTREAM.getName())
                .build();
		addListener(listener);
		waitingList = new ArrayList<>();
		current = new Event();
		asyncOperation = client.sendAsync(request, HttpResponse.BodyHandlers.ofLines())
        		.thenAccept(response -> response.body().forEachOrdered(SSEReader.this::readLine))
        		.exceptionally(error -> {
        			SSEReader.this.logError(error);
        			return null;
        		});
	}

	protected void readLine(String line) {
		if ((line == null) || line.isBlank()) {
			synchronized (listeners) {
				if (listeners.isEmpty()) {
					waitingList.add(current);
				} else {
					if (!waitingList.isEmpty()) {
						for (Event e: waitingList) {
							sendEvent(e);
						}
						waitingList.clear();
					}
					sendEvent(current);
				}
			}
			current = new Event();
		} else {
			StringBuilder key = new StringBuilder(); 
			StringBuilder value = new StringBuilder();
			boolean isKey = true;
			// FIXME Support multi-line data.
			for (char c: line.toCharArray()) {
				if (isKey) {
					if (c == ':') {
						isKey = false;
					} else if (c != ' ') {
						key.append(Character.toLowerCase(c));
					}
				} else {
					value.append(c);
				}
			}
			if (!key.isEmpty() && !value.isEmpty()) {
				String name = key.toString();
				if ("id".equalsIgnoreCase(name)) {
					try {
						current.id = Long.parseLong(value.toString().trim());
					} catch (NumberFormatException e) {}
				} else if ("event".equalsIgnoreCase(name)) {
					current.name = value.toString().trim();
				} else if ("data".equalsIgnoreCase(name)) {
					// TODO Support multi-line data...
					try {
						current.data = new JSONObject(value.toString());
					} catch (JSONException e) {
						logError(e);
					}
				} else {
					logWarn("Unknown SSE key name: " + name);
				}
			}
		}
	}
	
	private void sendEvent(Event event) {
		if ((event.id > 0) || (event.name != null) || (event.data != null)) {
			for (ISSEventListener listener: listeners) {
				try {
					listener.serverSentEvent(event.id, event.name, event.data);
				} catch (Exception e) {
					logError(e);
				}
			}
		}
	}

	public void addListener(ISSEventListener listener) {
		if (listener != null) {
			synchronized (listeners) {
				listeners.add(listener);
			}
		}
	}
	
	public void removeListener(ISSEventListener listener) {
		if (listener != null) {
			synchronized (listeners) {
				listeners.remove(listener);
			}
		}
	}
	
	protected void logError(Throwable e) {
		if (activator != null) {
			activator.error(e);
		}
	}
	
	protected void logWarn(String message) {
		if (activator != null) {
			activator.warn(message);
		}
	}
	
	@Override
	public void close() {
		asyncOperation.cancel(true);
	}
	
	public boolean isClosed() {
		return asyncOperation.isDone();
	}
}
