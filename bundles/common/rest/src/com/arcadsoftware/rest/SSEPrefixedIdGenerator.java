package com.arcadsoftware.rest;

import java.util.concurrent.atomic.AtomicLong;

public class SSEPrefixedIdGenerator implements ISSEIdGenerator {

	private final AtomicLong counter;
	private final String prefix;

	public SSEPrefixedIdGenerator(String prefix) {
		super();
		counter = new AtomicLong(1);
		this.prefix = prefix;
	}

	@Override
	public String getAndIncrement() {
		return prefix + Long.toString(counter.getAndIncrement());
	}

	@Override
	public void reset(String id) {
		try {
			if (id.length() > prefix.length()) {
				counter.set(Long.parseLong(id.substring(prefix.length())));
			}
		} catch (NumberFormatException e) {}
	}

	@Override
	public boolean possess(String id) {
		if (id.startsWith(prefix)) {
			try {
				long i = Long.parseLong(id.substring(prefix.length()));
				return (i >= 0) && (i < counter.get());
			} catch (NumberFormatException e) {}
		}
		return false;
	}

	@Override
	public ISSEIdGenerator clone() {
		SSEPrefixedIdGenerator result = new SSEPrefixedIdGenerator(prefix);
		result.counter.set(counter.get());
		return result;
	}

}
