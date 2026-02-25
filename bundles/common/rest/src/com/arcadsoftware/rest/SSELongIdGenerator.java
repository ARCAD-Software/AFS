package com.arcadsoftware.rest;

import java.util.concurrent.atomic.AtomicLong;

public class SSELongIdGenerator implements ISSEIdGenerator {

	private final AtomicLong counter;
	
	public SSELongIdGenerator() {
		super();
		counter = new AtomicLong(1);
	}

	@Override
	public String getAndIncrement() {
		return Long.toString(counter.getAndIncrement());
	}

	@Override
	public void reset(String id) {
		try {
			counter.set(Long.parseLong(id));
		} catch (NumberFormatException e) {}
	}

	@Override
	public boolean possess(String id) {
		try {
			long i = Long.parseLong(id);
			return (i >= 0) && (i < counter.get());
		} catch (NumberFormatException e) {}
		return false;
	}

	@Override
	public SSELongIdGenerator clone() {
		SSELongIdGenerator result = new SSELongIdGenerator();
		result.counter.set(counter.get());
		return result;
	}

}
