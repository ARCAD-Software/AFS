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
package com.arcadsoftware.cm.simple.internal;

public class RankedElement<T> implements Comparable<RankedElement<T>>, Cloneable {

	private final int rank;
	private final T value;

	public RankedElement(int rank, T value) {
		super();
		this.rank = rank;
		if (value == null) {
			throw new NullPointerException("RankElent value can no be null.");
		}
		this.value = value;
	}

	public int getRank() {
		return rank;
	}

	public T getValue() {
		return value;
	}

	@Override
	public int compareTo(RankedElement<T> o) {
		return o.rank - rank;
	}

	@Override
	public int hashCode() {
		return rank;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof RankedElement) && (rank == ((RankedElement<?>) obj).rank) && value.equals(((RankedElement<?>) obj).value); 
	}

	@Override
	protected RankedElement<T> clone() {
		return new RankedElement<T>(rank, value);
	}

	@Override
	public String toString() {
		return rank + " - " + value.toString(); //$NON-NLS-1$
	}
	
}
