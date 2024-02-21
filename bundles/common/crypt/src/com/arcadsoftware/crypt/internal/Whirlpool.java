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
package com.arcadsoftware.crypt.internal;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * Report of Whirlpool algorithm with same initialization used in AFS 1.0.
 * Used for ascendant compatibility.
 * 
 * @author ARCAD Software
 */
public class Whirlpool {

	private static final long[] T0 = new long[256];
	private static final long[] T1 = new long[256];
	private static final long[] T2 = new long[256];
	private static final long[] T3 = new long[256];
	private static final long[] T4 = new long[256];
	private static final long[] T5 = new long[256];
	private static final long[] T6 = new long[256];
	private static final long[] T7 = new long[256];
	private static final long[] rc = new long[10];

	static {
		final int ROOT = 0x11d;
		final byte[] S = new byte[256];
		final String init = "\u1823\uc6E8\u87B8\u014F\u36A6\ud2F5\u796F\u9152\u60Bc\u9B8E\uA30c\u7B35\u1dE0\ud7c2\u2E4B\uFE57" + //$NON-NLS-1$
				"\u1577\u37E5\u9FF0\u4AdA\u58c9\u290A\uB1A0\u6B85\uBd5d\u10F4\ucB3E\u0567\uE427\u418B\uA77d\u95d8\uFBEE\u7c66\udd17" + //$NON-NLS-1$
				"\u479E\ucA2d\uBF07\uAd5A\u8333\u6302\uAA71\uc819\u49d9\uF2E3\u5B88\u9A26\u32B0\uE90F\ud580\uBEcd\u3448\uFF7A\u905F" + //$NON-NLS-1$
				"\u2068\u1AAE\uB454\u9322\u64F1\u7312\u4008\uc3Ec\udBA1\u8d3d\u9700\ucF2B\u7682\ud61B\uB5AF\u6A50\u45F3\u30EF\u3F55" + //$NON-NLS-1$
				"\uA2EA\u65BA\u2Fc0\udE1c\uFd4d\u9275\u068A\uB2E6\u0E1F\u62d4\uA896\uF9c5\u2559\u8472\u394c\u5E78\u388c\ud1A5\uE261" + //$NON-NLS-1$
				"\uB321\u9c1E\u43c7\uFc04\u5199\u6d0d\uFAdF\u7E24\u3BAB\ucE11\u8F4E\uB7EB\u3c81\u94F7\uB913\u2cd3\uE76E\uc403\u5644" + //$NON-NLS-1$
				"\u7FA9\u2ABB\uc153\udc0B\u9d6c\u3174\uF646\uAc89\u14E1\u163A\u6909\u70B6\ud0Ed\ucc42\u98A4\u285c\uF886"; //$NON-NLS-1$
		for (int i = 0; i < 256; i++) {
			char c = init.charAt(i >>> 1);
			long a = ((i & 1) == 0 ? c >>> 8 : c) & 0xFFL;
			long b = a << 1;
			if (b > 0xFFL) {
				b ^= ROOT;
			}
			long d = b ^ a;
			long e = b << 1;
			if (e > 0xFFL) {
				e ^= ROOT;
			}
			long f = e ^ a;
			long g = e << 1;
			if (g > 0xFFL) {
				g ^= ROOT;
			}
			long h = g ^ a;
			S[i] = (byte) a;
			long x = a << 56 | a << 48 | d << 40 | a << 32 | f << 24 | g << 16 | h << 8 | f;
			T0[i] = x;
			T1[i] = x >>> 8 | x << 56;
			T2[i] = x >>> 16 | x << 48;
			T3[i] = x >>> 24 | x << 40;
			T4[i] = x >>> 32 | x << 32;
			T5[i] = x >>> 40 | x << 24;
			T6[i] = x >>> 48 | x << 16;
			T7[i] = x >>> 56 | x << 8;
		}
		int i = 0;
		int j = 0;
		for (int r = 1; r < 11; r++) {
			rc[i++] = (S[j++] & 0xFFL) << 56 | (S[j++] & 0xFFL) << 48 | (S[j++] & 0xFFL) << 40 | (S[j++] & 0xFFL) << 32 | (S[j++] & 0xFFL) << 24 | (S[j++] & 0xFFL) << 16 | (S[j++] & 0xFFL) << 8 | (S[j++] & 0xFFL);
		}
	}

	private final long[] H;

	public Whirlpool(final char[] text) {
		super();
		H = new long[8];
		int x = 0;
		// Note the bug here: 
		long count = text.length;
		// WARN: It must be conserved for ascendant compatibility.
		ByteBuffer bb = Charset.defaultCharset().encode(CharBuffer.wrap(text));
		final byte[] b = Arrays.copyOf(bb.array(), bb.limit());
		// WARN: Local byte conversion must be conserved for ascendant compatibility.
		int partLen = 64 - x;
		int i = 0;
		byte[]buffer = new byte[64];
		long[] k = new long[8];
		if (b.length >= partLen) {
			System.arraycopy(b, 0, buffer, x, partLen);
			transform(k, buffer, 0);
			for (i = partLen; i + 63 < count; i += 64) {
				transform(k, b, i);
			}
			x = 0;
		}
		if (i < count) {
			System.arraycopy(b, i, buffer, x, (int) count - i);
		}
		x = (int) ((count + 33) % 64);
		if (x == 0) {
			i = 33;
		} else {
			i = 97 - x;
		}
		byte[] tail = new byte[i];
		tail[0] = (byte) 0x80;
		long bits = count * 8;
		i -= 8;
		tail[i++] = (byte) (bits >>> 56);
		tail[i++] = (byte) (bits >>> 48);
		tail[i++] = (byte) (bits >>> 40);
		tail[i++] = (byte) (bits >>> 32);
		tail[i++] = (byte) (bits >>> 24);
		tail[i++] = (byte) (bits >>> 16);
		tail[i++] = (byte) (bits >>> 8);
		tail[i] = (byte) bits;
		x = (int) (count % 64);
		count += tail.length;
		partLen = 64 - x;
		i = 0;
		if (tail.length >= partLen) {
			System.arraycopy(tail, 0, buffer, x, partLen);
			transform(k, buffer, 0);
			for (i = partLen; i + 63 < tail.length; i += 64) {
				transform(k, tail, i);
			}
			x = 0;
		}
		if (i < tail.length) {
			System.arraycopy(tail, i, buffer, x, tail.length - i);
		}
	}

	public byte[] digest() {
		return new byte[] {
				(byte) (H[0] >>> 56), (byte) (H[0] >>> 48), (byte) (H[0] >>> 40), (byte) (H[0] >>> 32), (byte) (H[0] >>> 24), (byte) (H[0] >>> 16), (byte) (H[0] >>> 8), (byte) H[0],
				(byte) (H[1] >>> 56), (byte) (H[1] >>> 48), (byte) (H[1] >>> 40), (byte) (H[1] >>> 32), (byte) (H[1] >>> 24), (byte) (H[1] >>> 16), (byte) (H[1] >>> 8), (byte) H[1],
				(byte) (H[2] >>> 56), (byte) (H[2] >>> 48), (byte) (H[2] >>> 40), (byte) (H[2] >>> 32), (byte) (H[2] >>> 24), (byte) (H[2] >>> 16), (byte) (H[2] >>> 8), (byte) H[2],
				(byte) (H[3] >>> 56), (byte) (H[3] >>> 48), (byte) (H[3] >>> 40), (byte) (H[3] >>> 32), (byte) (H[3] >>> 24), (byte) (H[3] >>> 16), (byte) (H[3] >>> 8), (byte) H[3],
				(byte) (H[4] >>> 56), (byte) (H[4] >>> 48), (byte) (H[4] >>> 40), (byte) (H[4] >>> 32), (byte) (H[4] >>> 24), (byte) (H[4] >>> 16), (byte) (H[4] >>> 8), (byte) H[4],
				(byte) (H[5] >>> 56), (byte) (H[5] >>> 48), (byte) (H[5] >>> 40), (byte) (H[5] >>> 32), (byte) (H[5] >>> 24), (byte) (H[5] >>> 16), (byte) (H[5] >>> 8), (byte) H[5],
				(byte) (H[6] >>> 56), (byte) (H[6] >>> 48), (byte) (H[6] >>> 40), (byte) (H[6] >>> 32), (byte) (H[6] >>> 24), (byte) (H[6] >>> 16), (byte) (H[6] >>> 8), (byte) H[6],
				(byte) (H[7] >>> 56), (byte) (H[7] >>> 48), (byte) (H[7] >>> 40), (byte) (H[7] >>> 32), (byte) (H[7] >>> 24), (byte) (H[7] >>> 16), (byte) (H[7] >>> 8), (byte) H[7]};
	}

	private void transform(long[] k, byte[] in, int offset) {
		long[] n = new long[] {
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL),
				(in[offset++] & 0xFFL) << 56 | (in[offset++] & 0xFFL) << 48 | (in[offset++] & 0xFFL) << 40 | (in[offset++] & 0xFFL) << 32 | (in[offset++] & 0xFFL) << 24 | (in[offset++] & 0xFFL) << 16 | (in[offset++] & 0xFFL) << 8 | (in[offset++] & 0xFFL)};
		System.arraycopy(H, 0, k, 0, 8);
		long[] nn = new long[] {n[0] ^ k[0], n[1] ^ k[1], n[2] ^ k[2], n[3] ^ k[3], n[4] ^ k[4], n[5] ^ k[5], n[6] ^ k[6], n[7] ^ k[7]};
		long [] w = new long[8];
		for (int r = 0; r < 10; r++) {
			long[] kr = new long[] {
					T0[(int) ((k[0] >> 56) & 0xFFL)] ^ T1[(int) ((k[7] >> 48) & 0xFFL)] ^ T2[(int) ((k[6] >> 40) & 0xFFL)] ^ T3[(int) ((k[5] >> 32) & 0xFFL)] ^ T4[(int) ((k[4] >> 24) & 0xFFL)] ^ T5[(int) ((k[3] >> 16) & 0xFFL)] ^ T6[(int) ((k[2] >> 8) & 0xFFL)] ^ T7[(int) (k[1] & 0xFFL)] ^ rc[r],
					T0[(int) ((k[1] >> 56) & 0xFFL)] ^ T1[(int) ((k[0] >> 48) & 0xFFL)] ^ T2[(int) ((k[7] >> 40) & 0xFFL)] ^ T3[(int) ((k[6] >> 32) & 0xFFL)] ^ T4[(int) ((k[5] >> 24) & 0xFFL)] ^ T5[(int) ((k[4] >> 16) & 0xFFL)] ^ T6[(int) ((k[3] >> 8) & 0xFFL)] ^ T7[(int) (k[2] & 0xFFL)],
					T0[(int) ((k[2] >> 56) & 0xFFL)] ^ T1[(int) ((k[1] >> 48) & 0xFFL)] ^ T2[(int) ((k[0] >> 40) & 0xFFL)] ^ T3[(int) ((k[7] >> 32) & 0xFFL)] ^ T4[(int) ((k[6] >> 24) & 0xFFL)] ^ T5[(int) ((k[5] >> 16) & 0xFFL)] ^ T6[(int) ((k[4] >> 8) & 0xFFL)] ^ T7[(int) (k[3] & 0xFFL)],
					T0[(int) ((k[3] >> 56) & 0xFFL)] ^ T1[(int) ((k[2] >> 48) & 0xFFL)] ^ T2[(int) ((k[1] >> 40) & 0xFFL)] ^ T3[(int) ((k[0] >> 32) & 0xFFL)] ^ T4[(int) ((k[7] >> 24) & 0xFFL)] ^ T5[(int) ((k[6] >> 16) & 0xFFL)] ^ T6[(int) ((k[5] >> 8) & 0xFFL)] ^ T7[(int) (k[4] & 0xFFL)],
					T0[(int) ((k[4] >> 56) & 0xFFL)] ^ T1[(int) ((k[3] >> 48) & 0xFFL)] ^ T2[(int) ((k[2] >> 40) & 0xFFL)] ^ T3[(int) ((k[1] >> 32) & 0xFFL)] ^ T4[(int) ((k[0] >> 24) & 0xFFL)] ^ T5[(int) ((k[7] >> 16) & 0xFFL)] ^ T6[(int) ((k[6] >> 8) & 0xFFL)] ^ T7[(int) (k[5] & 0xFFL)],
					T0[(int) ((k[5] >> 56) & 0xFFL)] ^ T1[(int) ((k[4] >> 48) & 0xFFL)] ^ T2[(int) ((k[3] >> 40) & 0xFFL)] ^ T3[(int) ((k[2] >> 32) & 0xFFL)] ^ T4[(int) ((k[1] >> 24) & 0xFFL)] ^ T5[(int) ((k[0] >> 16) & 0xFFL)] ^ T6[(int) ((k[7] >> 8) & 0xFFL)] ^ T7[(int) (k[6] & 0xFFL)],
					T0[(int) ((k[6] >> 56) & 0xFFL)] ^ T1[(int) ((k[5] >> 48) & 0xFFL)] ^ T2[(int) ((k[4] >> 40) & 0xFFL)] ^ T3[(int) ((k[3] >> 32) & 0xFFL)] ^ T4[(int) ((k[2] >> 24) & 0xFFL)] ^ T5[(int) ((k[1] >> 16) & 0xFFL)] ^ T6[(int) ((k[0] >> 8) & 0xFFL)] ^ T7[(int) (k[7] & 0xFFL)],
					T0[(int) ((k[7] >> 56) & 0xFFL)] ^ T1[(int) ((k[6] >> 48) & 0xFFL)] ^ T2[(int) ((k[5] >> 40) & 0xFFL)] ^ T3[(int) ((k[4] >> 32) & 0xFFL)] ^ T4[(int) ((k[3] >> 24) & 0xFFL)] ^ T5[(int) ((k[2] >> 16) & 0xFFL)] ^ T6[(int) ((k[1] >> 8) & 0xFFL)] ^ T7[(int) (k[0] & 0xFFL)]};
			System.arraycopy(kr, 0, k, 0, 8);
			w[0] = T0[(int) ((nn[0] >> 56) & 0xFFL)] ^ T1[(int) ((nn[7] >> 48) & 0xFFL)] ^ T2[(int) ((nn[6] >> 40) & 0xFFL)] ^ T3[(int) ((nn[5] >> 32) & 0xFFL)] ^ T4[(int) ((nn[4] >> 24) & 0xFFL)] ^ T5[(int) ((nn[3] >> 16) & 0xFFL)] ^ T6[(int) ((nn[2] >> 8) & 0xFFL)] ^ T7[(int) (nn[1] & 0xFFL)] ^ kr[0];
			w[1] = T0[(int) ((nn[1] >> 56) & 0xFFL)] ^ T1[(int) ((nn[0] >> 48) & 0xFFL)] ^ T2[(int) ((nn[7] >> 40) & 0xFFL)] ^ T3[(int) ((nn[6] >> 32) & 0xFFL)] ^ T4[(int) ((nn[5] >> 24) & 0xFFL)] ^ T5[(int) ((nn[4] >> 16) & 0xFFL)] ^ T6[(int) ((nn[3] >> 8) & 0xFFL)] ^ T7[(int) (nn[2] & 0xFFL)] ^ kr[1];
			w[2] = T0[(int) ((nn[2] >> 56) & 0xFFL)] ^ T1[(int) ((nn[1] >> 48) & 0xFFL)] ^ T2[(int) ((nn[0] >> 40) & 0xFFL)] ^ T3[(int) ((nn[7] >> 32) & 0xFFL)] ^ T4[(int) ((nn[6] >> 24) & 0xFFL)] ^ T5[(int) ((nn[5] >> 16) & 0xFFL)] ^ T6[(int) ((nn[4] >> 8) & 0xFFL)] ^ T7[(int) (nn[3] & 0xFFL)] ^ kr[2];
			w[3] = T0[(int) ((nn[3] >> 56) & 0xFFL)] ^ T1[(int) ((nn[2] >> 48) & 0xFFL)] ^ T2[(int) ((nn[1] >> 40) & 0xFFL)] ^ T3[(int) ((nn[0] >> 32) & 0xFFL)] ^ T4[(int) ((nn[7] >> 24) & 0xFFL)] ^ T5[(int) ((nn[6] >> 16) & 0xFFL)] ^ T6[(int) ((nn[5] >> 8) & 0xFFL)] ^ T7[(int) (nn[4] & 0xFFL)] ^ kr[3];
			w[4] = T0[(int) ((nn[4] >> 56) & 0xFFL)] ^ T1[(int) ((nn[3] >> 48) & 0xFFL)] ^ T2[(int) ((nn[2] >> 40) & 0xFFL)] ^ T3[(int) ((nn[1] >> 32) & 0xFFL)] ^ T4[(int) ((nn[0] >> 24) & 0xFFL)] ^ T5[(int) ((nn[7] >> 16) & 0xFFL)] ^ T6[(int) ((nn[6] >> 8) & 0xFFL)] ^ T7[(int) (nn[5] & 0xFFL)] ^ kr[4];
			w[5] = T0[(int) ((nn[5] >> 56) & 0xFFL)] ^ T1[(int) ((nn[4] >> 48) & 0xFFL)] ^ T2[(int) ((nn[3] >> 40) & 0xFFL)] ^ T3[(int) ((nn[2] >> 32) & 0xFFL)] ^ T4[(int) ((nn[1] >> 24) & 0xFFL)] ^ T5[(int) ((nn[0] >> 16) & 0xFFL)] ^ T6[(int) ((nn[7] >> 8) & 0xFFL)] ^ T7[(int) (nn[6] & 0xFFL)] ^ kr[5];
			w[6] = T0[(int) ((nn[6] >> 56) & 0xFFL)] ^ T1[(int) ((nn[5] >> 48) & 0xFFL)] ^ T2[(int) ((nn[4] >> 40) & 0xFFL)] ^ T3[(int) ((nn[3] >> 32) & 0xFFL)] ^ T4[(int) ((nn[2] >> 24) & 0xFFL)] ^ T5[(int) ((nn[1] >> 16) & 0xFFL)] ^ T6[(int) ((nn[0] >> 8) & 0xFFL)] ^ T7[(int) (nn[7] & 0xFFL)] ^ kr[6];
			w[7] = T0[(int) ((nn[7] >> 56) & 0xFFL)] ^ T1[(int) ((nn[6] >> 48) & 0xFFL)] ^ T2[(int) ((nn[5] >> 40) & 0xFFL)] ^ T3[(int) ((nn[4] >> 32) & 0xFFL)] ^ T4[(int) ((nn[3] >> 24) & 0xFFL)] ^ T5[(int) ((nn[2] >> 16) & 0xFFL)] ^ T6[(int) ((nn[1] >> 8) & 0xFFL)] ^ T7[(int) (nn[0] & 0xFFL)] ^ kr[7];
			System.arraycopy(w, 0, nn, 0, 8);
		}
		H[0] ^= w[0] ^ n[0];
		H[1] ^= w[1] ^ n[1];
		H[2] ^= w[2] ^ n[2];
		H[3] ^= w[3] ^ n[3];
		H[4] ^= w[4] ^ n[4];
		H[5] ^= w[5] ^ n[5];
		H[6] ^= w[6] ^ n[6];
		H[7] ^= w[7] ^ n[7];
	}
}