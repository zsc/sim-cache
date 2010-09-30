/*
 *
 * Copyright (c) 2009-, 
 *  Shuchang Zhou    <zhoushuchang@ict.ac.cn>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

import java.util.*;
import java.lang.Math;

class BoundedMap {
	HashMap<Integer,Double> h = new HashMap<Integer,Double>();
	HashMap<Integer,Double> h2 = new HashMap<Integer,Double>();
	Double offset = new Double(0);
	final Double threshold;
	Double c = new Double(0);
	public BoundedMap (int n, Double eps) {
		threshold = Math.log(eps) / (Math.log ( 1. - 1. / n));
	}
	public Double get (Integer addr) {
		if (h.containsKey(addr))
			return h.get(addr) + offset;
		if (h2.containsKey(addr))
			return h2.get(addr) + offset;
		return Double.POSITIVE_INFINITY;
	}
	public void set (Integer addr, Double v) {
		h.put(addr, v - offset);
	}
	void swap () {
		h2.clear();
		HashMap<Integer,Double> t = h;
		h = h2;
		h2 = t;
	}
	public void addOffset (Double x) {
		offset = offset + x;
		c = c + x;
		if(c>threshold) {
			swap ();
			c = c - threshold;
		}
	}
}

public class cache {
	BoundedMap bm;
	final Double ratio;
	public cache (Double eps, Integer size) {
		bm = new BoundedMap(size, eps);
		ratio = 1 - 1/new Double(size);
	}
	public Double accept (Integer addr) {
		Double exi = 1. - Math.pow(ratio, bm.get(addr));
		bm.addOffset(exi);
		bm.set(addr, 0.);
		return exi;
	}
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		cache c = new cache (0.1,4); 
		// TODO Auto-generated method stub
		char [] trace = {'a','b','a','c','d','b'};
		for(int i=0;i<trace.length;i++) {
			System.out.printf("%f, ", c.accept((int)trace[i]));
		}
		System.out.println("");
	}

}
