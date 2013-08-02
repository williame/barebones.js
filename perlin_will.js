"use strict";

function PerlinNoiseWill(r,alpha,beta,n) {
	/*	r = an object with a random() method that returns 0..1; omit to randomize
		alpha = noisyness; 2 is smooth, 1 is noisy; default 1.5
		beta = harmonic scaling/spacing; default 2
		n = number of harmonics; default 4
		*/
	r = r || Math;
	alpha = this.alpha = alpha || 2;
	beta = this.beta = beta || 2;
	n = this.n = (n || 4)|0;
	var	B = this.B = 256,
		B1 = 1/B, B2 = B*2, B22 = B2+2, B3 = B*3,
		p = this.p = new Int32Array(B22),
		g1 = this.g1 = new Float32Array(B22),
		g2 = this.g2 = new Float32Array(B22*2),
		g3 = this.g3 = new Float32Array(B22*3),
		i, j, k, x, y, z, s;
	for(i=0; i<B; i++) {
		x = r.random()*2-1;
		y = r.random()*2-1;
		z = r.random()*2-1;
		p[i] = i;
		g1[i] = x;
		s = 1/(Math.sqrt(x*x+y*y)||1);
		g2[i*2+0] = x*s;
		g2[i*2+1] = y*s;
		s = 1/(Math.sqrt(x*x+y*y+z*z)||1);
		g3[i*3+0] = x*s;
		g3[i*3+1] = y*s;
		g3[i*3+2] = z*s;
	}
	for(i=B; i>0; i--) {
		j = (r.random()*i)|0;
		k = p[i];
		p[i] = p[j];
		p[j] = k;
	}
	p.set(p.subarray(0,B+1),B+1);
	g1.set(g1.subarray(0,B),B+1);
	g2.set(g2.subarray(0,B*2),B*2);
	g3.set(g3.subarray(0,B*3),B*3);
	this.scale = new Float32Array(n);
	s = 1;
	for(i = 0; i<n; i++) {
		this.scale[i] = 1/s;
		s *= alpha;
	}
}
PerlinNoiseWill.prototype = {
	_noise1d: function(x) {
		var	bx0, bx1,
			rx0, rx1, sx, t, u, v,
			p = this.p, g1 = this.g1;
		t = x + 0x1000;
		bx0 = t & 0xff;
		bx1 = (bx0+1) & 0xff;
		rx0 = t - (t|0);
		rx1 = rx0 - 1;
		sx = rx0 * rx0 * (3-2*rx0);
		u = rx0 * g1[p[bx0]];
		v = rx1 * g1[p[bx1]];
		return u+sx*(v-u);
	},
	_noise2d: function(x,y) {
		var	bx0, bx1, by0, by1,
			rx0, rx1, ry0, ry1,
			q, sx, sy,
			a, b, t, u, v,
			i, j,
			p = this.p, g2 = this.g2;
		t = x + 0x1000;
		bx0 = t & 0xff;
		bx1 = (bx0+1) & 0xff;
		rx0 = t - (t|0);
		rx1 = rx0 - 1;
		t = y + 0x1000;
		by0 = t & 0xff;
		by1 = (by0+1) & 0xff;
		ry0 = t - (t|0);
		ry1 = ry0 - 1;
		i = p[bx0];
		j = p[bx1];
		sx = rx0*rx0*(3-2*rx0);
		sy = ry0*ry0*(3-2*ry0);
		q = p[i+by0]*2; u = rx0*g2[q] + ry0*g2[q+1];
		q = p[j+by0]*2; v = rx1*g2[q] + ry1*g2[q+1];
		a = u+sx*(v-u);
		q = p[i+by1]*2; u = rx0*g2[q] + ry1*g2[q+1];
		q = p[j+by1]*2; v = rx1*g2[q] + ry1*g2[q+1];
		b = u+sx*(v-u);
		return a+sy*(b-a);
	},
	_noise3d: function(x,y,z) {
		var	bx0, bx1, by0, by1, bz0, bz1, b00, b10, b01, b11,
			rx0, rx1, ry0, ry1, rz0, rz1,
			q, sx, sy, sz,
			a, b, c, d, t, u, v,
			i, j,
			p = this.p, g3 = this.g3;
		t = (x + 0x1000);
		bx0 = t & 0xff;
		bx1 = (bx0+1) & 0xff;
		rx0 = t - (t|0);
		rx1 = rx0 - 1;
		t = y + 0x1000;
		by0 = t & 0xff;
		by1 = (by0+1) & 0xff;
		ry0 = t - (t|0);
		ry1 = ry0 - 1;
		t = z + 0x1000;
		bz0 = t & 0xff;
		bz1 = (bz0+1) & 0xff;
		rz0 = t - (t|0);
		rz1 = rz0 - 1;
		i = p[bx0];
		j = p[bx1];
		b00 = p[i+by0];
		b10 = p[j+by0];
		b01 = p[i+by1];
		b11 = p[j+by1];
		sx = rx0*rx0*(3-2*rx0);
		sy = ry0*ry0*(3-2*ry0);
		sz = rz0*rz0*(3-2*rz0);
		q = (b00+bz0)*3; u = rx0*g3[q] + ry0*g3[q+1] + rz0*g3[q+2];
		q = (b10+bz0)*3; v = rx1*g3[q] + ry0*g3[q+1] + rz0*g3[q+2];
		a = u+sx*(v-u);
		q = (b01+bz0)*3; u = rx0*g3[q] + ry1*g3[q+1] + rz0*g3[q+2];
		q = (b11+bz0)*3; v = rx1*g3[q] + ry1*g3[q+1] + rz0*g3[q+2];
		b = u+sx*(v-u);
		c = a+sy*(b-a);
		q = (b00+bz1)*3; u = rx0*g3[q] + ry0*g3[q+1] + rz1*g3[q+2];
		q = (b10+bz1)*3; v = rx1*g3[q] + ry0*g3[q+1] + rz1*g3[q+2];
		a = u+sx*(v-u);
		q = (b01+bz1)*3; u = rx0*g3[q] + ry1*g3[q+1] + rz1*g3[q+2];
		q = (b11+bz1)*3; v = rx1*g3[q] + ry1*g3[q+1] + rz1*g3[q+2];
		b = u+sx*(v-u);
		d = a+sy*(b-a);
		return c+sz*(d-c);
	},
	noise1d: function(x) {
		var	alpha = this.alpha, beta = this.beta, scale = this.scale,
			n = this.n, i,
			val, sum = 0;
		for(i=0; i<n; i++) {
			val = this._noise1d(x);
			sum += val * scale[i];
			x *= beta;
		}
		return sum;
	},
	noise2d: function(x,y) {
		var	alpha = this.alpha, beta = this.beta, scale = this.scale,
			n = this.n, i,
			val, sum = 0;
		for(i=0; i<n; i++) {
			val = this._noise2d(x,y);
			sum += val * scale[i];
			x *= beta;
			y *= beta;
		}
		return sum;
	},
	noise3d: function(x,y,z) {
		var	alpha = this.alpha, beta = this.beta, scale = this.scale,
			n = this.n, i,
			val, sum = 0;
		for(i=0; i<n; i++) {
			val = this._noise3d(x,y,z);
			sum += val * scale[i];
			x *= beta;
			y *= beta;
			z *= beta;
		}
		return sum;
	},
};
