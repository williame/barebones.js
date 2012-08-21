function createShader(str,type) {
	if(!window.x_shaders) window.x_shaders = [];
	var shader = window.x_shaders[[str,type]];
	if(!shader) {
		shader = gl.createShader(type);
		gl.shaderSource(shader,str);
		gl.compileShader(shader);
		if (!gl.getShaderParameter(shader,gl.COMPILE_STATUS))
			throw gl.getShaderInfoLog(shader);
		window.x_shaders[[str,type]] = shader;
	}
	return shader;
}

function createProgram(vstr,fstr) {
	if(!window.x_programs) window.x_programs = [];
	var program = window.x_programs[[vstr,fstr]];
	if(!program) {
		program = gl.createProgram();
		var vshader = createShader(vstr,gl.VERTEX_SHADER);
		var fshader = createShader(fstr,gl.FRAGMENT_SHADER);
		gl.attachShader(program,vshader);
		gl.attachShader(program,fshader);
		gl.linkProgram(program);
		window.x_programs[[vstr,fstr]] = program;
	}
	return program;
}

function createPerspective(fovy,aspect,near,far) {
        var top = near*Math.tan(fovy*Math.PI/360.0);
        var right = top*aspect, left = -right, bottom = -top;
        var rl = (right-left);
        var tb = (top-bottom);
        var fn = (far-near);
        return [(near*2)/rl,	0,	0,	0,
        	0,	(near*2)/tb,	0,	0,
        	(right+left)/rl,	(top+bottom)/tb, -(far+near)/fn,	-1,
        	0,	0,	-(far*near*2)/fn,	0];
}

function createLookAt(eye,centre,up) {
        if (eye[0] == centre[0] && eye[1] == centre[1] && eye[2] == centre[2])
                return [1, 0, 0, 0,
                	0, 1, 0, 0,
                	0, 0, 1, 0,
                	0, 0, 0, 1];
        var z0,z1,z2,x0,x1,x2,y0,y1,y2,len; 
        //vec3.direction(eye, center, z);
        z0 = eye[0] - centre[0];
        z1 = eye[1] - centre[1];
        z2 = eye[2] - centre[2];
        // normalize (no check needed for 0 because of early return)
        len = 1/Math.sqrt(z0*z0 + z1*z1 + z2*z2);
        z0 *= len;
        z1 *= len;
        z2 *= len;
        //vec3.normalize(vec3.cross(up, z, x));
        x0 = up[1]*z2 - up[2]*z1;
        x1 = up[2]*z0 - up[0]*z2;
        x2 = up[0]*z1 - up[1]*z0;
        len = Math.sqrt(x0*x0 + x1*x1 + x2*x2);
        if(len) len = 1/len; else len = 0;
	x0 *= len;
	x1 *= len;
	x2 *= len;
        //vec3.normalize(vec3.cross(z, x, y));
        y0 = z1*x2 - z2*x1;
        y1 = z2*x0 - z0*x2;
        y2 = z0*x1 - z1*x0;
        len = Math.sqrt(y0*y0 + y1*y1 + y2*y2);
        if(len) len = 1/len; else len = 0;
	y0 *= len;
	y1 *= len;
	y2 *= len;
        return [x0, y0, z0, 0,
        	x1, y1, z1, 0,
        	x2, y2, z2, 0,
        	-(x0*eye[0] + x1*eye[1] + x2*eye[2]), -(y0*eye[0] + y1*eye[1] + y2*eye[2]), -(z0*eye[0] + z1*eye[1] + z2*eye[2]), 1];
}

function mat4_multiply(a,b) {
        return [b[0]*a[0] + b[1]*a[4] + b[2]*a[8] + b[3]*a[12],
		b[0]*a[1] + b[1]*a[5] + b[2]*a[9] + b[3]*a[13],
		b[0]*a[2] + b[1]*a[6] + b[2]*a[10] + b[3]*a[14],
		b[0]*a[3] + b[1]*a[7] + b[2]*a[11] + b[3]*a[15],
		b[4]*a[0] + b[5]*a[4] + b[6]*a[8] + b[7]*a[12],
		b[4]*a[1] + b[5]*a[5] + b[6]*a[9] + b[7]*a[13],
		b[4]*a[2] + b[5]*a[6] + b[6]*a[10] + b[7]*a[14],
		b[4]*a[3] + b[5]*a[7] + b[6]*a[11] + b[7]*a[15],
		b[8]*a[0] + b[9]*a[4] + b[10]*a[8] + b[11]*a[12],
		b[8]*a[1] + b[9]*a[5] + b[10]*a[9] + b[11]*a[13],
		b[8]*a[2] + b[9]*a[6] + b[10]*a[10] + b[11]*a[14],
		b[8]*a[3] + b[9]*a[7] + b[10]*a[11] + b[11]*a[15],
		b[12]*a[0] + b[13]*a[4] + b[14]*a[8] + b[15]*a[12],
		b[12]*a[1] + b[13]*a[5] + b[14]*a[9] + b[15]*a[13],
		b[12]*a[2] + b[13]*a[6] + b[14]*a[10] + b[15]*a[14],
		b[12]*a[3] + b[13]*a[7] + b[14]*a[11] + b[15]*a[15]];
}

function mat4_inverse(mat) {
        var a00 = mat[0], a01 = mat[1], a02 = mat[2], a03 = mat[3];
        var a10 = mat[4], a11 = mat[5], a12 = mat[6], a13 = mat[7];
        var a20 = mat[8], a21 = mat[9], a22 = mat[10], a23 = mat[11];
        var a30 = mat[12], a31 = mat[13], a32 = mat[14], a33 = mat[15];
        var b00 = a00*a11 - a01*a10;
        var b01 = a00*a12 - a02*a10;
        var b02 = a00*a13 - a03*a10;
        var b03 = a01*a12 - a02*a11;
        var b04 = a01*a13 - a03*a11;
        var b05 = a02*a13 - a03*a12;
        var b06 = a20*a31 - a21*a30;
        var b07 = a20*a32 - a22*a30;
        var b08 = a20*a33 - a23*a30;
        var b09 = a21*a32 - a22*a31;
        var b10 = a21*a33 - a23*a31;
        var b11 = a22*a33 - a23*a32;
        var invDet = 1/(b00*b11 - b01*b10 + b02*b09 + b03*b08 - b04*b07 + b05*b06);
        return [
		(a11*b11 - a12*b10 + a13*b09)*invDet,
		(-a01*b11 + a02*b10 - a03*b09)*invDet,
		(a31*b05 - a32*b04 + a33*b03)*invDet,
		(-a21*b05 + a22*b04 - a23*b03)*invDet,
		(-a10*b11 + a12*b08 - a13*b07)*invDet,
		(a00*b11 - a02*b08 + a03*b07)*invDet,
		(-a30*b05 + a32*b02 - a33*b01)*invDet,
		(a20*b05 - a22*b02 + a23*b01)*invDet,
		(a10*b10 - a11*b08 + a13*b06)*invDet,
		(-a00*b10 + a01*b08 - a03*b06)*invDet,
		(a30*b04 - a31*b02 + a33*b00)*invDet,
		(-a20*b04 + a21*b02 - a23*b00)*invDet,
		(-a10*b09 + a11*b07 - a12*b06)*invDet,
		(a00*b09 - a01*b07 + a02*b06)*invDet,
		(-a30*b03 + a31*b01 - a32*b00)*invDet,
		(a20*b03 - a21*b01 + a22*b00)*invDet];
}

function mat4_transpose(f) {
	return [
		f[0], f[4], f[8], f[12],
		f[1], f[5], f[9], f[13],
		f[2], f[6], f[10], f[14],
		f[3], f[7], f[11], f[15]];
}
