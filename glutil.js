gl.activeTexture(gl.TEXTURE0);
gl.clearColor(0,0,0,1);
gl.enable(gl.DEPTH_TEST);
gl.depthFunc(gl.LEQUAL);
gl.enable(gl.BLEND);
gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);
gl.enable(gl.CULL_FACE);
gl.frontFace(gl.CCW);
gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);

var anisotropic = gl.getExtension("EXT_texture_filter_anisotropic") ||
		gl.getExtension("MOZ_EXT_texture_filter_anisotropic") || 
		gl.getExtension("WEBKIT_EXT_texture_filter_anisotropic"),
	max_anisotropy = anisotropic? gl.getParameter(anisotropic.MAX_TEXTURE_MAX_ANISOTROPY_EXT): 0,
	anisotropy = max_anisotropy,
	_textures = [];	

function set_anisotropy(anisotropy) {
	if(!max_anisotropy) return;
	for(var tex in _textures) {
		tex = _textures[tex];
		gl.bindTexture(gl.TEXTURE_2D,tex);
		gl.texParameterf(gl.TEXTURE_2D,anisotropic.TEXTURE_MAX_ANISOTROPY_EXT,anisotropy);
	}
	window.anisotropy = anisotropy;
	gl.bindTexture(gl.TEXTURE_2D,null);
}


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

function createTexture(width,height,data) {
	var tex = gl.createTexture();
	gl.bindTexture(gl.TEXTURE_2D,tex);
	tex.width = width || data.width;
	tex.height = height || data.height;
	if(width != null)
		gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA,width,height,0,gl.RGBA,gl.UNSIGNED_BYTE,data || null);
	else
		gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA,gl.RGBA,gl.UNSIGNED_BYTE,data);
	if(anisotropy)
		gl.texParameterf(gl.TEXTURE_2D,anisotropic.TEXTURE_MAX_ANISOTROPY_EXT,anisotropy);
	gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_S,gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_T,gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MAG_FILTER,gl.LINEAR);
	if(!(tex.width&(tex.width-1)) && !(tex.height&(tex.height-1))) { //pow2
		gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,gl.LINEAR_MIPMAP_LINEAR);
		gl.generateMipmap(gl.TEXTURE_2D);
	} else
		gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,gl.LINEAR);
	gl.bindTexture(gl.TEXTURE_2D,null);
	_textures.push(tex);
	return tex;
}

function createOrtho2D(left,right,bottom,top,near,far) {
	var undefined;
	if(near === undefined) near = -1;
	if(far === undefined) far = 1;
	var rl = right-left, tb = top-bottom, fn = far-near;
	return [2/rl,0,0,0,
		0,2/tb,0,0,
		0,0,-2/fn,0,
		-((right+left)/rl), -((top+bottom)/tb), -((far+near)/fn),1];
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

function unproject(x,y,mvMatrix,pMatrix,viewport) {
	var 	inv = mat4_inverse(mat4_multiply(pMatrix,mvMatrix));
	var 	near = [(x - viewport[0]) / viewport[2] * 2 - 1,
			(y - viewport[1]) / viewport[3] * 2 - 1,
			-1, 1],
		far = [near[0],near[1],1,1];
	near = vec4_vec3(mat4_vec4_multiply(inv,near));
	far = vec4_vec3(mat4_vec4_multiply(inv,far));
	return [near,vec3_sub(far,near)];
}

function mat4_translation(v) {
	return [1,0,0,0,
		0,1,0,0,
		0,0,1,0,
		v[0],v[1],v[2],1];
}

function mat4_rotation(theta,v) {
	var	x=v[0], y=v[1], z=v[2],
		s = Math.sin(theta),
		c = Math.cos(theta),
		t = 1-c;
	return [x*x*t+c,   x*y*t-z*s, x*z*t+y*s, 0,
		 x*y*t+z*s, y*y*t+c,   y*z*t-x*s, 0,
		 x*z*t-y*s, y*z*t+x*s, z*z*t+c,   0,
		0, 0, 0, 1];
}

function mat4_scale(factor) {
	return [factor,0,0,0,
		0,factor,0,0,
		0,0,factor,0,
		0,0,0,1];
}

function mat4_identity() {
	return [1,0,0,0,
		0,1,0,0,
		0,0,1,0,
		0,0,0,1];
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

function mat4_vec3_multiply(m,v) {
	return [v[0]*m[0] + v[1]*m[4] + v[2]*m[8]  + m[12],
		 v[0]*m[1] + v[1]*m[5] + v[2]*m[9]  + m[13],
		 v[0]*m[2] + v[1]*m[6] + v[2]*m[10] + m[14]];
}

function mat4_vec4_multiply(m,v) {
	return [v[0]*m[0] + v[1]*m[4] + v[2]*m[8]  + v[3]*m[12],
		 v[0]*m[1] + v[1]*m[5] + v[2]*m[9]  + v[3]*m[13],
		 v[0]*m[2] + v[1]*m[6] + v[2]*m[10] + v[3]*m[14],
		 v[0]*m[3] + v[1]*m[7] + v[2]*m[11] + v[3]*m[15]];
}

function vec4_vec3(v) {
	return [v[0]/v[3], v[1]/v[3], v[2]/v[3]];
}

function quat_multiply(a,b) {
	return [
		a[3]*b[0]+a[0]*b[3]+a[1]*b[2]-a[2]*b[1],
		a[3]*b[1]+a[1]*b[3]+a[2]*b[0]-a[0]*b[2],
		a[3]*b[2]+a[2]*b[3]+a[0]*b[1]-a[1]*b[0],
		a[3]*b[3]-a[0]*b[0]-a[1]*b[1]-a[2]*b[2]];
}

function vec3_normalise(v) {
	var len = v[0]*v[0]+v[1]*v[1]+v[2]*v[2];
	if(len && len != 1) {
		var scale = 1/Math.sqrt(len);
		return [v[0]*scale, v[1]*scale, v[2]*scale];
	}
	return v;
}

function vec3_scale(v,f) {
	return [v[0]*f,v[1]*f,v[2]*f];
}

function vec3_neg(v) {
	return [-v[0],-v[1],-v[2]];
}

function vec3_add(a,b) {
	return [a[0]+b[0],a[1]+b[1],a[2]+b[2]];
}

function quat_vec3_multiply(q,v) {
	var	vn = vec3_normalise(v),
		vq = [vn[0],vn[1],v[2],0],
		rq = quat_multiply(q,quat_multiply(vq,[-q[0],-q[1],-q[2],q[3]]));
	return [rq[0],rq[1],rq[2]];
}

var	PI_OVER_180 = Math.PI/180,
	HALF_PI_OVER_180 = PI_OVER_180/2,
	EPSILON = 0.0001;

function quat_from_euler(roll,pitch,yaw) {
	var	p = pitch * HALF_PI_OVER_180,
		y = yaw * HALF_PI_OVER_180,
		r = roll * HALF_PI_OVER_180,
		sinp = Math.sin(p),
		siny = Math.sin(y),
		sinr = Math.sin(r),
		cosp = Math.cos(p),
		cosy = Math.cos(y),
		cosr = Math.cos(r);
	return quat_normalise([
		sinr * cosp * cosy - cosr * sinp * siny,
		cosr * sinp * cosy + sinr * cosp * siny,
		cosr * cosp * siny - sinr * sinp * cosy,
		cosr * cosp * cosy + sinr * sinp * siny]);
}

function quat_slerp(from,to,t) {
	if(from==to)
		return to;
	var scale0 = 1-t, scale1=t, to1 = to;
	var cosom = from[0] * to[0] + from[1] * to[1] + from[2] * to[2] + from[3] * to[3];
	if (cosom <0){
		cosom = -cosom;
		to1 = [-to[0],-to[1],-to[2],-to[3]];
	}
	if((1 - cosom) > EPSILON) {
		// standard case (slerp)
		var omega = Math.acos(cosom), sinom = Math.sin(omega);
		scale0 = Math.sin((1.0 - t) * omega) / sinom;
		scale1 = Math.sin(t * omega) / sinom;
	}
	return [
		scale0 * from[0] + scale1 * to1[0],
		scale0 * from[1] + scale1 * to1[1],
		scale0 * from[2] + scale1 * to1[2],
		scale0 * from[3] + scale1 * to1[3]];
}

function sphere_sphere_intersects(a,b,b_ofs) {
	b_ofs = b_ofs || 0;
	var	i = a[0]-b[b_ofs++],
		j = a[1]-b[b_ofs++],
		k = a[2]-b[b_ofs++],
		d = a[3]+b[b_ofs++];
	return i*i+j*j+k*k <= d*d;
}

function triangle_to_plane(a,b,c) {
	return [a,triangle_normal(a,b,c)];
}

function plane_ray_intersection(plane,ray,is_seg) {
	var j = vec3_dot(plane[1],ray[1]);
	if(float_zero(j)) return null; // parallel, disjoint or on plane
	var k = -vec3_dot(plane[1],vec3_sub(ray[0],plane[0])) / j;
	if(k < 0.0) return null; // ray goes away from triangle
	if(is_seg && k > 1.0) return null; // for a segment, also test if (k > 1.0) => no intersect
	return vec3_add(ray[0],vec3_scale(ray[1],k)); // intersect point of ray and plane
}

function vec3_sub(a,b) {
	return [a[0]-b[0],a[1]-b[1],a[2]-b[2]];
}

function vec3_dot(a,b) {
	return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
}

function vec3_cross(a,b) {
	return [a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]];
}

function vec3_length(v) {
	return Math.sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
}

function vec3_vec4(v,w) {
	return [v[0],v[1],v[2],w];
}

function quat_to_mat4(q) {
	var	xx = q[0] * q[0],
		xy = q[0] * q[1],
		xz = q[0] * q[2],
		xw = q[0] * q[3],
		yy = q[1] * q[1],
		yz = q[1] * q[2],
		yw = q[1] * q[3],
		zz = q[2] * q[2],
		zw = q[2] * q[3];
	return [1-2*(yy+zz), 2*(xy-zw), 2*(xz+yw), 0,
		2*(xy+zw), 1-2*(xx+zz), 2*(yz-xw), 0,
		2*(xz-yw), 2*(yz+xw), 1-2*(xx+yy), 0,
		0, 0, 0, 1];
}

function quat_inverse(q) {
	return [-q[0],-q[1],-q[2],q[3]];
}
var quat_conjugate = quat_inverse;

function quat_forward(q) {
	return vec3_normalise(quat_multiply(quat_multiply(q,[0,0,-1,0]),quat_inverse(q)));
}

function quat_up(q) {
	return quat_vec3_multiply(quat_inverse(q),[0,-1,0]);	
}

function quat_normalise(q) {
	var mag = q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3];
	if(mag > EPSILON && Math.abs(mag-1) > EPSILON) {
		mag = Math.sqrt(mag);
		return [q[0]/mag, q[1]/mag, q[2]/mag, q[3]/mag];
	}
	return q;
}

function triangle_ray_intersection(a,b,c,ray_origin,ray_dir,n,is_seg) {
	// http://softsurfer.com/Archive/algorithm_0105/algorithm_0105.htm#intersect_RayTriangle%28%29
	// get triangle edge vectors and plane normal
	var u = vec3_sub(b,a);
	var v = vec3_sub(c,a);
	n = n || vec3_cross(u,v); // if not passed in, compute it
	if(n[0]==0 && n[1]==0 && n[2]==0) return null; // triangle is degenerate
	var w0 = vec3_sub(ray_origin,a);
	var j = vec3_dot(n,ray_dir);
	if(Math.abs(j) < 0.00000001) return null; // parallel, disjoint or on plane
	var i = -vec3_dot(n,w0);
	// get intersect point of ray with triangle plane
	var k = i / j;
	if(k < 0.0) return null; // ray goes away from triangle
	if(is_seg && k > 1.0) return null; // for a segment, also test if (k > 1.0) => no intersect
	var hit = vec3_add(ray_origin,vec3_scale(ray_dir,k)); // intersect point of ray and plane
	// is I inside T?
	var uu = vec3_dot(u,u);
	var uv = vec3_dot(u,v);
	var vv = vec3_dot(v,v);
	var w = vec3_sub(hit,a);
	var wu = vec3_dot(w,u);
	var wv = vec3_dot(w,v);
	var D = uv * uv - uu * vv;
	var s = (uv * wv - vv * wu) / D;
	if(s<0.0 || s>1.0) return null; // I is outside T
	var t = (uv * wu - uu * wv) / D;
	if(t<0.0 || (s+t)>1.0) return null; // I is outside T
	return [k,hit,n,null,null,null]; // I is in T
}

function sphere_ray_intersects(sphere,ray_origin,ray_dir) {
	var	C0 = vec3_sub(ray_origin,sphere),
		a = vec3_dot(ray_dir,ray_dir),
		b = 2 * vec3_dot(C0,ray_dir),
		c = vec3_dot(C0,C0) - sphere[3]*sphere[3],
		discriminant = b * b - 4 * a * c;
	if(discriminant < 0)
		return null;
	return [a*2,-b,discriminant];
}

function vec3_rotate(v,rad,axis1,axis2) {
	// http://local.wasp.uwa.edu.au/~pbourke/geometry/rotate/example.c
	var	q1 = vec3_sub(v,axis1),
		q2 = [0,0,0],
		u = vec3_normalise(vec3_sub(axis2,axis1)),
		d = Math.sqrt(u[1]*u[1] + u[2]*u[2]),
		cosrad = Math.cos(rad),
		sinrad = Math.sin(rad);
	if(d != 0) {
		q2[0] = q1[0];
		q2[1] = q1[1] * u[2] / d - q1[2] * u[1] / d;
		q2[2] = q1[1] * u[1] / d + q1[2] * u[2] / d;
	} else
		q2 = q1;
	q1[0] = q2[0] * d - q2[2] * u[0];
	q1[1] = q2[1];
	q1[2] = q2[0] * u[0] + q2[2] * d;
	q2[0] = q1[0] * cosrad - q1[1] * sinrad;
	q2[1] = q1[0] * sinrad + q1[1] * cosrad;
	q2[2] = q1[2];
	q1[0] =   q2[0] * d + q2[2] * u[0];
	q1[1] =   q2[1];
	q1[2] = - q2[0] * u[0] + q2[2] * d;
	if (d != 0) {
		q2[0] =   q1[0];
		q2[1] =   q1[1] * u[2] / d + q1[2] * u[1] / d;
		q2[2] = - q1[1] * u[1] / d + q1[2] * u[2] / d;
	} else
		q2 = q1;
	q1 = vec3_add(q2,axis1);
	return q1;
}

function vec3_distance_sqrd(a,b) {
	var d = vec3_sub(a,b);              
	return vec3_dot(d,d);
}

function vec3_distance(a,b) {
	return Math.sqrt(vec3_distance_sqrd(a,b));
}

function vec3_lerp(a,b,k) {
	return vec3_add(a,vec3_scale(vec3_sub(b,a),k));
}

function ray_to_line(ray) {
	return [ray[0],vec3_add(ray[0],ray[1])];
}

function line_to_ray(line) {
	return [line[0],vec3_sub(line[1],line[0])];
}

function line_line_closest_point_ofs_3(line1,line2) {
	var	d21 = vec3_sub(line1[0],line1[1]), // note order
		d34 = vec3_sub(line2[1],line2[0]),
		d13 = vec3_sub(line2[0],line1[0]),
		a = vec3_dot(d21,d21),
		b = vec3_dot(d21,d34),
		c = vec3_dot(d34,d34),
		d = -vec3_dot(d13,d21),
		e = -vec3_dot(d13,d34),
		u1 = (d*c-e*b)/(c*a-b*b),
		u2 = (e-b*u1) / c;
	return [Math.min(Math.max(0,u1),1),
		Math.min(Math.max(0,u2),1)];
}

function line_line_closest_point_3(line1,line2) {
	var ofs = line_line_closest_point_ofs_3(line1,line2);
	return [ofs[0],vec3_lerp(line1[0],line1[1],ofs[0]),
		ofs[1],vec3_lerp(line2[0],line2[1],ofs[1])];
}

function line_line_closest_point_ofs_2(line1,line2) {
	var	v1 = vec3_sub(line1[1],line1[0]),
		v2 = vec3_sub(line2[1],line2[0]),
		v1dotv2 = vec3_dot(v1,v2),
		denom = v1dotv2 * v1dotv2 - 1;
	assert(!float_zero(denom),"lines are parallel");
	var	c = 1/denom,
		dot1 = vec3_dot(vec3_sub(line2[0],line1[0]),v1),
		dot2 = vec3_dot(vec3_sub(line2[0],line1[0]),v2),
		t1 = c * (-1 * dot1 + v1dotv2 * dot2),
		t2 = c * (-v1dotv2 * dot1 + 1 * dot2);
	t1 = Math.min(Math.max(0,t1),1);
	t2 = Math.min(Math.max(0,t2),1);
	return [t1,t2];
}

function line_line_closest_point_2(line1,line2) {
	var ofs = line_line_closest_point_ofs_2(line1,line2);
	return [ofs[0],vec3_lerp(line1[0],line1[1],ofs[0]),
		ofs[1],vec3_lerp(line2[0],line2[1],ofs[1])];
}

function line_line_closest_point_ofs(line1,line2) {
	//#### don't trust this code at all!  must have transcoded it wrong or something
	function Dmnop(v,m,n,o,p) {
		return(v[m][0]-v[n][0])*(v[o][0]-v[p][0])+(v[m][1]-v[n][1])*(v[o][1]-v[p][1])+(v[m][2]-v[n][2])*(v[o][2]-v[p][2]);
	}
	var v = [line1[0],vec3_normalise(vec3_sub(line1[1],line1[0])),
		line2[0],vec3_normalise(vec3_sub(line2[1],line2[0]))];
	var	d0232 = Dmnop(v,0,2,3,2),
		d3210 = Dmnop(v,3,2,1,0),
		d3232 = Dmnop(v,3,2,3,2),
		mu = (d0232 * d3210 - Dmnop(v,0,2,1,0)*d3232) / (Dmnop(v,1,0,1,0)*Dmnop(v,3,2,3,2) - Dmnop(v,3,2,1,0)*Dmnop(v,3,2,1,0)),
		u = Math.min(Math.max(0,mu),1),
		v = Math.min(Math.max(0,(d0232 + mu * d3210) / d3232),1);
	return [u,v];
}

function line_line_closest_point(line1,line2) {
	var ofs = line_line_closest_point_ofs(line1,line2);
	return [ofs[0],vec3_lerp(line1[0],line1[1],ofs[0]),
		ofs[1],vec3_lerp(line2[0],line2[1],ofs[1])];
}

function tri_seg_loci_will(a,b,c,seg_origin,seg_dir,seg_radius) {
	// get triangle edge vectors and plane normal
	var	u = vec3_sub(b,a),
		v = vec3_sub(c,a),
		n = vec3_cross(u,v);
	if(n[0]==0&&n[1]==0&&n[2]==0) return null; //triangle is degenerate
	var	j = vec3_dot(n,seg_dir);
	if(float_zero(j)) //TODO parallel?
		return null; // parallel, disjoint or on plane
	// get intersect point of ray with triangle plane
	var	w0 = vec3_sub(seg_origin,a),
		i = -vec3_dot(n,w0),
		k = i / j;
	// going away?
	if(k <= 0) // we don't care for ball at start end
		return null; // ray goes away from triangle
	// too far ahead?
	k -= seg_radius/vec3_length(seg_dir);
	if(k > 1)
		return null;
	// intersect point of seg and plane
	var hit = vec3_add(seg_origin,vec3_scale(seg_dir,k));
	// now test each edge; we are inside (right), or are we too far away (left)
	var sides = [[a,b,c],[b,a,c],[c,a,b]];
	for(var side in sides) {
		side = sides[side];
		// check if inside
		var	v = vec3_sub(side[2],side[1]),
			w = vec3_sub(hit,side[1]),
			cp1 = vec3_cross(v,w),
			cp2 = vec3_cross(v,vec3_sub(side[0],side[1]));
		if(vec3_dot(cp1, cp2) >= 0)
			continue; // check next side
		// outside, so where on the side line does it hit?
		var	c1 = vec3_dot(w,v),
			nearest;
		if(c1 <= 0)
			nearest = side[1];
		else {
			var c2 = vec3_dot(v,v);
			if(c2 <= c1)
				nearest = side[2];
			else {
				var c3 = c1 / c2;
				nearest = vec3_add(side[1],vec3_scale(v,c3))
			}
		}
		if(vec3_distance_sqrd(hit,nearest) < seg_radius*seg_radius)
			break;
		return null;
	}
	console.log("hit",a,b,c,"and",seg_origin,seg_dir,seg_radius,"->",hit,k,n);
	return [k,hit,n];
}

function plane_point_distance(pos,normal,point) {
	return vec3_dot(normal,vec3_sub(point,pos));
}

function float_equ(a,b) {
	return Math.abs(a-b) < 0.00000001;
}

function float_zero(f) {
	return Math.abs(f) < 0.00000001;
}

function triangle_sphere_sweep(a,b,c,start,stop,radius,twoSided) { //clockwise winding!
	removeMessage(triangle_sphere_sweep);
	// get the normal of the triangle
	var	u = vec3_sub(b,a),
		v = vec3_sub(c,a),
		n = vec3_cross(u,v);
	if(n[0]==0 && n[1]==0 && n[2]==0) { // triangle is degenerate
		addMessage(0,null,"triangle is degenerate",triangle_sphere_sweep);
		return null;
	}
	var	dir = vec3_sub(stop,start),
		j = vec3_dot(n,dir);
	var colinear = float_zero(j); // parallel, disjoint or on plane
	// which side of triangle is line?
	var start_height = vec3_dot(n,vec3_sub(start,a));
	if(start_height < 0) {
		addMessage(0,null,"line is beneath triangle",triangle_sphere_sweep);
		return null;
	}
	var stop_height = vec3_dot(n,vec3_sub(stop,a));
	if(stop_height > start_height) {
		addMessage(0,null,"wrong slope",triangle_sphere_sweep);
		return null;
	}
	colinear = colinear || (float_equ(start_height,stop_height) && float_equ(start_height,radius));
	if(colinear)
		addMessage(0,null,"colinear",triangle_sphere_sweep);
	if(!colinear && start_height > radius) { // far enough above to hit the triangle itself?
		// get intersect point of ray with triangle plane that is radius above it
		var	normal = vec3_normalise(n),
			start_plane = vec3_add(a,vec3_scale(normal,radius)),
			i = -vec3_dot(n,vec3_sub(start,start_plane)),
			k = i / j;
		if(k < 0) { // line goes away from triangle
			addMessage(0,null,"line goes away from triangle",triangle_sphere_sweep);
			return null; 
		}
		if(k > 1) { // to far on line
			addMessage(0,null,"line stops before triangle",triangle_sphere_sweep);
			return null;
		}
		// do we hit the triangle radius above?
		var	hit = vec3_add(start,vec3_scale(dir,k)), // intersect point of ray and plane at radius above it
			uu = vec3_dot(u,u),
			uv = vec3_dot(u,v),
			vv = vec3_dot(v,v),
			w = vec3_sub(hit,start_plane),
			wu = vec3_dot(w,u),
			wv = vec3_dot(w,v),
			D = uv * uv - uu * vv,
			s = (uv * wv - vv * wu) / D;
		if(s>=0 && s<=1) {
			var t = (uv * wu - uu * wv) / D;
			if(t>=0 && (s+t)<=1) {
				addMessage(0,null,"hit in triangle",triangle_sphere_sweep);
				return [k,normal];
			}
		}
	}
	// we are on right side of the triangle, but we're not hitting exactly; maybe we hit an edge then?
	var	best_pt = null, best_ofs,
		line = [start,stop],
		line_scale,
		radius2 = radius*radius,
		sides = [[a,b],[b,c],[c,a]];
	for(var side in sides) {
		side = sides[side];
		var	nearest_ofs = line_line_closest_point_ofs_2(line,side), //[ofs_on_line,ofs_on_side]
			nearest_pt = [vec3_lerp(start,stop,nearest_ofs[0]),
				vec3_lerp(side[0],side[1],nearest_ofs[1])],
			nearest_dist2 = vec3_distance_sqrd(nearest_pt[0],nearest_pt[1]);
		if(nearest_dist2 <= radius2) {
			line_scale = line_scale || 1/vec3_length(vec3_sub(stop,start)); // lazy compute
			var ofs = nearest_ofs[0] - (Math.sqrt(radius2-nearest_dist2) * line_scale);
			if((ofs > 0) && (best_pt == null || ofs < best_ofs)) {
				best_pt = nearest_pt[1];
				best_ofs = ofs;
			}
		}
	}
	if(best_pt != null) {
		addMessage(0,null,"hit edge",triangle_sphere_sweep);
		var	hit = vec3_lerp(start,stop,best_ofs),
			hit_dist2 = vec3_distance_sqrd(hit,best_pt);
			if(!float_equ(hit_dist2,radius2))
				addMessage(0,null,"wrong distance! "+hit_dist2+" != "+radius2+" ("+(hit_dist2-radius2)+")");
		return [best_ofs,vec3_normalise(vec3_sub(hit,best_pt))];
	}
	// no hit
	addMessage(0,null,"miss",triangle_sphere_sweep);
	return null;
}

function sphere_ray_intersection(sphere,ray_origin,ray_dir) {
	var i = sphere_ray_intersects(sphere,ray_origin,ray_dir);
	if(i == null)
		return null;
	var	discriminant = Math.sqrt(i[2]),
		tmin = (i[1] - discriminant) / i[0],
		tmax = (i[1] + discriminant) / i[0];
	if(tmin > tmax)
		return [tmax,tmin];
	return [tmin,tmax];
}

function capsule_ray_intersection(a,b,radius,line_origin,line_dir) {
	// http://blog.makingartstudios.com/?p=286 thank you!
	var	AB = vec3_sub(b,a),
		AO = vec3_sub(line_origin,a),
		AB_dot_d = vec3_dot(AB,line_dir),
		AB_dot_AO = vec3_dot(AB,AO),
		AB_dot_AB = vec3_dot(AB,AB),
		m = AB_dot_d / AB_dot_AB,
		n = AB_dot_AO / AB_dot_AB,
		Q = vec3_sub(line_dir,vec3_scale(AB,m)),
		R = vec3_sub(AO,vec3_scale(AB,n)),
		A = vec3_dot(Q,Q),
		B = 2 * vec3_dot(Q,R),
		C = vec3_dot(R,R) - (radius * radius),
		t1, p1, n1, t2, p2, n2; // for return
	if(A == 0.0) {
		// Special case: AB and ray direction are parallel. If there is an intersection it will be on the end spheres...
		var	sphereA = [a[0],a[1],a[2],radius],
			sphereB = [b[0],b[1],b[2],radius],
			intersectA = sphere_ray_intersection(sphereA,line_origin,line_dir),
			intersectB = sphere_ray_intersection(sphereB,line_origin,line_dir);
		if(!intersectA || !intersectB)
			return false;
		if(intersectA[0] < intersectB[0]) {
			t1 = intersectA[0];
			p1 = vec3_add(line_origin,vec3_scale(line_dir,t1));
			n1 = vec3_normalise(vec3_sub(p1,a));
		} else {
			t1 = intersectB[0];
			p1 = vec3_add(line_origin,vec3_scale(line_dir,t1));
			n1 = vec3_normalise(vec3_sub(p1,b));
		}
		if(intersectA[1] > intersectB[1]) {
			t2 = intersectA[1];
			p2 = vec3_add(line_origin,vec3_scale(line_dir,t2));
			n2 = vec3_normalise(vec3_sub(p2,a));
		} else {
			t2 = intersectB[1];
			p2 = vec3_add(line_origin,vec3_scale(line_dir,t2));
			n2 = vec3_normalise(vec3_sub(p2,b));
		}
	} else {
		var discriminant = B * B - 4 * A * C;
		if(discriminant < 0) // The ray doesn't hit the infinite cylinder defined by (A, B)
			return null;
		discriminant = Math.sqrt(discriminant);
		var	tmin = (-B - discriminant) / (2 * A),
			tmax = (-B + discriminant) / (2 * A);
		if(tmin > tmax) {
			var temp = tmin;
			tmin = tmax;
			tmax = temp;
		}
		// Now check to see if K1 and K2 are inside the line segment defined by A,B
		var t_k1 = tmin * m + n;
		if(t_k1 < 0) {
			// On sphere (A, r)...
			var s = sphere_ray_intersection([a[0],a[1],a[2],radius],line_origin,line_dir);
			if(!s) return null;
			t1 = s[0];
			p1 = vec3_add(line_origin,vec3_scale(line_dir,t1));
			n1 = vec3_normalise(vec3_sub(p1,a));
			return [t1,p1,n1,null,null,null];
		} else if(t_k1 > 1) {
			// On sphere (B, r)...
			var s = sphere_ray_intersection([b[0],b[1],b[2],radius],line_origin,line_dir);
			if(!s) return null;
			t1 = s[0];
			p1 = vec3_add(line_origin,vec3_scale(line_dir,t1));
			n1 = vec3_normalise(vec3_sub(p1,b));
			return [t1,p1,n1,null,null,null];
		} else {
			// On the cylinder...
			t1 = tmin;
			p1 = vec3_add(line_origin,vec3_scale(line_dir,t1));
			n1 = vec3_normalise(vec3_sub(p1,vec3_add(a,vec3_scale(AB,t_k1))));
		}
		var t_k2 = tmax * m + n;
		if(t_k2 < 0) {
			// On sphere (A, r)...
			var s = sphere_ray_intersection([a[0],a[1],a[2],radius],line_origin,line_dir);
			if(!s) return null;
			t2 = s[1];
			p2 = vec3_add(line_origin,vec3_scale(line_dir,t2));
			n2 = vec3_normalise(vec3_sub(p2,a));
		} else if(t_k2 > 1) {
			// On sphere (B, r)...
			var s = sphere_ray_intersection([b[0],b[1],b[2],radius],line_origin,line_dir);
			if(!s) return null;
			t2 = s[1];
			p2 = vec3_add(line_origin,vec3_scale(line_dir,t2));
			n2 = vec3_normalise(vec3_sub(p2,b));
		} else {
			t2 = tmax;
			p2 = vec3_add(line_origin,vec3_scale(line_dir,t2));
			n2 = vec3_normalise(vec3_sub(p2,vec3_add(a,vec3_scale(AB,t_k2))));
		}
	}
	return [t1,p1,n1,t2,p2,n2];
}
	
function triangle_normal(a,b,c) {
	var u = vec3_sub(b,a);
	var v = vec3_sub(c,a);
	return vec3_cross(u,v);
}

function sqr(x) { return x*x; }

function Sphere(iterations) {
	if(!Sphere.spheres)
		Sphere.spheres = [];
	if(iterations in Sphere.spheres)
		return Sphere.spheres[iterations];
	var 	vertices = [],
		vIndex = {},
		indices = [],
		addTriangle = function(a,b,c) {
			indices.push(a);
			indices.push(b);
			indices.push(c);
		},
		addVertex = function(v) {
			if(!(v in vIndex)) {
				vIndex[v] = vertices.length/3;
				vertices.push(v[0]);
				vertices.push(v[1]);
				vertices.push(v[2]);
			}
			return vIndex[v];
		},
		halfway = function(a,b) {
			a = [vertices[a*3],vertices[a*3+1],vertices[a*3+2]];
			b = [vertices[b*3],vertices[b*3+1],vertices[b*3+2]];
			return addVertex(vec3_normalise(vec3_add(a,vec3_scale(vec3_sub(b,a),0.5))));
		},
		bisect = function(a,b,c,iteration) {
			var	ab = halfway(a,b),
				ac = halfway(a,c),
				bc = halfway(b,c),
				func = iteration==iterations? addTriangle: bisect;
			func(a,ab,ac,iteration+1);
			func(b,bc,ab,iteration+1);
			func(c,ac,bc,iteration+1);
			func(ab,bc,ac,iteration+1);
		},
		top = addVertex([0,-1,0]),
		bottom = addVertex([0,1,0]),
		leftFront = addVertex(vec3_normalise([-1,0,-1])),
		leftBack = addVertex(vec3_normalise([-1,0,1])),
		rightFront = addVertex(vec3_normalise([1,0,-1])),
		rightBack = addVertex(vec3_normalise([1,0,1]));
	bisect(leftFront,top,rightFront,0);
	bisect(rightFront,bottom,leftFront,0);
	bisect(leftBack,top,leftFront,0);
	bisect(bottom,leftBack,leftFront,0);
	bisect(rightFront,top,rightBack,0);
	bisect(bottom,rightFront,rightBack,0);
	bisect(rightBack,top,leftBack,0);
	bisect(bottom,rightBack,leftBack,0);
	if(!Sphere.program) {
		Sphere.program = createProgram(
			"precision mediump float;\n"+
			"uniform mat4 mvMatrix, pMatrix, nMatrix;\n"+
			"attribute vec3 vertex;\n"+
			"varying vec3 lighting;\n"+
			"void main() {\n"+
			"	vec3 normal = vertex;\n"+
			"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
			"	vec3 ambientLight = vec3(0.6,0.6,0.6);\n"+
			"	vec3 lightColour = vec3(0.8,0.9,0.75);\n"+
			"	vec3 lightDir = vec3(0.85,0.8,0.75);\n"+
			"	vec3 transformed = normalize(nMatrix * vec4(normal,1.0)).xyz;\n"+
			"	float directional = clamp(dot(transformed,lightDir),0.0,1.0);\n"+
			"	lighting = ambientLight + (lightColour*directional);\n"+
			"}",
			"precision mediump float;\n"+
			"uniform vec4 colour;\n"+
			"varying vec3 lighting;\n"+
			"void main() {\n"+
			"	gl_FragColor = vec4(lighting*colour.rgb,colour.a);\n"+
			"}");
		Sphere.program.vertex = gl.getAttribLocation(Sphere.program,"vertex");
		Sphere.program.nMatrix = gl.getUniformLocation(Sphere.program,"nMatrix");
		Sphere.program.mvMatrix = gl.getUniformLocation(Sphere.program,"mvMatrix");
		Sphere.program.pMatrix = gl.getUniformLocation(Sphere.program,"pMatrix");
		Sphere.program.colour = gl.getUniformLocation(Sphere.program,"colour");
	}
	var self = {
		vVbo: gl.createBuffer(),
		iVbo: gl.createBuffer(),
		indexCount: indices.length,
		draw: function(pMatrix,mvMatrix,sphere,colour,invert) {
			var frontFace = gl.getParameter(gl.FRONT_FACE);
			gl.frontFace(invert? gl.CCW: gl.CW);
			mvMatrix = mat4_multiply(mvMatrix,mat4_translation(sphere));
			mvMatrix = mat4_multiply(mvMatrix,mat4_scale(sphere[3]));
			gl.useProgram(Sphere.program);
			gl.uniformMatrix4fv(Sphere.program.pMatrix,false,pMatrix);
			gl.uniformMatrix4fv(Sphere.program.mvMatrix,false,mvMatrix);
			gl.uniformMatrix4fv(Sphere.program.nMatrix,false,mat4_transpose(mvMatrix));
			gl.uniform4fv(Sphere.program.colour,colour||[1,1,1,1]);
			gl.enableVertexAttribArray(Sphere.program.vertex);
			gl.bindBuffer(gl.ARRAY_BUFFER,self.vVbo);
			gl.vertexAttribPointer(Sphere.program.vertex,3,gl.FLOAT,false,3*4,0);
			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,self.iVbo);
			gl.drawElements(gl.TRIANGLES,self.indexCount,gl.UNSIGNED_SHORT,0);
			gl.disableVertexAttribArray(Sphere.program.vertex);
			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
			gl.useProgram(null);
			gl.frontFace(frontFace);
		},
	};
	gl.bindBuffer(gl.ARRAY_BUFFER,self.vVbo);
	gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(vertices),gl.STATIC_DRAW);
	gl.bindBuffer(gl.ARRAY_BUFFER,null);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,self.iVbo);
	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,new Uint16Array(indices),gl.STATIC_DRAW);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
	Sphere.spheres[iterations] = self;
	return self;
}

function string_vec(s) { // if you use a vec as an object key, it gets stringified; unpacker
	var decode = s.split(","), v = [];
	for(var dim in decode)
		v.push(parseFloat(decode[dim]));
	return v;
}

function vec3(array,ofs) {
	ofs = ofs || 0;
	return [array[ofs++],array[ofs++],array[ofs++]];
}

var programs = {
	blankTex: createTexture(1,1,new Uint8Array([255,255,255,255])),
	standard: function(cb,uniforms) {
		assert(cb);
		if(!programs.standard.program) {
			var program = programs.standard.program = createProgram(
				"precision mediump float;\n"+
				"varying vec2 texel;\n"+
				"varying vec3 lighting;\n"+
				"attribute vec3 vertex;\n"+
				"attribute vec3 normal;\n"+
				"attribute vec2 texCoord;\n"+
				"uniform mat4 mvMatrix, pMatrix, nMatrix;\n"+
				"uniform vec3 lightDir, ambientLight, lightColour;\n"+
				"void main() {\n"+
				"	vec3 transformed = normalize(nMatrix * vec4(normal,1.0)).xyz;\n"+
				"	float directional = clamp(dot(transformed,lightDir),0.0,1.0);\n"+
				"	lighting = ambientLight + (lightColour*directional);\n"+
				"	texel = vec2(texCoord.x,texCoord.y);\n"+
				"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
				"}\n",
				"precision mediump float;\n"+
				"varying vec2 texel;\n"+
				"varying vec3 lighting;\n"+
				"uniform sampler2D texture;\n"+
				"uniform vec4 colour;\n"+
				"uniform float fogDensity;\n"+
				"uniform vec4 fogColour;\n"+
				"const float LOG2 = 1.442695;\n"+
				"void main() {\n"+
				"	float z = gl_FragCoord.z / gl_FragCoord.w;\n"+
				"	float fogFactor = exp2(-fogDensity*fogDensity*z*z*LOG2);\n"+
				"	fogFactor = clamp(fogFactor,0.0,1.0);\n"+
				"	vec4 fragColour = texture2D(texture,texel) * colour;\n"+
				"	fragColour.rgb *= lighting;\n"+
				"	gl_FragColor = mix(fogColour,fragColour,fogFactor);\n"+
				"}\n");
			program.vertex =	gl.getAttribLocation(program,"vertex");
			program.normal =	gl.getAttribLocation(program,"normal");
			program.texCoord =	gl.getAttribLocation(program,"texCoord");
			program.mvMatrix =	gl.getUniformLocation(program,"mvMatrix");
			program.pMatrix =	gl.getUniformLocation(program,"pMatrix");
			program.nMatrix =	gl.getUniformLocation(program,"nMatrix");
			program.texture =	gl.getUniformLocation(program,"texture");
			program.colour =	gl.getUniformLocation(program,"colour");
			program.lightDir =	gl.getUniformLocation(program,"lightDir");
			program.ambientLight =	gl.getUniformLocation(program,"ambientLight");
			program.lightColour =	gl.getUniformLocation(program,"lightColour");
			program.fogDensity =	gl.getUniformLocation(program,"fogDensity");
			program.fogColour =	gl.getUniformLocation(program,"fogColour");
		} else
			var program = programs.standard.program;
		uniforms = uniforms || {};
		gl.useProgram(program);
		gl.uniformMatrix4fv(program.pMatrix,false,uniforms.pMatrix || camera.pMatrix);
		gl.uniformMatrix4fv(program.mvMatrix,false,uniforms.mvMatrix || camera.mvMatrix);
		gl.uniformMatrix4fv(program.nMatrix,false,uniforms.nMatrix || camera.nMatrix);
		gl.uniform4fv(program.colour,uniforms.colour || [1,1,1,1]);
		gl.uniform1i(program.texture,0);
		gl.bindTexture(gl.TEXTURE_2D,uniforms.tex || programs.blankTex);
		gl.uniform3fv(program.lightDir,uniforms.lightDir || [0.85,0.8,0.75]);
		gl.uniform3fv(program.ambientLight,uniforms.ambientLight || [0.6,0.6,0.6]);
		gl.uniform3fv(program.lightColour,uniforms.lightColour || [0.8,0.9,0.75]);
		gl.uniform1f(program.fogDensity,uniforms.fogDensity || 0); // default no fog
		gl.uniform4fv(program.fogColour,uniforms.fogColour || [1.0,1.0,1.0,0.0]);
		gl.enableVertexAttribArray(program.texCoord);
		gl.enableVertexAttribArray(program.normal);
		gl.enableVertexAttribArray(program.vertex);
		cb(program);
		gl.disableVertexAttribArray(program.vertex);
		gl.disableVertexAttribArray(program.normal);
		gl.disableVertexAttribArray(program.texCoord);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.bindTexture(gl.TEXTURE_2D,null);
		gl.useProgram(null);
	},
};
