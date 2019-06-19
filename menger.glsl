uniform float time;
uniform vec2 resolution;
uniform vec2 mouse;

#define PI 3.1415926
#define TWO_PI PI * 2.0

struct Object {
    float dist;
    vec3 color;
    vec3 edge;
};

vec3 permute(vec3 x) { return mod(((x*34.0)+1.0)*x, 289.0); }

float snoise(vec2 v){
    const vec4 C = vec4(0.211324865405187, 0.366025403784439,
            -0.577350269189626, 0.024390243902439);
    vec2 i  = floor(v + dot(v, C.yy) );
    vec2 x0 = v -   i + dot(i, C.xx);
    vec2 i1;
    i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    vec4 x12 = x0.xyxy + C.xxzz;
    x12.xy -= i1;
    i = mod(i, 289.0);
    vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
    + i.x + vec3(0.0, i1.x, 1.0 ));
    vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy),
        dot(x12.zw,x12.zw)), 0.0);
    m = m*m ;
    m = m*m ;
    vec3 x = 2.0 * fract(p * C.www) - 1.0;
    vec3 h = abs(x) - 0.5;
    vec3 ox = floor(x + 0.5);
    vec3 a0 = x - ox;
    m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
    vec3 g;
    g.x  = a0.x  * x0.x  + h.x  * x0.y;
    g.yz = a0.yz * x12.xz + h.yz * x12.yw;
    return 130.0 * dot(m, g);
}

vec3 lightDir = normalize(vec3(0.577, 0.577, .577));
vec3 repeat(vec3 p, float interval) {
    return mod(p, interval) - interval * 0.5;
}

vec3 fold(vec3 p, int mode) {
    if(mode == 0) p.x = abs(p.x);
    else if(mode == 1) p.y = abs(p.y);
    else if(mode == 2) p.z = abs(p.z);
    return p;
}

vec3 rotate(vec3 p, float angle, vec3 axis){
    vec3 a = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float r = 1.0 - c;
    mat3 m = mat3(
        a.x * a.x * r + c,
        a.y * a.x * r + a.z * s,
        a.z * a.x * r - a.y * s,
        a.x * a.y * r - a.z * s,
        a.y * a.y * r + c,
        a.z * a.y * r + a.x * s,
        a.x * a.z * r + a.y * s,
        a.y * a.z * r - a.x * s,
        a.z * a.z * r + c
    );
    return m * p;
}

vec3 twist(vec3 p, float power) {
    float s = sin(power * p.y);
    float c = cos(power * p.y);
    mat3 m = mat3(
        c, 0.0, -s,
        0.0, 1.0, 0.0,
        s, 0.0, c
    );
    return m * p;
}

mat2 rotate(float a) {
    float s = sin(a), c = cos(a);
    return mat2(c, s, -s, -c);
}

vec2 foldRotate(vec2 p, float s) {
    float a = PI / s - atan(p.y, p.x);
    float n = TWO_PI / s;
    a = float(a / n) * n;
    p *= rotate(a);
    return p;
}

float box(vec3 p, float size) {
    vec3 q = abs(p);
    return length(max(q - vec3(size), 0.0)) - size * 0.15;
}

float maxcomp(vec3 p) {
    float m1 = max(p.x, p.y);
    return max(m1, p.z);
}


vec2 obj_floor(in vec3 p)
{
  return vec2(p.y+10.0,0);
}

vec2 obj_box_s(vec3 p, vec3 b) {
    vec3 di = abs(p) - b;
    float mc = maxcomp(di);
    float d = min(mc, length(max(di, 0.0)));
    return vec2(d, 1.);
}

vec2 obj_box(vec3 p) {
    vec3 b = vec3(4., 4., 4.);
    return obj_box_s(p, b);
}

vec2 obj_cross(in vec3 p) {
    float inf = 100.;
    vec2 da = obj_box_s(p.xyz, vec3(inf, 2.0, 2.0));
    vec2 db = obj_box_s(p.xyz, vec3(2.0, inf, 2.0));
    vec2 dc = obj_box_s(p.xyz, vec3(2.0, 2.0, inf));
    return vec2(min(da.x, min(db.x, dc.x)), 1.);
}

vec2 obj_menger_simple(in vec3 p) {
    vec2 d1 = obj_box(p);
    vec2 d2 = obj_cross(p / 3.);
    float d = max(d1.x, -d2.x);
    return vec2(d, 1.0);
}

vec2 obj_menger(in vec3 p) {
    vec2 d2 = obj_box(p);
    float s = 1.2;
    for(int m=0; m<4; m++) {
        vec3 a = mod(p*s, 2.) - 1.;
        s *= 3.;
        vec3 r = 1. - 4. * abs(a);
        vec2 c = obj_cross(r)/s;
        d2.x = max(d2.x, c.x);
    }

    return d2;
}

float menger(vec3 z0, vec3 offset, float scale) {
    vec4 z = vec4(z0, 1.0);
    for(int i = 0; i < 4; i++) {
        z = abs(z);

        if(z.x < z.y) z.xy = z.yx;
        if(z.x < z.z) z.xz = z.zx;
        if(z.y < z.z) z.yz = z.zy;

        z *= scale;
        z.xyz -= offset * (scale - 1.0);

        if(z.z < -0.5 * offset.z * (scale - 1.0))
            z.z += offset.z * (scale - 1.0);
    }
    return (length(max(abs(z.xyz) - 
        vec3(1.0, 1.0, 1.0), 0.0)) - 0.05) / z.w;
}

// normalized blinn-phong shading
vec3 blinn_phong(vec3 normal, vec3 light_dir, vec3 cam_dir, vec3 col) {
    float m = 30.0;
    float norm_factor = (m + 2.0) / (TWO_PI);
	
    vec3 ambient = col * 0.1;
    float diffuse = dot(normalize(normal), normalize(light_dir)) * 1.5;
    vec3 halfLE = normalize(light_dir + cam_dir);
    float specular = norm_factor * pow(max(0.0, dot(normal, halfLE)), m);

    vec3 out_col = ambient + diffuse * col + specular * 0.8;
    return out_col;
}

// Bechman分布
float BechmanDistribution(float d, float m) {
    float d2 = d * d;
    float m2 = m * m;
    return exp((d2 - 1.0) / (d2 * m2)) / (m2 * d2 * d2);
}

float Fresnel(float c, float f0) {
    float sf = sqrt(f0);
    float n = (1.0 + sf) / (1.0 - sf);
    float g = sqrt(n * n + c * c - 1.0);
    float ga = (c * (g + c) - 1.0) * (c * (g + c) - 1.0);
    float gb = (c * (g - c) + 1.0) * (c * (g - c) + 1.0);
    return (g - c) * (g - c) / (2.0 * (g + c) + (g + c)) * (1.0 + ga / gb);
}

vec3 cook_torrance(vec3 normal, vec3 light_dir, vec3 cam_dir, vec3 col) {
    vec3 specular_coh = vec3(1.0);
    vec3 diffuse_coh = col;
    float microfacet = 0.01; // 面の粗さ

    vec3 l = normalize(light_dir);
    vec3 n = normalize(normal);

    vec3 v = normalize(cam_dir);
    vec3 h = normalize(l + v);

    float hn = dot(h, n);
    float ln = dot(l, n);
    float lh = dot(l, h);
    float vn = dot(v, n);

    vec3 f = vec3(Fresnel(lh, specular_coh.x), Fresnel(lh, specular_coh.y), Fresnel(lh, specular_coh.z));
    float d = BechmanDistribution(hn, microfacet);
    float t = 2.0 * hn / dot(v, h);
    float g = min(1.0, min(t * vn, t * ln));
    float m = PI * vn * ln;
    vec3 spe = max(f * d * g / m, 0.0);
    vec3 dif = max(ln, 0.0) * col;
    vec3 amb = col * .3;

    return amb + dif + spe;
}

Object distanceFunc(vec3 p) {
    float bx = box(rotate(p, time, vec3(1., 0.75, .53)), .05);
    vec3 q = p;
    // q.xy = foldRotate(q.xy, 5.0);
    q -= vec3(0.0, 0.0, time * 0.8);
    q = repeat(q, 5.);
    float gam_menger = menger(q * .3, vec3(.79, 1.1, .47), 2.31);
    q.yx = foldRotate(q.yx, 2.);
    float dist = max(gam_menger,obj_menger(q).x);
    float d = length(q - vec3(0.0, 0.0, q.z));
    vec3 bx_col = vec3(1., 0., 0.);
    Object obj;
    obj.dist = min(bx, dist);
    vec3 col = vec3(.05);// + mod(q.z * 0.2 + time * 1.5, 2.0) * 0.6;
    vec3 out_col = col;
    // vec3 out_col = col;
    obj.color = bx < dist ? bx_col : out_col;
    obj.edge = bx < dist ? bx_col : vec3(.3, 1.5, 1.5);
    // obj.color = col;
    return obj;
}

vec3 getNormal(vec3 p) {
    float d = 0.001;
    return normalize(vec3(
        distanceFunc(p + vec3(d, 0.0, 0.0)).dist - distanceFunc(p + vec3(-d, 0.0, 0.0)).dist,
        distanceFunc(p + vec3(0.0, d, 0.0)).dist - distanceFunc(p + vec3(0.0, -d, 0.0)).dist,
        distanceFunc(p + vec3(0.0, 0.0, d)).dist - distanceFunc(p + vec3(0.0, 0.0, -d)).dist
    ));
}

void main() {
    vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

    float t = mod(time, 6.0);
    // vec3 cPos = vec3(sin(time * .5) * .5, .1, cos(time * .5) * .5);
    vec3 cPos = vec3(0., 0., .3);
    vec3 cDir = normalize(-cPos);
    vec3 cUp = vec3(0.0, 1.0, 0.0);
    vec3 cSide = cross(cDir, cUp);
    float targetDepth = .3;
    
    vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth);

    float rLen = 0.0;
    vec3 rPos = cPos;
    Object obj;
    obj.dist = 0.0;
    for(int i=0; i<128; i++) {
        obj = distanceFunc(rPos);
        rLen += obj.dist;
        rPos = cPos + ray * rLen;
    }
    
    if(obj.dist < 0.001) {
        vec3 normal = getNormal(rPos);
        float eps = 0.001;
        // float eps = 0.001;
        vec3 dif_x = getNormal(rPos+ vec3(eps, 0.0, 0.0));
        vec3 dif_y = getNormal(rPos + vec3(0.0, eps, 0.0));
        vec3 dif_z = getNormal(rPos + vec3(0.0, 0.0, eps));

        float max_dif = .99;
        if(abs(dot(normal, dif_x)) < max_dif ||
            abs(dot(normal, dif_y)) < max_dif || 
            abs(dot(normal, dif_z)) < max_dif) {
                // obj.color = vec3(abs(dot(normal, dif_x)), abs(dot(normal, dif_y)), abs(dot(normal, dif_z)));
                obj.color = obj.edge;
            }
        vec3 shaded_col = blinn_phong(abs(normal), lightDir, -cDir, obj.color);
        gl_FragColor = vec4(shaded_col, 1.0);
        // gl_FragColor = vec4(rPos, 1.0);
    } else {
        gl_FragColor = vec4(.5);
    }
}