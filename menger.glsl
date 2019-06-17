uniform float time;
uniform vec2 resolution;
uniform vec2 mouse;

#define PI 3.1415926
#define TWO_PI PI * 2.0

struct Object {
    float dist;
    vec3 color;
};

const vec3 lightDir = normalize(vec3(0.577, 0.577, .577));
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
    float a = PI / s - atan(p.x, p.y);
    float n = TWO_PI / s;
    a = float(a / n) * n;
    p *= rotate(a);
    return p;
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
    float s = 1.0;
    for(int m=0; m<3; m++) {
        vec3 a = mod(p*s, 2.0) - 1.0;
        s *= 3.;
        vec3 r = 1. - 4. * abs(a);
        vec2 c = obj_cross(r)/s;
        d2.x = max(d2.x, c.x);
    }

    return d2;
}

// normalized blinn-phong shading
vec3 blinn_phong(vec3 normal, vec3 light_dir, vec3 cam_dir, vec3 col) {
    float m = 150.0;
    float norm_factor = (m + 2.0) / (TWO_PI);
	
    vec3 ambient = col * 0.1;
    float diffuse = dot(normalize(normal), normalize(light_dir)) * 1.5;
    vec3 halfLE = normalize(light_dir + cam_dir);
    float specular = norm_factor * pow(max(0.0, dot(normal, halfLE)), m);

    vec3 out_col = ambient + diffuse * col + specular;
    return out_col;
}

Object distanceFunc(vec3 p) {
    vec3 q = p;
    // q.xy = foldRotate(q.xy, 5.0);
    q += vec3(0.0, 0.0, -time);
    q = repeat(q, 3.);
    // q.xy = foldRotate(q.xy, 6.);
    float dist = obj_menger(q).x;
    vec3 col = vec3(1., 0., 0.);
    Object obj;
    obj.dist = dist;
    float d = length(q - vec3(0.0, 0.0, q.z));
    obj.color = col * d * .5 + mod(-q.z * 2.0, 2.0) * 0.2;
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

    vec3 cPos = vec3(0.0, 0.0, 10.0);
    vec3 cDir = normalize(-cPos);
    vec3 cUp = vec3(0.0, 1.0, 0.0);
    vec3 cSide = cross(cDir, cUp);
    float targetDepth = 1.0;
    
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
        vec3 shaded_col = blinn_phong(abs(normal), lightDir, -cDir, obj.color);
        gl_FragColor = vec4(shaded_col, 1.0);
    } else {
        discard;
    }
}