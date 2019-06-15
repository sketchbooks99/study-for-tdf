

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

float box(vec3 p, float size) {
    vec3 q = abs(p);
    return length(max(q - vec3(size), 0.0));
}

float tube_dist(vec2 p, float width) {
    return length(p) - width;
}

float bar_dist(vec2 p, float width) {
    return length(max(abs(p) - width, 0.0));
}

float three_tube(vec3 p) {
    float tube_x = tube_dist(p.yz, 0.025);
    float tube_y = tube_dist(p.xz, 0.025);
    float tube_z = tube_dist(p.xy, 0.025);

    return min(min(tube_x, tube_y), tube_z);
}

float three_bar(vec3 p) {
    float bar_x = bar_dist(p.yz, 0.1);
    float bar_y = bar_dist(p.xz, 0.1);
    float bar_z = bar_dist(p.xy, 0.1);
    
    return min(min(bar_x, bar_y), bar_z);
}

float sphere(vec3 p, float size) {
    return length(p) - size;
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
    float sp = sphere(p, .3);
    vec3 bx_p = rotate(p, time, vec3(0.1, 0.3, 0.7));
    float bx = box(bx_p, 0.2);
    vec3 q = p;
    // q = twist(p, 0.1);
    // q = rotate(q, time * 0.3, vec3(0.0, 0.0, 1.0));
    q.yx = foldRotate(q.yx, 20.0);
    q -= vec3(0.0, 0.0, time * 3.0);
    float back_scene = max(three_bar(repeat(q, 1.5)), -three_tube(repeat(q, 0.3)));
    Object obj;
    obj.dist = min(bx, back_scene);
    float d = length(q - vec3(0.0, 0.0, q.z));
    vec3 back_col = d  * vec3(.3);
    vec3 bx_col = vec3(0.3,.6, 0.2);
    obj.color = bx < back_scene ? bx_col : back_col;
    // obj.color -= wave;
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

    vec2 m = mouse * 2.0 - 1.0;
    // camera
    vec3 cPos = vec3(0.0, 0.0, 2.0);
    // vec3 cDir = vec3(0.0, 0.0, -1.0);
    vec3 cDir = normalize(-cPos);
    vec3 cUp = vec3(0.0, 1.0, 0.0);
    vec3 cSide = cross(cDir, cUp);
    float targetDepth = 1.0;

    // ray
    vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth);

    // marching loop
    Object obj;
    float rLen = 0.0;
    vec3 rPos = cPos;
    for(int i = 0; i < 64; i++) {
        obj = distanceFunc(rPos);
        rLen += obj.dist;
        rPos = cPos + ray * rLen;
    }

    // hit check
    if(abs(obj.dist) < 0.001) {
        vec3 normal = getNormal(rPos);
        vec3 shaded_col = blinn_phong(abs(normal), lightDir, -cDir, obj.color);
        gl_FragColor = vec4(shaded_col, 1.0);
        // gl_FragColor = vec4(obj.coabr, 1.0);

    } else {
        gl_FragColor = vec4(0.0);
    }
}