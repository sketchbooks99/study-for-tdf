uniform float time;
uniform vec2 resolution;
uniform vec2 mouse;

#define PI 3.1415926
#define TWO_PI PI * 2.0

const vec3 lightDir = normalize(vec3(0.1, 0.577, -0.577));
vec3 repeat(vec3 p, float interval) {
    return mod(p, interval) - interval * 0.5;
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

float distanceFunc(vec3 p) {
    float sp = sphere(twist(p, 1.0), 3.0);
    float bx = box(p, 2.0);
    p = twist(p, 1.0);
    p.xy = foldRotate(p.xy, 10.0);
    p -= vec3(0.0, 0.0, time * 2.0);
    float back_scene = max(three_bar(repeat(p, 1.0)), -three_tube(repeat(p, 0.1)));
    return max(sp, back_scene);
}

vec3 getNormal(vec3 p) {
    float d = 0.001;
    return normalize(vec3(
        distanceFunc(p + vec3(d, 0.0, 0.0)) - distanceFunc(p + vec3(-d, 0.0, 0.0)),
        distanceFunc(p + vec3(0.0, d, 0.0)) - distanceFunc(p + vec3(0.0, -d, 0.0)),
        distanceFunc(p + vec3(0.0, 0.0, d)) - distanceFunc(p + vec3(0.0, 0.0, -d))
    ));
}

void main() {
    vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

    vec2 m = mouse * 2.0 - 1.0;
    // camera
    vec3 cPos = vec3(sin(time * 0.3) * 6.0, 2.0, cos(time* 0.3) * 6.0);
    // vec3 cDir = vec3(0.0, 0.0, -1.0);
    vec3 cDir = normalize(-cPos);
    vec3 cUp = vec3(0.0, 1.0, 0.0);
    vec3 cSide = cross(cDir, cUp);
    float targetDepth = 1.0;

    // ray
    vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth);

    // marching loop
    float distance = 0.0;
    float rLen = 0.0;
    vec3 rPos = cPos;
    for(int i = 0; i < 64; i++) {
        distance = distanceFunc(rPos);
        rLen += distance;
        rPos = cPos + ray * rLen;
    }

    vec4 out_color = vec4(0.0);

    // hit check
    if(abs(distance) < 0.001) {
        vec3 normal = getNormal(rPos);
        float diff = clamp(dot(lightDir, normal), 0.1, 1.0);
        float spec = pow(diff, 5.0);
        gl_FragColor = vec4(vec3(diff) * vec3(0.2, 0.1, 0.2) + vec3(spec), 1.0);

    } else {
        float len = 0.3  / length(p);
        gl_FragColor = vec4(vec3(len) * vec3(0.5, 0.5, 0.2), 1.0);
    }
}