uniform float time;
uniform vec2 resolution;

const float PI = acos(-1.);
const float PI2 = PI * 2.;

const vec3 light_dir = vec3(.577, .577, -.577);

mat2 rotate(float a) {
    float s = sin(a), c = cos(a);
    return mat2(c, s, -s, c);
}

vec2 foldRotate(vec2 p, float s) {
    float a = PI / s - atan(p.y, p.x);
    float n = PI2 / s;
    a = floor(a / n) * n;
    p = rotate(a) * p;
    return p;
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

float distanceFunc(vec3 p) {
    p.yx = foldRotate(p.yx, 6.);
    return menger(p, vec3(0.79, 1.1, 0.47), 2.31);
}

vec3 getNormal(vec3 p) {
    float eps = 0.001;
    return normalize(vec3(
        distanceFunc(p + vec3(eps, 0., 0.)) - distanceFunc(p - vec3(eps, 0., 0.)),
        distanceFunc(p + vec3(0., eps, 0.)) - distanceFunc(p - vec3(0., eps, 0.)),
        distanceFunc(p + vec3(0., 0., eps)) - distanceFunc(p - vec3(0., 0., eps))
    ));
}

void main() {
    vec2 p = (gl_FragCoord.xy * 2.0 -  resolution) / min(resolution.x, resolution.y);

    vec3 cPos = vec3(0., 0., 3.);
    vec3 cDir = normalize(-cPos);
    vec3 cUp = vec3(0., 1., 0.);
    vec3 cSide = normalize(cross(cDir, cUp));
    float targetDepth = 1.0;

    vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth);

    vec3 rPos = cPos;
    float rLen;
    float distance = 0.0;
    for (int i = 0; i < 128; i++) {
        distance = distanceFunc(rPos);
        rLen += distance;
        rPos = cPos + ray * rLen;
    }

    if(distance < 0.001) {
        vec3 normal = -abs(getNormal(rPos));
        float diffuse = dot(normalize(light_dir), normalize(normal));
        float specular = pow(diffuse, 3.);
        gl_FragColor = vec4(diffuse + specular);
    } else {
        discard;
    }
}