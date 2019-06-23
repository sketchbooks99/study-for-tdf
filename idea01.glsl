uniform float time;
uniform vec2 resolution;

const vec3 light_dir = vec3(.57, .57, .57);

float sphere(vec3 p, float size) {
    return length(p) - size;
}

float distanceFunc(vec3 p) {
    return sphere(p, 0.5);
}

float hex(vec2 uv) {
    uv = abs(uv);
    float d = dot(uv, normalize(vec2(1.0, 1.73)));
    d = max(d, uv.x);
    return step(d, 0.2);
}

vec3 getNormal(vec3 p) {
    float eps = 0.001;
    return normalize(vec3(
        distanceFunc(p + vec3(eps, 0.0, 0.0)) - distanceFunc(p + vec3(-eps, 0.0, 0.0)),
        distanceFunc(p + vec3(0.0, eps, 0.0)) - distanceFunc(p + vec3(0.0, -eps, 0.0)),
        distanceFunc(p + vec3(0.0, 0.0, eps)) - distanceFunc(p + vec3(0.0, 0.0, -eps))
    ));
}

void main() {
    vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

    vec3 cPos = vec3(0., 0., 1.);
    vec3 cDir = normalize(-cPos);
    vec3 cUp = vec3(0.0, 1.0, 0.0);
    vec3 cSide = normalize(cross(cDir, cUp));
    float targetDepth = 1.0;

    vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth);

    float rLen = 0.0;
    vec3 rPos = cPos;
    float distance = 0.0;
    for(int i=0; i<99; i++) {
        distance = distanceFunc(rPos);
        rLen += distance;
        rPos = cPos + ray * rLen;
        if(distance < 0.001) break;
    }

    // if(distance < 0.001){
    //     vec3 normal = getNormal(rPos);
    //     gl_FragColor = vec4(normal, 1.0);
    // }
    gl_FragColor = vec4(hex(p));
}