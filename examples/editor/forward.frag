R"(
#version 460

//layout(origin_upper_left) in vec4 gl_FragCoord;
      
layout(location = 0) in vec3 vFragPos;  
layout(location = 1) in vec3 vNormal;  
layout(location = 2) in vec2 vTexCoord;

struct Material {
  vec3 Ka;
  vec3 Kd;
  vec3 Ks;
  float shininess;
};

struct LightBase {
  vec3 La;
  vec3 Ld;
  vec3 Ls;
};

struct DirLight {
  vec3 direction;
  LightBase base;
};

struct PointLight {
  vec3 position;
  float radius;
  LightBase base;
};

// 0 = Phong, 1 = Blinn-Phong
uniform int uMode = 0;

uniform float uGamma = 2.2;

uniform vec3 uEyePos;
uniform Material uMaterial;
uniform DirLight uDirLight;
uniform PointLight uPointLight;

// v = view direction
// s = direction TO light
// n = normal to surface

// d = distance to fragment (from light)
// r = radius
float Attenuate(float d, float r) {
  return 1.0 - smoothstep(r * 0.75, r, d); 
}

float Phong(vec3 v, vec3 s, vec3 n) {
  vec3 r = reflect(-s, n);
  float spec_angle = max(dot(r, v), 0.0);
  return pow(spec_angle, uMaterial.shininess / 4.0);
}

float BlinnPhong(vec3 v, vec3 s, vec3 n) {
  vec3 h = normalize(s + v);
  float spec_angle = max(dot(h, n), 0.0);
  return pow(spec_angle, uMaterial.shininess);
}

vec3 CalcLightApprox(vec3 v, LightBase light, vec3 s, vec3 n) {
  vec3 ambient = uMaterial.Ka * light.La;
  vec3 diffuse = vec3(0.0);
  vec3 specular = vec3(0.0);

  float lambertian = max(dot(s, n), 0.0);
  if (lambertian > 0.0) {
    diffuse = (uMaterial.Kd * lambertian) * light.Ld;
    specular = (uMaterial.Ks * uMode == 0 ? Phong(v, s, n) : BlinnPhong(v, s, n)) * light.Ls;
  }

  return ambient + diffuse + specular;
}

vec3 CalcDirLight(vec3 v, DirLight light, vec3 n) {
  vec3 s = normalize(-light.direction);
  return CalcLightApprox(v, light.base, s, n);
}

vec3 CalcPointLight(vec3 v, PointLight light, vec3 n) {
  vec3 s = light.position - vFragPos;
  float d = length(s);
  if (d > light.radius) return vec3(0.0);

  s = normalize(s);
  return CalcLightApprox(v, light.base, s, n) * Attenuate(d, light.radius);
}

layout(location = 0) out vec4 FragColor;
void main() {
  vec3 v = normalize(uEyePos - vFragPos);
  vec3 n = normalize(vNormal);

  vec3 linear_color = CalcDirLight(v, uDirLight, n);
  linear_color += CalcPointLight(v, uPointLight, n);
  vec3 gamma_corrected = pow(linear_color, vec3(1.0 / uGamma));
  
  FragColor = vec4(gamma_corrected, 1.0); 
}
)"