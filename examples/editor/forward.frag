R"(
#version 460

//layout(origin_upper_left) in vec4 gl_FragCoord;
      
layout(location = 0) in vec3 vFragPos;  
layout(location = 1) in vec3 vNormal;  
layout(location = 2) in vec2 vTexCoord;

uniform vec3 uEyePos;

struct DirLight {
  vec3 direction;
  vec3 La;
  vec3 Ld;
  vec3 Ls;
};
uniform DirLight uDirLight;

struct Material {
  vec3 Ka;
  vec3 Kd;
  vec3 Ks;
  float shininess;
};
uniform Material uMaterial;
      
layout(location = 0) out vec4 FragColor;
void main() {
  vec3 v = normalize(uEyePos - vFragPos);
  vec3 n = normalize(vNormal);
  vec3 s = normalize(-uDirLight.direction);
  float s_dot_n = max(dot(s, n), 0.0);

  float spec = 0.0; 
  if (s_dot_n > 0.0) {
    vec3 h = normalize(s + v);
    float spec_angle = max(dot(h, n), 0.0);
    spec = pow(spec_angle, uMaterial.shininess);
  }    

  vec3 ambient = uDirLight.La * uMaterial.Ka;
  vec3 diffuse = uDirLight.Ld * s_dot_n * uMaterial.Kd;
  vec3 specular = uDirLight.Ls * spec * uMaterial.Ks;

  vec3 result = ambient + diffuse + specular;
  FragColor = vec4(result, 1.0); 
}
)"