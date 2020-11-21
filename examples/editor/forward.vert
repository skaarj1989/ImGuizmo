R"(
#version 460

layout(location = 0) in vec3 aPosition;
layout(location = 1) in vec3 aNormal;
layout(location = 2) in vec2 aTexCoord;

uniform mat4 uProjectionMatrix;
uniform mat4 uViewMatrix;
uniform mat4 uModelMatrix;

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
	float gl_ClipDistance[];
};
layout(location = 0) out vec3 vFragPos;
layout(location = 1) out vec3 vNormal;
layout(location = 2) out vec2 vTexCoord;
void main() {
	vFragPos = vec3(uModelMatrix * vec4(aPosition, 1.0));
	vNormal = mat3(transpose(inverse(uModelMatrix))) * aNormal;  
  vTexCoord = aTexCoord;
  
	gl_Position = uProjectionMatrix * uViewMatrix * vec4(vFragPos, 1.0);
}
)"