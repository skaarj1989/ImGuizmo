#include <algorithm>
#include <array>
#include <random>
#include <unordered_map>

#include "ImGuizmo.h"

#include "glad/glad.h"
#include "GLFW/glfw3.h"
#include "../backends/imgui_impl_glfw.h"
#include "../backends/imgui_impl_opengl3.h"

#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtx/wrap.hpp"

#include "Geometry.hpp"
#include "Materials.hpp"

#define LOWER_LEFT_ST { 0.0f, 1.0f }, { 1.0f, 0.0f }
#define UPPER_LEFT_UV { 0.0f, 0.0f }, { 1.0f, 1.0f }
#define TEXCOORDS LOWER_LEFT_ST

constexpr auto IdentityMatrix = glm::mat4{ 1.0f };

struct entity_t {
  material_t Material{};
  glm::mat4 ModelMatrix{ 1.0f };
};
entity_t *CurrentEntity{ nullptr };

struct {
  struct {
    GLuint ProjectionMatrix, ViewMatrix;
    GLuint EyePos;
  } Camera;
  GLuint ModelMatrix;

  GLuint Mode;
  GLuint Gamma;

  struct {
    GLuint Ka, Kd, Ks, Shininess;
  } Material;
  struct {
    GLuint Direction, La, Ld, Ls;
  } DirLight;
  struct {
    GLuint Position, Radius, La, Ld, Ls;
  } PointLight;
} ShaderResources;
struct {
  glm::ivec2 Size{ 1, 1 };

  GLuint FBO{ GL_NONE };
  GLuint ColorTexture{ GL_NONE };
  GLuint DepthRBO{ GL_NONE };

  GLuint VAO{ GL_NONE };
  GLuint PlaneVBO{ GL_NONE }, CubeVBO{ GL_NONE };
  GLuint VertexShader{ GL_NONE }, FragmentShader{ GL_NONE };

  struct {
    glm::vec3 Direction{ -0.2f, -1.0f, -0.3f };
    glm::vec3 Ambient{ 0.05f, 0.05f, 0.05f };
    glm::vec3 Diffuse{ 0.4f, 0.4f, 0.4f };
    glm::vec3 Specular{ 0.9f, 0.9f, 0.9f };
  } DirLight;

  struct {
    glm::vec3 Position{ 0.0f, 5.0f, 0.0f };
    float Radius{ 6.0f };
    glm::vec3 Ambient{ 0.05f, 0.05f, 0.05f };
    glm::vec3 Diffuse{ 0.4f, 0.4f, 0.4f };
    glm::vec3 Specular{ 0.9f, 0.9f, 0.9f };
  } PointLight;

  std::vector<entity_t> Entities;

  struct {
    bool IsPerspective{ true };
    glm::mat4 ProjectionMatrix;
    glm::mat4 ViewMatrix;
    glm::vec3 Eye;
  } Camera;

  glm::vec3 BackgroundColor{ 0.45f, 0.55f, 0.60f };
} Scene;

struct {
  ImGuizmoMode Mode{ ImGuizmoMode_Local };
  ImGuizmoOperation Operation{ ImGuizmoOperation_Translate };
  ImGuizmoAxisFlags LockedAxes{ ImGuizmoAxisFlags_None };

  bool UseSnap{ false };
  float Snap[3]{ 1.0f, 1.0f, 1.0f };
} TransformSettings;

void CreateEntities(int num_entities) {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<float> horizontal_dis(-8.0f, 8.0f),
    vertical_dis(1.0f, 3.0f), scale_dis(0.2f, 1.2f);
  std::uniform_int_distribution<int> idx_dis(0, MaterialPresets.size() - 1);

  for (int i = 0; i < num_entities; ++i) {
    auto it = MaterialPresets.begin();
    std::advance(it, idx_dis(gen));

    const glm::vec3 pos{ horizontal_dis(gen), vertical_dis(gen),
                         horizontal_dis(gen) };
    const auto model_matrix =
      glm::translate(IdentityMatrix, pos) *
      glm::scale(IdentityMatrix, glm::vec3{ scale_dis(gen) });

    Scene.Entities.push_back(entity_t{ it->second, model_matrix });
  }
}

void GLAPIENTRY DebugCallback(GLenum source, GLenum type, GLuint id,
                              GLenum severity, GLsizei /*length*/,
                              const GLchar *message,
                              const void * /*userParam*/) {
  static std::unordered_map<GLenum, const char *> sources{
    { GL_DEBUG_SOURCE_API, "API" },
    { GL_DEBUG_SOURCE_WINDOW_SYSTEM, "WINDOW SYSTEM" },
    { GL_DEBUG_SOURCE_SHADER_COMPILER, "SHADER COMPILER" },
    { GL_DEBUG_SOURCE_THIRD_PARTY, "THIRD PARTY" },
    { GL_DEBUG_SOURCE_APPLICATION, "APPLICATION" },
    { GL_DEBUG_SOURCE_OTHER, "OTHER" }
  };
  static std::unordered_map<GLenum, const char *> types{
    { GL_DEBUG_TYPE_ERROR, "ERROR" },
    { GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR, "DEPRECATED BEHAVIOR" },
    { GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR, "UNDEFINED BEHAVIOR" },
    { GL_DEBUG_TYPE_PORTABILITY, "PORTABILITY" },
    { GL_DEBUG_TYPE_PERFORMANCE, "PERFORMANCE" },
    { GL_DEBUG_TYPE_MARKER, "MARKER" },
    { GL_DEBUG_TYPE_OTHER, "OTHER" }
  };
  static std::unordered_map<GLenum, const char *> severities{
    { GL_DEBUG_SEVERITY_HIGH, "HIGH" },
    { GL_DEBUG_SEVERITY_MEDIUM, "MEDIUM" },
    { GL_DEBUG_SEVERITY_LOW, "LOW" },
    { GL_DEBUG_SEVERITY_NOTIFICATION, "NOTIFICATION" }
  };

  fprintf(stderr, "[%s] %d: %s, raised from %s: %s\n", severities[severity], id,
          types[type], sources[source], message);
}
void SetupDebugCallback() {
  glDebugMessageCallback(DebugCallback, nullptr);
  glEnable(GL_DEBUG_OUTPUT);
  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
  glDebugMessageControl(GL_DONT_CARE, GL_DEBUG_TYPE_OTHER, GL_DONT_CARE, 0,
                        nullptr, GL_FALSE);
}

void ResizeFramebuffer(const glm::ivec2 &size) {
  int width = glm::max(size.x, 1);
  int height = glm::max(size.y, 1);

  glDeleteRenderbuffers(1, &Scene.DepthRBO);
  glCreateRenderbuffers(1, &Scene.DepthRBO);
  glNamedRenderbufferStorage(Scene.DepthRBO, GL_DEPTH_COMPONENT24, width,
                             height);

  glDeleteTextures(1, &Scene.ColorTexture);
  glCreateTextures(GL_TEXTURE_2D, 1, &Scene.ColorTexture);
  glTextureStorage2D(Scene.ColorTexture, 1, GL_RGB8, width, height);

  glNamedFramebufferRenderbuffer(Scene.FBO, GL_DEPTH_ATTACHMENT,
                                 GL_RENDERBUFFER, Scene.DepthRBO);
  glNamedFramebufferTexture(Scene.FBO, GL_COLOR_ATTACHMENT0, Scene.ColorTexture,
                            0);

  assert(glCheckNamedFramebufferStatus(Scene.FBO, GL_FRAMEBUFFER) ==
         GL_FRAMEBUFFER_COMPLETE);

  Scene.Size.x = width;
  Scene.Size.y = height;
}

const char *GetShaderInfoLog(GLuint spo) {
  GLint info_log_length;
  glGetProgramiv(spo, GL_INFO_LOG_LENGTH, &info_log_length);
  static std::string info_log;

  info_log.reserve(info_log_length);
  glGetProgramInfoLog(spo, info_log_length, nullptr, info_log.data());

  return info_log.c_str();
}

void SetupGeometry() {
  glCreateBuffers(1, &Scene.PlaneVBO);
  glNamedBufferStorage(Scene.PlaneVBO, sizeof(vertex_t) * 6, PlaneVertices,
                       GL_NONE);
  glCreateBuffers(1, &Scene.CubeVBO);
  glNamedBufferStorage(Scene.CubeVBO, sizeof(vertex_t) * 36, CubeVertices,
                       GL_NONE);

  glCreateVertexArrays(1, &Scene.VAO);
  glEnableVertexArrayAttrib(Scene.VAO, 0);
  glEnableVertexArrayAttrib(Scene.VAO, 1);
  glEnableVertexArrayAttrib(Scene.VAO, 2);
  glVertexArrayAttribFormat(Scene.VAO, 0, 3, GL_FLOAT, GL_FALSE, 0);
  glVertexArrayAttribFormat(Scene.VAO, 1, 3, GL_FLOAT, GL_FALSE,
                            offsetof(vertex_t, normal));
  glVertexArrayAttribFormat(Scene.VAO, 2, 2, GL_FLOAT, GL_FALSE,
                            offsetof(vertex_t, st));
  glVertexArrayAttribBinding(Scene.VAO, 0, 0);
  glVertexArrayAttribBinding(Scene.VAO, 1, 0);
  glVertexArrayAttribBinding(Scene.VAO, 2, 0);

  glBindVertexArray(Scene.VAO);
}
void SetupShaders() {
  auto vs_src{
#include "forward.vert"
  };
  Scene.VertexShader = glCreateShaderProgramv(GL_VERTEX_SHADER, 1, &vs_src);
  static GLint link_status;
  glGetProgramiv(Scene.VertexShader, GL_LINK_STATUS, &link_status);
  if (!link_status) printf("%s\n", GetShaderInfoLog(Scene.VertexShader));

  ShaderResources.Camera.ProjectionMatrix =
    glGetUniformLocation(Scene.VertexShader, "uProjectionMatrix");
  ShaderResources.Camera.ViewMatrix =
    glGetUniformLocation(Scene.VertexShader, "uViewMatrix");
  ShaderResources.ModelMatrix =
    glGetUniformLocation(Scene.VertexShader, "uModelMatrix");

  auto fs_src{
#include "forward.frag"
  };
  Scene.FragmentShader = glCreateShaderProgramv(GL_FRAGMENT_SHADER, 1, &fs_src);
  glGetProgramiv(Scene.FragmentShader, GL_LINK_STATUS, &link_status);
  if (!link_status) printf("%s\n", GetShaderInfoLog(Scene.FragmentShader));

  ShaderResources.Mode = glGetUniformLocation(Scene.FragmentShader, "uMode");
  ShaderResources.Gamma = glGetUniformLocation(Scene.FragmentShader, "uGamma");

  ShaderResources.Camera.EyePos =
    glGetUniformLocation(Scene.FragmentShader, "uEyePos");

  ShaderResources.Material.Ka =
    glGetUniformLocation(Scene.FragmentShader, "uMaterial.Ka");
  ShaderResources.Material.Kd =
    glGetUniformLocation(Scene.FragmentShader, "uMaterial.Kd");
  ShaderResources.Material.Ks =
    glGetUniformLocation(Scene.FragmentShader, "uMaterial.Ks");
  ShaderResources.Material.Shininess =
    glGetUniformLocation(Scene.FragmentShader, "uMaterial.shininess");

  ShaderResources.DirLight.Direction =
    glGetUniformLocation(Scene.FragmentShader, "uDirLight.direction");
  ShaderResources.DirLight.La =
    glGetUniformLocation(Scene.FragmentShader, "uDirLight.base.La");
  ShaderResources.DirLight.Ld =
    glGetUniformLocation(Scene.FragmentShader, "uDirLight.base.Ld");
  ShaderResources.DirLight.Ls =
    glGetUniformLocation(Scene.FragmentShader, "uDirLight.base.Ls");

  ShaderResources.PointLight.Position =
    glGetUniformLocation(Scene.FragmentShader, "uPointLight.position");
  ShaderResources.PointLight.Radius =
    glGetUniformLocation(Scene.FragmentShader, "uPointLight.radius");
  ShaderResources.PointLight.La =
    glGetUniformLocation(Scene.FragmentShader, "uPointLight.base.La");
  ShaderResources.PointLight.Ld =
    glGetUniformLocation(Scene.FragmentShader, "uPointLight.base.Ld");
  ShaderResources.PointLight.Ls =
    glGetUniformLocation(Scene.FragmentShader, "uPointLight.base.Ls");


  static GLuint po;
  glCreateProgramPipelines(1, &po);
  glUseProgramStages(po, GL_VERTEX_SHADER_BIT, Scene.VertexShader);
  glUseProgramStages(po, GL_FRAGMENT_SHADER_BIT, Scene.FragmentShader);

  glBindProgramPipeline(po);
}
void SetupFramebuffer() {
  glCreateTextures(GL_TEXTURE_2D, 1, &Scene.ColorTexture);
  glTextureStorage2D(Scene.ColorTexture, 1, GL_RGBA8, 1, 1);

  glCreateRenderbuffers(1, &Scene.DepthRBO);
  glNamedRenderbufferStorage(Scene.DepthRBO, GL_DEPTH_COMPONENT24, 1, 1);

  glCreateFramebuffers(1, &Scene.FBO);
  glNamedFramebufferRenderbuffer(Scene.FBO, GL_DEPTH_ATTACHMENT,
                                 GL_RENDERBUFFER, Scene.DepthRBO);
  glNamedFramebufferTexture(Scene.FBO, GL_COLOR_ATTACHMENT0, Scene.ColorTexture,
                            0);

  assert(glCheckNamedFramebufferStatus(Scene.FBO, GL_FRAMEBUFFER) ==
         GL_FRAMEBUFFER_COMPLETE);
}

void UploadCamera() {
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.Camera.EyePos, 1,
                      glm::value_ptr(Scene.Camera.Eye));
  glProgramUniformMatrix4fv(Scene.VertexShader,
                            ShaderResources.Camera.ViewMatrix, 1, GL_FALSE,
                            glm::value_ptr(Scene.Camera.ViewMatrix));
  glProgramUniformMatrix4fv(Scene.VertexShader,
                            ShaderResources.Camera.ProjectionMatrix, 1,
                            GL_FALSE, glm::value_ptr(Scene.Camera.ProjectionMatrix));
}
void UploadLights() {
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.DirLight.Direction,
                      1, glm::value_ptr(Scene.DirLight.Direction));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.DirLight.La, 1,
                      glm::value_ptr(Scene.DirLight.Ambient));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.DirLight.Ld, 1,
                      glm::value_ptr(Scene.DirLight.Diffuse));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.DirLight.Ls, 1,
                      glm::value_ptr(Scene.DirLight.Specular));

  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.PointLight.Position, 1,
                      glm::value_ptr(Scene.PointLight.Position));
  glProgramUniform1f(Scene.FragmentShader, ShaderResources.PointLight.Radius,
                     Scene.PointLight.Radius);
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.PointLight.La, 1,
                      glm::value_ptr(Scene.PointLight.Ambient));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.PointLight.Ld, 1,
                      glm::value_ptr(Scene.PointLight.Diffuse));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.PointLight.Ls, 1,
                      glm::value_ptr(Scene.PointLight.Specular));
}
void UploadMaterial(const material_t &material) {
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.Material.Ka, 1,
                      glm::value_ptr(material.Ka));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.Material.Kd, 1,
                      glm::value_ptr(material.Kd));
  glProgramUniform3fv(Scene.FragmentShader, ShaderResources.Material.Ks, 1,
                      glm::value_ptr(material.Ks));
  glProgramUniform1f(Scene.FragmentShader, ShaderResources.Material.Shininess,
                     material.Shininess * 128.0f);
}

void RenderModel(GLuint vbo, GLuint num_vertices,
                 const glm::mat4 &model_matrix) {
  glProgramUniformMatrix4fv(Scene.VertexShader, ShaderResources.ModelMatrix, 1,
                            GL_FALSE, glm::value_ptr(model_matrix));
  glVertexArrayVertexBuffer(Scene.VAO, 0, vbo, 0, sizeof(vertex_t));
  glDrawArrays(GL_TRIANGLES, 0, num_vertices);
}
void RenderPlane(const glm::mat4 &model_matrix = IdentityMatrix) {
  RenderModel(Scene.PlaneVBO, 6, model_matrix);
}
void RenderCube(const glm::mat4 &model_matrix) {
  RenderModel(Scene.CubeVBO, 36, model_matrix);
}

void RenderEntity(const entity_t &entity) {
  UploadMaterial(entity.Material);
  RenderCube(entity.ModelMatrix);
}

void RenderScene() {
  glBindFramebuffer(GL_FRAMEBUFFER, Scene.FBO);
  glViewport(0, 0, Scene.Size.x, Scene.Size.y);

  glClearColor(Scene.BackgroundColor.r, Scene.BackgroundColor.g,
               Scene.BackgroundColor.b, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  UploadCamera();
  UploadLights();

  UploadMaterial(MaterialPresets["WhiteRubber"]);
  RenderPlane();
  for (const auto &entity : Scene.Entities)
    RenderEntity(entity);
}

void EditMaterial(material_t &material) {
  if (ImGui::BeginCombo("Preset", "")) {
    for (const auto &[name, preset] : MaterialPresets) {
      const bool is_selected = false;
      if (ImGui::Selectable(name.c_str(), is_selected)) {
        material = preset;
      }
    }

    ImGui::EndCombo();
  }
  ImGui::ColorEdit3("Ka", glm::value_ptr(material.Ka));
  ImGui::ColorEdit3("Kd", glm::value_ptr(material.Kd));
  ImGui::ColorEdit3("Ks", glm::value_ptr(material.Ks));
}

void EditTransform(glm::mat4 &model_matrix) {
  static float bounds[]{ -0.5f, -0.5f, -0.5f, 0.5f, 0.5f, 0.5f };
  static float bound_snap[]{ 0.1f, 0.1f, 0.1f };
  static bool bound_sizing{ false };
  static bool bound_sizing_snap{ false };
  
  if (ImGui::IsKeyPressed(GLFW_KEY_W))
    TransformSettings.Operation = ImGuizmoOperation_Translate;
  if (ImGui::IsKeyPressed(GLFW_KEY_E))
    TransformSettings.Operation = ImGuizmoOperation_Rotate;
  if (ImGui::IsKeyPressed(GLFW_KEY_R))
    TransformSettings.Operation = ImGuizmoOperation_Scale;

  if (ImGui::RadioButton("Translate", TransformSettings.Operation ==
                                        ImGuizmoOperation_Translate)) {
    TransformSettings.Operation = ImGuizmoOperation_Translate;
  }
  ImGui::SameLine();
  if (ImGui::RadioButton("Rotate", TransformSettings.Operation ==
                                     ImGuizmoOperation_Rotate)) {
    TransformSettings.Operation = ImGuizmoOperation_Rotate;
  }
  ImGui::SameLine();
  if (ImGui::RadioButton("Scale", TransformSettings.Operation ==
                                    ImGuizmoOperation_Scale)) {
    TransformSettings.Operation = ImGuizmoOperation_Scale;
  }

  #if 1
  static float translation[3], rotation[3], scale[3];
  ImGuizmo::DecomposeMatrix(glm::value_ptr(model_matrix),
                            translation, rotation, scale);
  constexpr int input_flags = ImGuiInputTextFlags_EnterReturnsTrue;
  bool input_modified{ false };
  input_modified |= ImGui::InputFloat3("T", translation, "%.3f", input_flags);
  input_modified |= ImGui::InputFloat3("R", rotation, "%.3f", input_flags);
  input_modified |= ImGui::InputFloat3("S", scale, "%.3f", input_flags);
  if (input_modified) {
    ImGuizmo::RecomposeMatrix(translation, rotation, scale,
                              glm::value_ptr(model_matrix));
  }
  #endif

  if (TransformSettings.Operation != ImGuizmoOperation_Scale) {
    if (ImGui::RadioButton("Local",
                           TransformSettings.Mode == ImGuizmoMode_Local)) {
      TransformSettings.Mode = ImGuizmoMode_Local;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("World",
                           TransformSettings.Mode == ImGuizmoMode_World)) {
      TransformSettings.Mode = ImGuizmoMode_World;
    }
  } else {
    TransformSettings.Mode = ImGuizmoMode_Local;
  }

  if (ImGui::IsKeyPressed(GLFW_KEY_S))
    TransformSettings.UseSnap = !TransformSettings.UseSnap;
  ImGui::Checkbox("", &TransformSettings.UseSnap);
  ImGui::SameLine();

  switch (TransformSettings.Operation) {
  case ImGuizmoOperation_Translate:
    ImGui::InputFloat3("Snap", &TransformSettings.Snap[0]);
    break;
  case ImGuizmoOperation_Rotate:
    ImGui::InputFloat("Angle Snap", &TransformSettings.Snap[0]);
    break;
  case ImGuizmoOperation_Scale:
    ImGui::InputFloat("Scale Snap", &TransformSettings.Snap[0]);
    break;
  }

  #if 0
  ImGui::Checkbox("Bound Sizing", &bound_sizing);
  if (bound_sizing) {
    ImGui::PushID(3);
    ImGui::Checkbox("", &bound_sizing_snap);
    ImGui::SameLine();
    ImGui::InputFloat3("Snap", bound_snap);
    ImGui::PopID();
  }
  #endif

  ImGui::Text("Locked axes");
  ImGui::SameLine();
  ImGui::CheckboxFlags("X", &TransformSettings.LockedAxes, ImGuizmoAxisFlags_X);
  ImGui::SameLine();
  ImGui::CheckboxFlags("Y", &TransformSettings.LockedAxes, ImGuizmoAxisFlags_Y);
  ImGui::SameLine();
  ImGui::CheckboxFlags("Z", &TransformSettings.LockedAxes, ImGuizmoAxisFlags_Z);
}

int main(int argc, char *argv[]) {
#ifdef _DEBUG
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
  _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
#endif


  if (!glfwInit()) return 1;
  glfwWindowHint(GLFW_MAXIMIZED, GLFW_TRUE);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 6);

  GLFWwindow *window{ glfwCreateWindow(1280, 720, "ImGuizmo:Editor", nullptr,
                                       nullptr) };
  if (!window) return 1;
  glfwMakeContextCurrent(window);

  if (!gladLoadGL()) {
    fprintf(stderr, "Failed to initialize OpenGL loader!\n");
    return 1;
  }

  //CreateEntities(2);
  Scene.Entities.push_back(
    { MaterialPresets["Turquoise"],
      glm::translate(IdentityMatrix, glm::vec3{ 0.0f, 2.0f, 0.0f }) });
  CurrentEntity = &(*Scene.Entities.begin());

  SetupDebugCallback();
  SetupGeometry();
  SetupShaders();
  SetupFramebuffer();
  
  glEnable(GL_DEPTH_TEST);

  // ---

  IMGUI_CHECKVERSION();
  ImGui::CreateContext();
  ImGuiIO &io{ ImGui::GetIO() };
  io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
  // io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;

  io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;
  io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
  io.ConfigWindowsMoveFromTitleBarOnly = true;

  // io.ConfigViewportsNoAutoMerge = true;
  // io.ConfigViewportsNoTaskBarIcon = true;

  ImGui::StyleColorsDark();
  ImGuizmo::StyleColorsBlender();
  ImGuizmo::GetStyle().GizmoScale = 0.2f;

  // When viewports are enabled we tweak WindowRounding/WindowBg so platform
  // windows can look identical to regular ones.
  ImGuiStyle &style{ ImGui::GetStyle() };
  if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
    style.WindowRounding = 0.0f;
    style.Colors[ImGuiCol_WindowBg].w = 1.0f;
  }

  ImGui_ImplGlfw_InitForOpenGL(window, true);
  ImGui_ImplOpenGL3_Init();

  //glfwSwapInterval(1);

  float fov{ 60.0 };

  float view_width{ 10.0f }; // for ortho projection
  constexpr float cam_angle_y{ glm::radians(235.0f) };
  constexpr float cam_angle_x{ glm::radians(140.0f) };

  bool show_demo_window{ true };

  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();
    static glm::ivec2 backbuffer_size;
    glfwGetFramebufferSize(window, &backbuffer_size.x, &backbuffer_size.y);

    if (Scene.Camera.IsPerspective) {
      float aspect_ratio =
        static_cast<float>(Scene.Size.x) / static_cast<float>(Scene.Size.y);
      Scene.Camera.ProjectionMatrix =
        glm::perspective(glm::radians(fov), aspect_ratio, 0.1f, 1000.0f);
    } else {
      const float view_height{ view_width * static_cast<float>(Scene.Size.y) /
                               static_cast<float>(Scene.Size.x) };
      Scene.Camera.ProjectionMatrix = glm::ortho(
        -view_width, view_width, -view_height,
                                     view_height, -1000.0f, 1000.0f);
    }
    static float cam_distance{ 8.0f };

    static bool first_frame{ true };
    static bool view_dirty{ true };
    if (first_frame || view_dirty) {
      const glm::vec3 at{ 0.0f, 2.0f, 0.0f };
      const glm::vec3 up{ 0.0f, 1.0f, 0.0f };
      const glm::vec3 eye =
        glm::vec3{ glm::cos(cam_angle_y) * glm::cos(cam_angle_x) * cam_distance,
                   glm::sin(cam_angle_x) * cam_distance,
                   glm::sin(cam_angle_y) * glm::cos(cam_angle_x) *
                     cam_distance };
      Scene.Camera.ViewMatrix = glm::lookAt(eye, at, up);
      first_frame = false;
    }
    glm::mat4 inversed_camera_view{ glm::inverse(Scene.Camera.ViewMatrix) };
    glm::vec3 eye_pos = glm::vec3{ inversed_camera_view[2] } * cam_distance;
    glm::vec3 cam_dir = glm::normalize(glm::vec3{ 0.0f } - eye_pos);
    Scene.Camera.Eye = eye_pos;

    RenderScene();

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    if (ImGui::Begin("ImGuizmoContext")) {
      ImGuizmo::PrintContext();
    }
    ImGui::End();

    if (ImGui::Begin("ImGuizmo")) {
      ImGui::Text("ConfigFlags");
      static ImGuizmoConfigFlags config_flags{ 0 };
      ImGui::CheckboxFlags("CloakOnManipulate", &config_flags,
                           ImGuizmoConfigFlags_CloakOnManipulate);
      ImGui::CheckboxFlags("HideLocked", &config_flags,
                           ImGuizmoConfigFlags_HideLocked);
      ImGui::CheckboxFlags("HasReversing", &config_flags,
                           ImGuizmoConfigFlags_HasReversing);
      ImGuizmo::SetConfigFlags(config_flags);

      ImGui::Separator();
      ImGuizmo::ShowStyleEditor();
    }
    ImGui::End();

    if (ImGui::Begin("Renderer")) {
      ImGui::ColorEdit3("Background Color",
                        glm::value_ptr(Scene.BackgroundColor));

      static int mode{ 0 };
      if (ImGui::RadioButton("Phong", mode == 0)) mode = 0;
      ImGui::SameLine();
      if (ImGui::RadioButton("Blinn-Phong", mode == 1)) mode = 1;
      glProgramUniform1i(Scene.FragmentShader, ShaderResources.Mode, mode);

      static float gamma{ 2.2f };
      ImGui::DragFloat("Gamma", &gamma, 0.01f, 0.1f, 5.0f);
      glProgramUniform1f(Scene.FragmentShader, ShaderResources.Gamma, gamma);
    }
    ImGui::End();


    if (show_demo_window) ImGui::ShowDemoWindow(&show_demo_window);

    ImGui::SetNextWindowSize(glm::vec2{ 256 });
    if (ImGui::Begin("View", nullptr, ImGuiWindowFlags_NoResize)) {
      ImGuizmo::ViewManipulate(glm::value_ptr(Scene.Camera.ViewMatrix),
                               cam_distance);
    }
    ImGui::End();

    //ImGui::ShowStyleEditor();
    //ImGuizmo::ShowStyleEditor();

    ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, { 0.0f, 0.0f });
    if (ImGui::Begin("Scene", nullptr, ImGuiWindowFlags_NoScrollbar)) {
      const glm::vec2 content_size{ ImGui::GetContentRegionAvail() };
      if (glm::ivec2{ content_size } != Scene.Size) {
        ResizeFramebuffer(content_size);
      } else {
        ImGui::Image(reinterpret_cast<ImTextureID>(Scene.ColorTexture),
                     glm::vec2{ Scene.Size }, TEXCOORDS);

        ImGuizmo::SetCamera(glm::value_ptr(Scene.Camera.ViewMatrix),
                            glm::value_ptr(Scene.Camera.ProjectionMatrix),
                            !Scene.Camera.IsPerspective);
        if (ImGuizmo::Begin(TransformSettings.Mode,
                            glm::value_ptr(CurrentEntity->ModelMatrix),
                            TransformSettings.LockedAxes)) {
          const float *snap{ TransformSettings.UseSnap ? TransformSettings.Snap
                                                       : nullptr };
         


          switch (TransformSettings.Operation) {
          case ImGuizmoOperation_Translate:
            ImGuizmo::Translate(snap);
            break;
          case ImGuizmoOperation_Rotate:
            ImGuizmo::Rotate(snap);
            break;
          case ImGuizmoOperation_Scale:
            ImGuizmo::Scale(snap);
            break;
          }

          static float bound_snap[]{ 0.1f, 0.1f, 0.1f };
          static float bounds[]{ -1.0f, -1.0f, -1.0f, 1.0f, 1.0f, 1.0f };
          ImGuizmo::BoundsScale(bounds, TransformSettings.UseSnap ? bound_snap
                                                                  : nullptr);
        }
        ImGuizmo::End();
      }
    }
    ImGui::End();
    ImGui::PopStyleVar();

    if (ImGui::Begin("Camera")) {
      if (ImGui::RadioButton("Perspective", Scene.Camera.IsPerspective))
        Scene.Camera.IsPerspective = true;
      ImGui::SameLine();
      if (ImGui::RadioButton("Orthographic", !Scene.Camera.IsPerspective))
        Scene.Camera.IsPerspective = false;
      if (Scene.Camera.IsPerspective) {
        ImGui::SliderFloat("FOV", &fov, 20.0f, 120.f);
      } else {
        ImGui::SliderFloat("Ortho width", &view_width, 1.0f, 20.0f);
      }
      view_dirty = ImGui::InputFloat("Distance", &cam_distance, 1.0f);
      ImGui::Separator();

      ImGui::Text("EyePos = %.2f, %.2f, %.2f", eye_pos.x, eye_pos.y, eye_pos.z);
      ImGui::Text("Direction = %.2f, %.2f, %.2f", cam_dir.x, cam_dir.y,
                  cam_dir.z);
    }
    ImGui::End();

    if (ImGui::Begin("Lights")) {
      ImGui::Text("Direction = %.2f, %.2f, %.2f", Scene.DirLight.Direction.x,
                  Scene.DirLight.Direction.y, Scene.DirLight.Direction.z);
      ImGui::ColorEdit3("La##DirLight", glm::value_ptr(Scene.DirLight.Ambient));
      ImGui::ColorEdit3("Ld##DirLight", glm::value_ptr(Scene.DirLight.Diffuse),
                        ImGuiColorEditFlags_HDR);
      ImGui::ColorEdit3("Ls##DirLight", glm::value_ptr(Scene.DirLight.Specular));

      ImGui::Separator();

      ImGui::DragFloat3("Position", glm::value_ptr(Scene.PointLight.Position), 0.1f);
      ImGui::DragFloat("Radius", &Scene.PointLight.Radius, 0.1f, 0.0f, 50.0f);
      ImGui::ColorEdit3("La##PointLight", glm::value_ptr(Scene.PointLight.Ambient));
      ImGui::ColorEdit3("Ld##PointLight", glm::value_ptr(Scene.PointLight.Diffuse),
                        ImGuiColorEditFlags_HDR);
      ImGui::ColorEdit3("Ls##PointLight", glm::value_ptr(Scene.PointLight.Specular));
    }
    ImGui::End();

    if (ImGui::Begin("Entities")) {
      static int current_entity_id{ 0 };
      ImGui::SliderInt("ID", &current_entity_id, 0, Scene.Entities.size() - 1);
      CurrentEntity = &Scene.Entities[current_entity_id];
      ImGui::Separator();
      EditMaterial(CurrentEntity->Material);
      ImGui::Separator();
      EditTransform(CurrentEntity->ModelMatrix);
    }
    ImGui::End();

    #if 0
    constexpr auto view_size = 256;
    ImGui::SetNextWindowSize(glm::vec2{ view_size });
    if (ImGui::Begin("ViewManipulate", nullptr, ImGuiWindowFlags_NoResize)) {
      glm::vec2 position{ glm::vec2{ ImGui::GetWindowPos() } +
                          glm::vec2{ ImGui::GetWindowContentRegionMin() } };
      //ImGuizmo::ViewManipulate(glm::value_ptr(Scene.Camera.ViewMatrix), cam_distance,
      //                         position, ImGui::GetContentRegionAvail(), 0x10101010);
    }
    ImGui::End();
    #endif


    ImGui::Render();

    static glm::vec3 clear_color{ 0.466f, 0.533f, 0.6f };
    glBindFramebuffer(GL_FRAMEBUFFER, GL_NONE);
    glViewport(0, 0, backbuffer_size.x, backbuffer_size.y);
    glClearColor(clear_color.r, clear_color.g, clear_color.b, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

    if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
      GLFWwindow *backup_context{ glfwGetCurrentContext() };
      ImGui::UpdatePlatformWindows();
      ImGui::RenderPlatformWindowsDefault();
      glfwMakeContextCurrent(backup_context);
    }

    glfwSwapBuffers(window);
  }

  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplGlfw_Shutdown();

  ImGui::DestroyContext();

  glfwDestroyWindow(window);
  glfwTerminate();

  return 0;
}