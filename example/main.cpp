#define _PROFILE_CODE 1

#include <array>
#if _PROFILE_CODE
#  include <algorithm>
#  include <chrono>
#  include <execution>
#endif

#include "ImGuizmo.h"
#include "imgui.h"
#include "imgui_internal.h"

#include "glad/glad.h"
#include "GLFW/glfw3.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl3.h"

#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"

constexpr auto kIdentityMatrix = glm::mat4{ 1.0f };

void GetViewport(glm::vec2 &position, glm::vec2 &size) {
#ifdef IMGUI_HAS_VIEWPORT
  position = ImGui::GetMainViewport()->Pos;
  size = ImGui::GetMainViewport()->Size;
#else
  const ImGuiIO &io{ ImGui::GetIO() };
  position = glm::vec2{ 0, 0 };
  size = io.DisplaySize;
#endif
}

void Frustum(float left, float right, float bottom, float top, float znear,
             float zfar, float *m16) {
  float temp, temp2, temp3, temp4;
  temp = 2.0f * znear;
  temp2 = right - left;
  temp3 = top - bottom;
  temp4 = zfar - znear;
  m16[0] = temp / temp2;
  m16[1] = 0.0;
  m16[2] = 0.0;
  m16[3] = 0.0;
  m16[4] = 0.0;
  m16[5] = temp / temp3;
  m16[6] = 0.0;
  m16[7] = 0.0;
  m16[8] = (right + left) / temp2;
  m16[9] = (top + bottom) / temp3;
  m16[10] = (-zfar - znear) / temp4;
  m16[11] = -1.0f;
  m16[12] = 0.0;
  m16[13] = 0.0;
  m16[14] = (-temp * zfar) / temp4;
  m16[15] = 0.0;
}

void Perspective(float fovyInDegrees, float aspectRatio, float znear,
                 float zfar, float *m16) {
  float ymax, xmax;
  ymax = znear * tanf(fovyInDegrees * 3.141592f / 180.0f);
  xmax = ymax * aspectRatio;
  Frustum(-xmax, xmax, -ymax, ymax, znear, zfar, m16);
}

void OrthoGraphic(const float l, float r, float b, const float t, float zn,
                  const float zf, float *m16) {
  m16[0] = 2 / (r - l);
  m16[1] = 0.0f;
  m16[2] = 0.0f;
  m16[3] = 0.0f;
  m16[4] = 0.0f;
  m16[5] = 2 / (t - b);
  m16[6] = 0.0f;
  m16[7] = 0.0f;
  m16[8] = 0.0f;
  m16[9] = 0.0f;
  m16[10] = 1.0f / (zf - zn); // 1.0f / ...
  m16[11] = 0.0f;
  m16[12] = (l + r) / (l - r);
  m16[13] = (t + b) / (b - t);
  m16[14] = zn / (zn - zf);
  m16[15] = 1.0f;
}

void EditTransform(const float *view, float *projection, float *model,
                   bool editTransformDecomposition) {
  static auto currentGizmoOperation{ ImGuizmoOperation_Translate };
  static auto currentGizmoMode{ ImGuizmoMode_Local };

  static bool useSnap{ false };
  static float snap[3]{ 1.0f, 1.0f, 1.0f };
  static float bounds[]{ -0.5f, -0.5f, -0.5f, 0.5f, 0.5f, 0.5f };
  static float boundsSnap[]{ 0.1f, 0.1f, 0.1f };
  static bool boundSizing{ false };
  static bool boundSizingSnap{ false };

  constexpr auto kTranslateKey = GLFW_KEY_Z;
  constexpr auto kRotateKey = GLFW_KEY_E;   
  constexpr auto kScaleKey = GLFW_KEY_R;    
  constexpr auto kSnapKey = GLFW_KEY_S;     

  if (editTransformDecomposition) {
    if (ImGui::IsKeyPressed(kTranslateKey))
      currentGizmoOperation = ImGuizmoOperation_Translate;
    if (ImGui::IsKeyPressed(kRotateKey))
      currentGizmoOperation = ImGuizmoOperation_Rotate;
    if (ImGui::IsKeyPressed(kScaleKey))
      currentGizmoOperation = ImGuizmoOperation_Scale;

    if (ImGui::RadioButton("Translate", currentGizmoOperation ==
                                          ImGuizmoOperation_Translate))
      currentGizmoOperation = ImGuizmoOperation_Translate;
    ImGui::SameLine();
    if (ImGui::RadioButton("Rotate",
                           currentGizmoOperation == ImGuizmoOperation_Rotate))
      currentGizmoOperation = ImGuizmoOperation_Rotate;
    ImGui::SameLine();
    if (ImGui::RadioButton("Scale",
                           currentGizmoOperation == ImGuizmoOperation_Scale))
      currentGizmoOperation = ImGuizmoOperation_Scale;

    float translation[3]{}, rotation[3]{}, scale[3]{};
    ImGuizmo::DecomposeMatrix(model, translation, rotation, scale);
    ImGui::InputFloat3("T", translation);
    ImGui::InputFloat3("R", rotation);
    ImGui::InputFloat3("S", scale);
    ImGuizmo::RecomposeMatrix(translation, rotation, scale, model);

    if (currentGizmoOperation != ImGuizmoOperation_Scale) {
      if (ImGui::RadioButton("Local", currentGizmoMode == ImGuizmoMode_Local))
        currentGizmoMode = ImGuizmoMode_Local;
      ImGui::SameLine();
      if (ImGui::RadioButton("World", currentGizmoMode == ImGuizmoMode_World))
        currentGizmoMode = ImGuizmoMode_World;
    }

    if (ImGui::IsKeyPressed(kSnapKey)) useSnap = !useSnap;
    ImGui::Checkbox("", &useSnap);
    ImGui::SameLine();

    switch (currentGizmoOperation) {
    case ImGuizmoOperation_Translate:
      ImGui::InputFloat3("Snap", &snap[0]);
      break;
    case ImGuizmoOperation_Rotate:
      ImGui::InputFloat("Angle Snap", &snap[0]);
      break;
    case ImGuizmoOperation_Scale:
      ImGui::InputFloat("Scale Snap", &snap[0]);
      break;
    }
    ImGui::Checkbox("Bound Sizing", &boundSizing);
    if (boundSizing) {
      ImGui::PushID(3);
      ImGui::Checkbox("", &boundSizingSnap);
      ImGui::SameLine();
      ImGui::InputFloat3("Snap", boundsSnap);
      ImGui::PopID();
    }
  }

  glm::vec2 position, size;
  GetViewport(position, size);
  ImGuizmo::SetViewport(position, size);
  ImGuizmo::Manipulate(
    view, projection, currentGizmoOperation, currentGizmoMode, model, nullptr,
    useSnap ? &snap[0] : nullptr, boundSizing ? bounds : nullptr,
    boundSizingSnap ? boundsSnap : nullptr);
}

int main(int argc, char *argv[]) {
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
  _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);

#if _PROFILE_CODE
  constexpr auto kNumSamples = 1000;
  std::array<long long, kNumSamples> timeSamples{};
  int currentSampleId{ 0 };
#endif

  if (!glfwInit()) return 1;

  glfwWindowHint(GLFW_MAXIMIZED, GLFW_TRUE);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

  GLFWwindow *window{ glfwCreateWindow(1280, 720, "ImGuizmo example", nullptr,
                                       nullptr) };
  if (window == nullptr) return 1;
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);

  if (!gladLoadGL()) {
    fprintf(stderr, "Failed to initialize OpenGL loader!\n");
    return 1;
  }

  IMGUI_CHECKVERSION();
  ImGui::CreateContext();
  ImGuiIO &io{ ImGui::GetIO() };
  io.ConfigFlags |=
    ImGuiConfigFlags_NavEnableKeyboard; // Enable Keyboard Controls
  // io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad

  io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;   // Enable Docking
  io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable; // Enable Multi-Viewport /

  // io.ConfigViewportsNoAutoMerge = true;
  // io.ConfigViewportsNoTaskBarIcon = true;

  ImGui::StyleColorsDark();
  // ImGui::StyleColorsClassic();

  // When viewports are enabled we tweak WindowRounding/WindowBg so platform
  // windows can look identical to regular ones.
  {
    ImGuiStyle &style{ ImGui::GetStyle() };
    if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
      style.WindowRounding = 0.0f;
      style.Colors[ImGuiCol_WindowBg].w = 1.0f;
    }
  }

  ImGui_ImplGlfw_InitForOpenGL(window, true);

  const char *glsl_version{ "#version 130" };
  ImGui_ImplOpenGL3_Init(glsl_version);

  int lastUsing{ 0 };

  static glm::mat4 cameraView{ 1.0f };
  static glm::mat4 cameraProjection{ 1.0f };

  constexpr auto kMaxNumGizmos = 4;
  glm::mat4 modelMatrices[kMaxNumGizmos] = {
    glm::translate(kIdentityMatrix, { 0.0f, 0.0f, 0.0f }),
    glm::translate(kIdentityMatrix, { 2.0f, 0.0f, 0.0f }),
    glm::translate(kIdentityMatrix, { 2.0f, 0.0f, 2.0f }),
    glm::translate(kIdentityMatrix, { 0.0f, 0.0f, 2.0f }),
  };

  float viewWidth{ 10.0f }; // for ortho projection
  float camYAngle{ 165.0f / 180.0f * 3.14159f };
  float camXAngle{ 32.0f / 180.0f * 3.14159f };

  float fov{ 27.0f };

  bool showDemoWindow{ true };
  ImVec4 clearColor{ 0.45f, 0.55f, 0.60f, 1.00f };

  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    if (showDemoWindow) ImGui::ShowDemoWindow(&showDemoWindow);

    ImGui::Begin("ImGuizmo: Style");
    auto &style{ ImGuizmo::GetStyle() };
    ImGui::DragFloat("Alpha", &style.Alpha, 0.01f, 0.01f, 1.0f);
    ImGui::DragFloat("Gizmo Scale", &style.GizmoScale, 0.01f, 0.01f, 1.0f);
    ImGui::DragFloat("Ring Size", &style.ScreenRingSize, 0.001f, 0.01f, 1.0f);

    ImGui::ColorEdit4("Text", &style.Colors[ImGuizmoCol_Text].x);
    ImGui::ColorEdit4("Text shadow", &style.Colors[ImGuizmoCol_TextShadow].x);
    ImGui::ColorEdit4("Inactive", &style.Colors[ImGuizmoCol_Inactive].x);
    ImGui::ColorEdit4("Selection", &style.Colors[ImGuizmoCol_Selection].x);
    ImGui::ColorEdit4("Axis X", &style.Colors[ImGuizmoCol_AxisX].x);
    ImGui::ColorEdit4("Axis Y", &style.Colors[ImGuizmoCol_AxisY].x);
    ImGui::ColorEdit4("Axis Z", &style.Colors[ImGuizmoCol_AxisZ].x);
    ImGui::ColorEdit4("Plane YZ", &style.Colors[ImGuizmoCol_PlaneYZ].x);
    ImGui::ColorEdit4("Plane ZX", &style.Colors[ImGuizmoCol_PlaneZX].x);
    ImGui::ColorEdit4("Plane XY", &style.Colors[ImGuizmoCol_PlaneXY].x);

    ImGui::End();

    static bool isPerspective{ true };
    if (isPerspective) {
#if 0
      cameraProjection = glm::perspective(
        glm::radians(fov), io.DisplaySize.x / io.DisplaySize.y, 0.1f, 1000.0f);
#else
      Perspective(fov, io.DisplaySize.x / io.DisplaySize.y, 0.1f, 100.f,
                  glm::value_ptr(cameraProjection));
#endif
    } else {
      const float viewHeight{ viewWidth * io.DisplaySize.y / io.DisplaySize.x };
#if 1
      cameraProjection = glm::ortho(-viewWidth, viewWidth, -viewHeight,
                                    viewHeight, -1000.0f, 1000.0f);
#else
      OrthoGraphic(-viewWidth, viewWidth, -viewHeight, viewHeight, 1000.f,
                   -1000.f, glm::value_ptr(cameraProjection));
#endif
    }
    ImGuizmo::SetOrthographic(!isPerspective);

    static bool gizmoEnabled{ true };
    if (ImGui::IsKeyPressed(GLFW_KEY_X)) gizmoEnabled = !gizmoEnabled;
    ImGui::Checkbox("Enabled", &gizmoEnabled);
    ImGuizmo::Enable(gizmoEnabled);

    ImGui::Text("Camera");
    if (ImGui::RadioButton("Perspective", isPerspective)) isPerspective = true;
    ImGui::SameLine();
    if (ImGui::RadioButton("Orthographic", !isPerspective))
      isPerspective = false;
    if (isPerspective) {
      ImGui::SliderFloat("FOV", &fov, 20.0f, 120.f);
    } else {
      ImGui::SliderFloat("Ortho width", &viewWidth, 1, 20);
    }
    static float camDistance{ 8.0f };
    bool viewDirty = ImGui::InputFloat("Distance", &camDistance, 1.0f);
    static int gizmoCount{ 1 };
    ImGui::SliderInt("Gizmo count", &gizmoCount, 1, kMaxNumGizmos);

    static bool firstFrame{ true };
    if (viewDirty || firstFrame) {
      firstFrame = false;

      const glm::vec3 eye{
        glm::cos(camYAngle) * glm::cos(camXAngle) * camDistance,
        glm::sin(camXAngle) * camDistance,
        glm::sin(camYAngle) * glm::cos(camXAngle) * camDistance
      };
      const glm::vec3 at{ 0.0f, 0.0f, 0.0f };
      const glm::vec3 up{ 0.0f, 1.0f, 0.0f };
      cameraView = glm::lookAt(eye, at, up);
    }

    ImGui::Text("X: %f Y: %f", io.MousePos.x, io.MousePos.y);

    ImGuizmo::BeginFrame();
    ImGuizmo::DrawGrid(glm::value_ptr(cameraView),
                       glm::value_ptr(cameraProjection),
                       glm::value_ptr(kIdentityMatrix), 10.0f);

    ImGuizmo::DrawCubes(glm::value_ptr(cameraView),
                        glm::value_ptr(cameraProjection),
                        glm::value_ptr(modelMatrices[0]), gizmoCount);

    ImGui::Begin("Editor");

#if _PROFILE_CODE
    auto start = std::chrono::high_resolution_clock::now();
#endif
    for (int matId = 0; matId < gizmoCount; matId++) {
      ImGuizmo::SetID(matId);

      EditTransform(glm::value_ptr(cameraView),
                    glm::value_ptr(cameraProjection),
                    glm::value_ptr(modelMatrices[matId]), lastUsing == matId);
      if (ImGuizmo::IsUsing()) lastUsing = matId;
    }
    ImGui::End();

#if _PROFILE_CODE
    auto finish = std::chrono::high_resolution_clock::now();
    auto execTime =
      std::chrono::duration_cast<std::chrono::microseconds>(finish - start)
        .count();

    if (currentSampleId < kNumSamples)
      timeSamples[currentSampleId++] = execTime;

    static long long avgTime{ 0 };
    if (avgTime == 0 && currentSampleId == kNumSamples) {
      const auto accumulated = std::reduce(
        std::execution::par, timeSamples.cbegin(), timeSamples.cend());
      avgTime = accumulated / kNumSamples;
    }

    ImGui::Begin("Profiler");
    if (avgTime == 0)
      ImGui::Text("Gathering samples ... %d", currentSampleId);
    else
      ImGui::Text("EditTransform() = ~%ld microseconds", avgTime);
    ImGui::End();
#endif

    constexpr auto kViewSize = 128;

    glm::vec2 position, size;
    GetViewport(position, size);

    ImGuizmo::ViewManipulate(
      glm::value_ptr(cameraView), camDistance,
      glm::vec2{ position.x + size.x - kViewSize, position.y },
      glm::vec2{ kViewSize }, 0x10101010);

    ImGui::Render();

    // ---

    int displayWidth, displayHeight;
    glfwGetFramebufferSize(window, &displayWidth, &displayHeight);
    glViewport(0, 0, displayWidth, displayHeight);
    glClearColor(clearColor.x, clearColor.y, clearColor.z, clearColor.w);
    glClear(GL_COLOR_BUFFER_BIT);
    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

    if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
      GLFWwindow *backupContext{ glfwGetCurrentContext() };
      ImGui::UpdatePlatformWindows();
      ImGui::RenderPlatformWindowsDefault();
      glfwMakeContextCurrent(backupContext);
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