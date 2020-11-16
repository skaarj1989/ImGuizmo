#define _PROFILE_CODE 0

#include <array>
#include <algorithm>
#include <random>
#if _PROFILE_CODE
#  include <algorithm>
#  include <chrono>
#  include <execution>
#endif

#include "ImGuizmo.h"

#include "glad/glad.h"
#include "GLFW/glfw3.h"
#include "backends/imgui_impl_glfw.h"
#include "backends/imgui_impl_opengl3.h"

#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"

constexpr auto kIdentityMatrix = glm::mat4{ 1.0f };

constexpr auto kMaxNumGizmos = 4;
/*
glm::mat4 modelMatrices[kMaxNumGizmos] = {
  glm::translate(kIdentityMatrix, { 0.0f, 0.0f, 0.0f }),
  glm::translate(kIdentityMatrix, { 2.0f, 0.0f, 0.0f }),
  glm::translate(kIdentityMatrix, { 2.0f, 0.0f, 2.0f }),
  glm::translate(kIdentityMatrix, { 0.0f, 0.0f, 2.0f }),
};
*/
std::array<glm::mat4, 4> modelMatrices{
  glm::translate(kIdentityMatrix, { 0.0f, 0.0f, 0.0f }),
  glm::translate(kIdentityMatrix, { 2.0f, 0.0f, 0.0f }),
  glm::translate(kIdentityMatrix, { 2.0f, 0.0f, 2.0f }),
  glm::translate(kIdentityMatrix, { 0.0f, 0.0f, 2.0f }),
};
static int gizmoCount{ 1 };

void EditTransform(float *model, bool editTransformDecomposition) {
  static auto mode{ ImGuizmoMode_Local };
  static ImGuizmoOperation operation{ ImGuizmoOperation_Translate };

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
      operation = ImGuizmoOperation_Translate;
    if (ImGui::IsKeyPressed(kRotateKey)) operation = ImGuizmoOperation_Rotate;
    if (ImGui::IsKeyPressed(kScaleKey)) operation = ImGuizmoOperation_Scale;

    if (ImGui::RadioButton("Translate",
                           operation == ImGuizmoOperation_Translate))
      operation = ImGuizmoOperation_Translate;
    ImGui::SameLine();
    if (ImGui::RadioButton("Rotate", operation == ImGuizmoOperation_Rotate))
      operation = ImGuizmoOperation_Rotate;
    ImGui::SameLine();
    if (ImGui::RadioButton("Scale", operation == ImGuizmoOperation_Scale))
      operation = ImGuizmoOperation_Scale;
     
    static float translation[3], rotation[3], scale[3];
    ImGuizmo::DecomposeMatrix(model, translation, rotation, scale);
    bool inputModified{ false };
    inputModified |= ImGui::InputFloat3("T", translation);
    inputModified |= ImGui::InputFloat3("R", rotation);
    inputModified |= ImGui::InputFloat3("S", scale);
    if (inputModified)
      ImGuizmo::RecomposeMatrix(translation, rotation, scale, model);
    
    if (operation != ImGuizmoOperation_Scale) {
      if (ImGui::RadioButton("Local", mode == ImGuizmoMode_Local))
        operation = ImGuizmoMode_Local;
      ImGui::SameLine();
      if (ImGui::RadioButton("Global", mode == ImGuizmoMode_Global))
        operation = ImGuizmoMode_Global;
    }

    if (ImGui::IsKeyPressed(kSnapKey)) useSnap = !useSnap;
    ImGui::Checkbox("", &useSnap);
    ImGui::SameLine();

    switch (operation) {
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

#if 1
  for (int i = 0; i < gizmoCount; ++i)
    ImGuizmo::Manipulate(mode, operation, glm::value_ptr(modelMatrices[i]),
                         useSnap ? snap : nullptr);
#else
  
  if (ImGuizmo::Begin("test2", mode, glm::value_ptr(modelMatrices[0]))) {
    //switch (operation) {
    //case ImGuizmoOperation_Translate:
      ImGuizmo::Translate(useSnap ? snap : nullptr);
    //  break;
    //case ImGuizmoOperation_Rotate:
      ImGuizmo::Rotate(useSnap ? snap : nullptr);
    //  break;
    //case ImGuizmoOperation_Scale:
    //  ImGuizmo::Scale(useSnap ? snap : nullptr);
    //  break;
    //}

    // if (boundSizing)
    //  ImGuizmo::Cage(bounds, boundSizingSnap ? boundsSnap : nullptr);
  }
  ImGuizmo::End();
#endif
}

void EditGizmoStyle() {
  ImGuizmoStyle &style{ ImGuizmo::GetStyle() };
  if (ImGui::Begin("ImGuizmo: Style")) {
    ImGui::DragFloat("Alpha", &style.Alpha, 0.01f, 0.01f, 1.0f);
    ImGui::DragFloat("GizmoScale", &style.GizmoScale, 0.01f, 0.01f, 1.0f);
    ImGui::DragFloat("RingThickness", &style.RotationRingThickness, 0.1f, 0.1f,
                     10.0f);

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
  }
  ImGui::End();
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
  glfwWindowHint(GLFW_MAXIMIZED, GLFW_FALSE);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);

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
  io.ConfigWindowsMoveFromTitleBarOnly = true;

  // io.ConfigViewportsNoAutoMerge = true;
  // io.ConfigViewportsNoTaskBarIcon = true;

  ImGui::StyleColorsDark();
  // ImGui::StyleColorsClassic();

  // When viewports are enabled we tweak WindowRounding/WindowBg so platform
  // windows can look identical to regular ones.
  ImGuiStyle &style{ ImGui::GetStyle() };
  if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
    style.WindowRounding = 0.0f;
    style.Colors[ImGuiCol_WindowBg].w = 1.0f;
  }

  ImGui_ImplGlfw_InitForOpenGL(window, true);
  const char *glsl_version{ "#version 130" };
  ImGui_ImplOpenGL3_Init(glsl_version);

  static glm::mat4 cameraView{ 1.0f };
  static glm::mat4 cameraProjection{ 1.0f };



  float viewWidth{ 10.0f }; // for ortho projection
  float camYAngle{ 165.0f / 180.0f * 3.14159f };
  float camXAngle{ 32.0f / 180.0f * 3.14159f };

  float fov{ 60.0 };

  bool showDemoWindow{ true };
  ImVec4 clearColor{ 0.45f, 0.55f, 0.60f, 1.00f };

  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    if (showDemoWindow) ImGui::ShowDemoWindow(&showDemoWindow);

    ImGuizmo::PrintContext();
    EditGizmoStyle();

    static bool isPerspective{ true };
    if (isPerspective) {
      cameraProjection = glm::perspective(
        glm::radians(fov), io.DisplaySize.x / io.DisplaySize.y, 0.1f, 1000.0f);
    } else {
      const float viewHeight{ viewWidth * io.DisplaySize.y / io.DisplaySize.x };
      cameraProjection = glm::ortho(-viewWidth, viewWidth, -viewHeight,
                                    viewHeight, -1000.0f, 1000.0f);
    }
    static float camDistance{ 8.0f };
    static bool viewDirty{ false };
   

    if (ImGui::Begin("Example")) {
      ImGui::Text("Camera");
      if (ImGui::RadioButton("Perspective", isPerspective))
        isPerspective = true;
      ImGui::SameLine();
      if (ImGui::RadioButton("Orthographic", !isPerspective))
        isPerspective = false;
      if (isPerspective) {
        ImGui::SliderFloat("FOV", &fov, 20.0f, 120.f);
      } else {
        ImGui::SliderFloat("Ortho width", &viewWidth, 1, 20);
      }
      viewDirty = ImGui::InputFloat("Distance", &camDistance, 1.0f);
      ImGui::SliderInt("Gizmo count", &gizmoCount, 1, kMaxNumGizmos);
    }
    ImGui::End();

    static bool firstFrame{ true };
    if (viewDirty || firstFrame) {
      const glm::vec3 eye{
        glm::cos(camYAngle) * glm::cos(camXAngle) * camDistance,
        glm::sin(camXAngle) * camDistance,
        glm::sin(camYAngle) * glm::cos(camXAngle) * camDistance
      };
      const glm::vec3 at{ 0.0f, 0.0f, 0.0f };
      const glm::vec3 up{ 0.0f, 1.0f, 0.0f };
      cameraView = glm::lookAt(eye, at, up);

      firstFrame = false;
    }

    const ImGuiViewport *mainViewport{ ImGui::GetMainViewport() };
    ImGui::SetNextWindowPos(mainViewport->Pos);
    ImGui::SetNextWindowSize(mainViewport->Size);
    ImGuizmo::CreateCanvas("ImGuizmo");
    ImGuizmo::SetCamera(glm::value_ptr(cameraView),
                        glm::value_ptr(cameraProjection), !isPerspective);

    
    ImGuizmo::DrawGrid(glm::value_ptr(cameraView),
                       glm::value_ptr(cameraProjection),
                       glm::value_ptr(glm::mat4{ 1.0f }), 10.0f);
    ImGuizmo::DrawCubes(glm::value_ptr(cameraView),
                        glm::value_ptr(cameraProjection),
                        glm::value_ptr(modelMatrices[0]), gizmoCount);

#if _PROFILE_CODE
    auto start = std::chrono::high_resolution_clock::now();
#endif

    ImGui::Begin("Transform");
    //for (int i = 0; i < gizmoCount; ++i) {
      EditTransform(glm::value_ptr(modelMatrices[0]), true);
      ImGui::Separator();
    //}
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

    constexpr auto kViewSize = 256;

    ImGui::SetNextWindowSize(glm::vec2{ kViewSize } + glm::vec2{ 0, 20 });
    if (ImGui::Begin("ViewManipulate", nullptr, ImGuiWindowFlags_NoResize)) {
      // position = ImGui::GetCurrentWindow()->Viewport->Pos;
      // size = ImGui::GetCurrentWindow()->Viewport->Size;
      // glm::vec2{ position.x + size.x - kViewSize, position.y }

      const glm::vec2 position{ glm::vec2{ ImGui::GetWindowPos() } +
                                glm::vec2{ 0, 20 } };
      #if 0
      ImGuizmo::ViewManip(glm::value_ptr(cameraView), camDistance,
                               position, glm::vec2{ kViewSize });
      #else
      ImGuizmo::ViewManipulate(glm::value_ptr(cameraView), camDistance,
                               position, glm::vec2{ kViewSize }, 0x10101010);
      #endif
    }
    ImGui::End();

    ImGui::Render();

    // ---

    glm::ivec2 framebufferSize;
    glfwGetFramebufferSize(window, &framebufferSize.x, &framebufferSize.y);
    glViewport(0, 0, framebufferSize.x, framebufferSize.y);
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