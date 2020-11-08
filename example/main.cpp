#include "imgui.h"
#define IMAPP_IMPL
#include "ImApp.h"
#include "ImGuizmo.h"
#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"

#include <array>
#include <chrono>
#include <algorithm>
#include <execution>

#define PROFILE_CODE 1

constexpr auto kIdentityMatrix = glm::mat4{ 1.0f };

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

  /*
  Result[0][0] = static_cast<T>(2) / (right - left);
  Result[1][1] = static_cast<T>(2) / (top - bottom);
  Result[2][2] = -static_cast<T>(2) / (zFar - zNear);
  Result[3][0] = -(right + left) / (right - left);
  Result[3][1] = -(top + bottom) / (top - bottom);
  Result[3][2] = -(zFar + zNear) / (zFar - zNear);
  */

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

  constexpr auto kTranslateKey = 90; // 'z'
  constexpr auto kRotateKey = 69; // 'e'
  constexpr auto kScaleKey = 82; // 'r'
  constexpr auto kSnapKey = 83; // 's'

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

    float translation[3], rotation[3], scale[3];
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
  const ImGuiIO &io{ ImGui::GetIO() };
  ImGuizmo::SetViewport(0, 0, io.DisplaySize.x, io.DisplaySize.y);
  ImGuizmo::Manipulate(
    view, projection, currentGizmoOperation, currentGizmoMode, model, nullptr,
    useSnap ? &snap[0] : nullptr, boundSizing ? bounds : nullptr,
    boundSizingSnap ? boundsSnap : nullptr);
}

int main(int argc, char *argv[]) {
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
  _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);

  ImApp::ImApp imApp;
  ImApp::Config config;
  config.mWidth = 1280;
  config.mHeight = 720;
  // config.mFullscreen = true;
  imApp.Init(config);

  int lastUsing = 0;

  glm::mat4 modelMatrices[] = {
    glm::translate(kIdentityMatrix, glm::vec3{ 0.0f, 0.0f, 0.0f }),
    glm::translate(kIdentityMatrix, glm::vec3{ 2.0f, 0.0f, 0.0f }),
    glm::translate(kIdentityMatrix, glm::vec3{ 2.0f, 0.0f, 2.0f }),
    glm::translate(kIdentityMatrix, glm::vec3{ 0.0f, 0.0f, 2.0f })
  };

  glm::mat4 cameraView{ 1.0f };
  glm::mat4 cameraProjection{ 1.0f };

  // Camera projection
  bool firstFrame = true;
  bool isPerspective = true;
  float viewWidth = 10.f; // for orthographic
  float camYAngle = 165.f / 180.f * 3.14159f;
  float camXAngle = 32.f / 180.f * 3.14159f;
  float camDistance = 8.f;
  int gizmoCount = 1;

  float fov = 27.0f;

#if PROFILE_CODE
  constexpr auto kNumSamples = 1000;
  std::array<long long, kNumSamples> timeSamples{};
  int currentSampleId{ 0 };
#endif

  while (!imApp.Done()) {
    imApp.NewFrame();
    ImGui::ShowDemoWindow();

    const ImGuiIO &io = ImGui::GetIO();
    if (isPerspective) {
#if 0
      cameraProjection = glm::perspective(
        glm::radians(fov), io.DisplaySize.x / io.DisplaySize.y, 0.1f, 100.0f);
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
    if (ImGui::Checkbox("Enabled", &gizmoEnabled))
      ImGuizmo::Enable(gizmoEnabled);

    ImGui::Text("Camera");
    bool viewDirty = false;
    if (ImGui::RadioButton("Perspective", isPerspective)) isPerspective = true;
    ImGui::SameLine();
    if (ImGui::RadioButton("Orthographic", !isPerspective))
      isPerspective = false;
    if (isPerspective) {
      ImGui::SliderFloat("Fov", &fov, 20.f, 110.f);
    } else {
      ImGui::SliderFloat("Ortho width", &viewWidth, 1, 20);
    }
    viewDirty |= ImGui::SliderFloat("Distance", &camDistance, 1.f, 10.f);
    ImGui::SliderInt("Gizmo count", &gizmoCount, 1, 4);

    if (viewDirty || firstFrame) {
      glm::vec3 eye{ glm::cos(camYAngle) * glm::cos(camXAngle) * camDistance,
                     glm::sin(camXAngle) * camDistance,
                     glm::sin(camYAngle) * glm::cos(camXAngle) * camDistance };
      glm::vec3 at{ 0.0f, 0.0f, 0.0f };
      glm::vec3 up{ 0.0f, 1.0f, 0.0f };
      cameraView = glm::lookAt(eye, at, up);
      firstFrame = false;
    }

    ImGui::Text("X: %f Y: %f", io.MousePos.x, io.MousePos.y);

    ImGui::Separator();

    ImGuizmo::BeginFrame();

    // ImGui::SetNextWindowPos(ImVec2(10, 10));
    ImGui::SetNextWindowSize(ImVec2(320, 340));
    ImGui::Begin("Editor");

    ImGuizmo::DrawGrid(glm::value_ptr(cameraView),
                       glm::value_ptr(cameraProjection),
                       glm::value_ptr(kIdentityMatrix), 10.f);

    ImGuizmo::DrawCubes(glm::value_ptr(cameraView),
                        glm::value_ptr(cameraProjection),
                        glm::value_ptr(modelMatrices[0]), gizmoCount);

#if PROFILE_CODE
    auto start = std::chrono::high_resolution_clock::now();
#endif
    for (int matId = 0; matId < gizmoCount; matId++) {
      ImGuizmo::SetID(matId);

      EditTransform(glm::value_ptr(cameraView),
                    glm::value_ptr(cameraProjection),
                    glm::value_ptr(modelMatrices[matId]), lastUsing == matId);
      if (ImGuizmo::IsUsing()) lastUsing = matId;
    }
#if PROFILE_CODE
    auto finish = std::chrono::high_resolution_clock::now();
    auto execTime =
      std::chrono::duration_cast<std::chrono::microseconds>(finish - start)
        .count();
#endif
    ImGui::End();

#if PROFILE_CODE
    if (currentSampleId < kNumSamples) {
      timeSamples[currentSampleId++] = execTime;
    }

    static long long avgTime = 0;
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
    ImGuizmo::ViewManipulate(glm::value_ptr(cameraView), camDistance,
                             ImVec2(io.DisplaySize.x - kViewSize, 0),
                             ImVec2(kViewSize, kViewSize), 0x10101010);

    // ---


    glClearColor(0.45f, 0.4f, 0.4f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    ImGui::Render();

    imApp.EndFrame();
  }

  imApp.Finish();

  return 0;
}