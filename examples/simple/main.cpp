#define _PROFILE_CODE 0
#if _PROFILE_CODE
#  include <algorithm>
#  include <array>
#  include <chrono>
#  include <execution>
#endif

#include "ImGuizmo.h"

#include "glad/glad.h"
#include "GLFW/glfw3.h"
#include "../backends/imgui_impl_glfw.h"
#include "../backends/imgui_impl_opengl3.h"

#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtx/wrap.hpp"

int main(int argc, char *argv[]) {
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
  _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
  _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);

#if _PROFILE_CODE
  constexpr auto num_samples = 1000;
  std::array<long long, num_samples> time_samples{};
  int current_sample_id{ 0 };
#endif

  if (!glfwInit()) return 1;
  glfwWindowHint(GLFW_MAXIMIZED, GLFW_TRUE);
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
  io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
  // io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;

  io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;
  io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
  io.ConfigWindowsMoveFromTitleBarOnly = true;

  // io.ConfigViewportsNoAutoMerge = true;
  io.ConfigViewportsNoTaskBarIcon = true;

#if 1
  ImGui::StyleColorsDark();
#else
  ImGui::StyleColorsClassic();
#endif

  // When viewports are enabled we tweak WindowRounding/WindowBg so platform
  // windows can look identical to regular ones.
  ImGuiStyle &style{ ImGui::GetStyle() };
  if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
    style.WindowRounding = 0.0f;
    style.Colors[ImGuiCol_WindowBg].w = 1.0f;
  }

  ImGui_ImplGlfw_InitForOpenGL(window, true);
  ImGui_ImplOpenGL3_Init();

  static glm::mat4 camera_projection{ 1.0f };
  static glm::mat4 camera_view{ 1.0f };

  float fov{ 60.0 };
  float view_width{ 10.0f }; // for ortho projection
  float cam_angle_y{ 165.0f / 180.0f * 3.14159f };
  float cam_angle_x{ 32.0f / 180.0f * 3.14159f };

  static glm::mat4 model_matrix{ 1.0f };

  bool show_demo_window{ true };
  ImVec4 clear_color{ 0.45f, 0.55f, 0.60f, 1.00f };

  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();

    static glm::ivec2 framebuffer_size;
    glfwGetFramebufferSize(window, &framebuffer_size.x, &framebuffer_size.y);

    float aspect_ratio =
      static_cast<float>(framebuffer_size.x) / framebuffer_size.y;

    static bool is_perspective{ true };
    if (is_perspective) {
      camera_projection =
        glm::perspective(glm::radians(fov), aspect_ratio, 0.1f, 1000.0f);
    } else {
      const float view_height{ view_width * framebuffer_size.y /
                               framebuffer_size.x };
      camera_projection = glm::ortho(-view_width, view_width, -view_height,
                                     view_height, -1000.0f, 1000.0f);
    }

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    if (show_demo_window) ImGui::ShowDemoWindow(&show_demo_window);

    ImGuizmo::PrintContext();

    static float cam_distance{ 8.0f };
    static bool view_dirty{ false };
    if (ImGui::Begin("Example")) {
      ImGui::Text("Camera");
      if (ImGui::RadioButton("Perspective", is_perspective))
        is_perspective = true;
      ImGui::SameLine();
      if (ImGui::RadioButton("Orthographic", !is_perspective))
        is_perspective = false;
      if (is_perspective) {
        ImGui::SliderFloat("FOV", &fov, 20.0f, 120.f);
      } else {
        ImGui::SliderFloat("Ortho width", &view_width, 1, 20);
      }
      view_dirty = ImGui::InputFloat("Distance", &cam_distance, 1.0f);
    }
    ImGui::End();

    static bool first_frame{ true };
    if (view_dirty || first_frame) {
      const glm::vec3 eye{
        glm::cos(cam_angle_y) * glm::cos(cam_angle_x) * cam_distance,
        glm::sin(cam_angle_x) * cam_distance,
        glm::sin(cam_angle_y) * glm::cos(cam_angle_x) * cam_distance
      };
      const glm::vec3 at{ 0.0f, 0.0f, 0.0f };
      const glm::vec3 up{ 0.0f, 1.0f, 0.0f };
      camera_view = glm::lookAt(eye, at, up);

      first_frame = false;
    }

    static const ImGuiViewport *main_viewport{ ImGui::GetMainViewport() };
    ImGui::SetNextWindowPos(main_viewport->Pos);
    ImGui::SetNextWindowSize(main_viewport->Size);
       
    ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
    ImGui::PushStyleColor(ImGuiCol_Border, 0);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

    constexpr ImGuiWindowFlags flags{ ImGuiWindowFlags_NoBackground |
                                      ImGuiWindowFlags_NoDecoration |
                                      ImGuiWindowFlags_NoSavedSettings |
                                      ImGuiWindowFlags_NoInputs |
                                      ImGuiWindowFlags_NoFocusOnAppearing |
                                      ImGuiWindowFlags_NoBringToFrontOnFocus };

    if (ImGui::Begin("Canvas", nullptr, flags)) {
      ImGuizmo::SetDrawlist(ImGui::GetWindowDrawList());
      // SetViewport(ImGui::GetWindowPos(), ImGui::GetWindowSize());
      ImGuizmo::DrawGrid(glm::value_ptr(camera_view),
                         glm::value_ptr(camera_projection),
                         glm::value_ptr(glm::mat4{ 1.0f }), 100.0f);
      ImGuizmo::DrawCubes(glm::value_ptr(camera_view),
                          glm::value_ptr(camera_projection),
                          glm::value_ptr(model_matrix), 1);

      ImGuizmo::SetCamera(glm::value_ptr(camera_view),
                          glm::value_ptr(camera_projection), !is_perspective);

      static ImGuizmoMode mode{ ImGuizmoMode_Global };
      static ImGuizmoOperation operation{ ImGuizmoOperation_Translate };

#if _PROFILE_CODE
      auto start = std::chrono::high_resolution_clock::now();
#endif
      ImGuizmo::Manipulate(mode, operation, glm::value_ptr(model_matrix));
#if _PROFILE_CODE
      auto finish = std::chrono::high_resolution_clock::now();
      auto exec_time =
        std::chrono::duration_cast<std::chrono::microseconds>(finish - start)
          .count();

      if (current_sample_id < num_samples)
        time_samples[current_sample_id++] = exec_time;

      static long long avg_time{ 0 };
      if (avg_time == 0 && current_sample_id == num_samples) {
        const auto accumulated = std::reduce(
          std::execution::par, time_samples.cbegin(), time_samples.cend());
        avg_time = accumulated / num_samples;
      }

      ImGui::Begin("Profiler");
      if (avg_time == 0)
        ImGui::Text("Gathering samples ... %d", current_sample_id);
      else
        ImGui::Text("Manipulate() = ~%ld microseconds", avg_time);
      ImGui::End();
#endif
    }
    ImGui::End();
    ImGui::PopStyleVar();
    ImGui::PopStyleColor(2);

    constexpr auto view_size = 256;
    ImGui::SetNextWindowSize(glm::vec2{ view_size } + glm::vec2{ 0, 20 });
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
      ImGuizmo::ViewManipulate(glm::value_ptr(camera_view), cam_distance,
                               position, glm::vec2{ view_size }, 0x10101010);
#endif
    }
    ImGui::End();

    ImGui::Render();

    glViewport(0, 0, framebuffer_size.x, framebuffer_size.y);
    glClearColor(clear_color.x, clear_color.y, clear_color.z, clear_color.w);
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