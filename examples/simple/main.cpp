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
  glfwWindowHint(GLFW_MAXIMIZED, GLFW_FALSE);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);

  GLFWwindow *window{ glfwCreateWindow(1280, 720, "ImGuizmo example", nullptr,
                                       nullptr) };
  if (window == nullptr) return 1;
  glfwMakeContextCurrent(window);

  if (!gladLoadGL()) {
    fprintf(stderr, "Failed to initialize OpenGL loader!\n");
    return 1;
  }

  IMGUI_CHECKVERSION();
  ImGui::CreateContext();
  ImGuiIO &io{ ImGui::GetIO() };
  io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
  io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;
  io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
  io.ConfigWindowsMoveFromTitleBarOnly = true;

  // io.ConfigViewportsNoAutoMerge = true;
  io.ConfigViewportsNoTaskBarIcon = true;

  ImGui::StyleColorsDark();
  ImGuizmo::StyleColorsUnreal();

  // When viewports are enabled we tweak WindowRounding/WindowBg so platform
  // windows can look identical to regular ones.
  ImGuiStyle &style{ ImGui::GetStyle() };
  if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable) {
    style.WindowRounding = 0.0f;
    style.Colors[ImGuiCol_WindowBg].w = 1.0f;
  }

  ImGui_ImplGlfw_InitForOpenGL(window, true);
  ImGui_ImplOpenGL3_Init();

  constexpr float cam_distance{ 8.0f };
  constexpr float cam_angle_y { glm::radians(165.0f)};
  constexpr float cam_angle_x{ glm::radians(32.0f) };
  const glm::vec3 eye{
    glm::cos(cam_angle_y) * glm::cos(cam_angle_x) * cam_distance,
    glm::sin(cam_angle_x) * cam_distance,
    glm::sin(cam_angle_y) * glm::cos(cam_angle_x) * cam_distance
  };
  const glm::vec3 at{ 0.0f, 0.0f, 0.0f };
  const glm::vec3 up{ 0.0f, 1.0f, 0.0f };
  static glm::mat4 view_matrix{ glm::lookAt(eye, at, up) };
  static glm::mat4 projection_matrix{ 1.0f };
  static glm::mat4 model_matrix{ 1.0f };

  const ImVec4 clear_color{ 0.45f, 0.55f, 0.60f, 1.00f };
  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();

    static glm::ivec2 framebuffer_size;
    glfwGetFramebufferSize(window, &framebuffer_size.x, &framebuffer_size.y);
    float aspect_ratio = static_cast<float>(framebuffer_size.x) /
                         static_cast<float>(framebuffer_size.y);
    projection_matrix =
      glm::perspective(glm::radians(60.0f), aspect_ratio, 0.1f, 1000.0f);

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    ImGuizmo::PrintContext();

    static ImGuizmoMode mode{ ImGuizmoMode_Local };
    static ImGuizmoOperation operation{ ImGuizmoOperation_Translate };
    if (ImGui::Begin("ImGuizmo")) {
      if (ImGui::IsKeyPressed(GLFW_KEY_W))
        operation = ImGuizmoOperation_Translate;
      if (ImGui::IsKeyPressed(GLFW_KEY_E)) operation = ImGuizmoOperation_Rotate;
      if (ImGui::IsKeyPressed(GLFW_KEY_R)) operation = ImGuizmoOperation_Scale;

      if (ImGui::RadioButton("Translate",
                             operation == ImGuizmoOperation_Translate)) {
        operation = ImGuizmoOperation_Translate;
      }
      ImGui::SameLine();
      if (ImGui::RadioButton("Rotate", operation == ImGuizmoOperation_Rotate)) {
        operation = ImGuizmoOperation_Rotate;
      }
      ImGui::SameLine();
      if (ImGui::RadioButton("Scale", operation == ImGuizmoOperation_Scale)) {
        operation = ImGuizmoOperation_Scale;
      }

      if (operation != ImGuizmoOperation_Scale) {
        if (ImGui::RadioButton("Local", mode == ImGuizmoMode_Local)) {
          mode = ImGuizmoMode_Local;
        }
        ImGui::SameLine();
        if (ImGui::RadioButton("Global", mode == ImGuizmoMode_World)) {
          mode = ImGuizmoMode_World;
        }
      } else {
        mode = ImGuizmoMode_Local;
      }
    }
    ImGui::End();

    static const ImGuiViewport *main_viewport{ ImGui::GetMainViewport() };
    ImGui::SetNextWindowPos(main_viewport->Pos);
    ImGui::SetNextWindowSize(main_viewport->Size);
       
    ImGui::PushStyleColor(ImGuiCol_WindowBg, 0);
    ImGui::PushStyleColor(ImGuiCol_Border, 0);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);

    constexpr ImGuiWindowFlags window_flags{ ImGuiWindowFlags_NoBackground |
                                      ImGuiWindowFlags_NoDecoration |
                                      ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoDocking |
                                      ImGuiWindowFlags_NoFocusOnAppearing |
                                      ImGuiWindowFlags_NoBringToFrontOnFocus };

    if (ImGui::Begin("Canvas", nullptr, window_flags)) {
      ImGuizmo::SetCamera(glm::value_ptr(view_matrix),
                          glm::value_ptr(projection_matrix), false);
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
      constexpr float manip_size{ 128.0f };
      glm::vec2 position{ main_viewport->Pos };
      position.x += main_viewport->Size.x - manip_size;
      ImGuizmo::ViewManipulate(glm::value_ptr(view_matrix), cam_distance,
                               position, glm::vec2{ manip_size }, 0x10101010);
    }
    ImGui::End();
    ImGui::PopStyleVar();
    ImGui::PopStyleColor(2);

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