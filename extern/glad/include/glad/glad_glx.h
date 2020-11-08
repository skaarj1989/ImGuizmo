/*

    GLX loader generated by glad 0.1.34 on Sat Oct 24 10:00:54 2020.

    Language/Generator: C/C++
    Specification: glx
    APIs: glx=1.4
    Profile: -
    Extensions:
        
    Loader: True
    Local files: False
    Omit khrplatform: False
    Reproducible: False

    Commandline:
        --api="glx=1.4" --generator="c" --spec="glx" --extensions=""
    Online:
        https://glad.dav1d.de/#language=c&specification=glx&loader=on&api=glx%3D1.4
*/


#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <glad/glad.h>

#ifndef __glad_glxext_h_

#ifdef __glxext_h_
#error GLX header already included, remove this include, glad already provides it
#endif

#define __glad_glxext_h_
#define __glxext_h_

#ifndef APIENTRY
#define APIENTRY
#endif
#ifndef APIENTRYP
#define APIENTRYP APIENTRY *
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef void* (* GLADloadproc)(const char *name);

#ifndef GLAPI
# if defined(GLAD_GLAPI_EXPORT)
#  if defined(_WIN32) || defined(__CYGWIN__)
#   if defined(GLAD_GLAPI_EXPORT_BUILD)
#    if defined(__GNUC__)
#     define GLAPI __attribute__ ((dllexport)) extern
#    else
#     define GLAPI __declspec(dllexport) extern
#    endif
#   else
#    if defined(__GNUC__)
#     define GLAPI __attribute__ ((dllimport)) extern
#    else
#     define GLAPI __declspec(dllimport) extern
#    endif
#   endif
#  elif defined(__GNUC__) && defined(GLAD_GLAPI_EXPORT_BUILD)
#   define GLAPI __attribute__ ((visibility ("default"))) extern
#  else
#   define GLAPI extern
#  endif
# else
#  define GLAPI extern
# endif
#endif

GLAPI int gladLoadGLX(Display *dpy, int screen);

GLAPI int gladLoadGLXLoader(GLADloadproc, Display *dpy, int screen);

#ifndef GLEXT_64_TYPES_DEFINED
/* This code block is duplicated in glext.h, so must be protected */
#define GLEXT_64_TYPES_DEFINED
/* Define int32_t, int64_t, and uint64_t types for UST/MSC */
/* (as used in the GLX_OML_sync_control extension). */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <inttypes.h>
#elif defined(__sun__) || defined(__digital__)
#include <inttypes.h>
#if defined(__STDC__)
#if defined(__arch64__) || defined(_LP64)
typedef long int int64_t;
typedef unsigned long int uint64_t;
#else
typedef long long int int64_t;
typedef unsigned long long int uint64_t;
#endif /* __arch64__ */
#endif /* __STDC__ */
#elif defined( __VMS ) || defined(__sgi)
#include <inttypes.h>
#elif defined(__SCO__) || defined(__USLC__)
#include <stdint.h>
#elif defined(__UNIXOS2__) || defined(__SOL64__)
typedef long int int32_t;
typedef long long int int64_t;
typedef unsigned long long int uint64_t;
#elif defined(_WIN32) && defined(__GNUC__)
#include <stdint.h>
#elif defined(_WIN32)
typedef __int32 int32_t;
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#else
/* Fallback if nothing above works */
#include <inttypes.h>
#endif
#endif
typedef XID GLXFBConfigID;
typedef struct __GLXFBConfigRec *GLXFBConfig;
typedef XID GLXContextID;
typedef struct __GLXcontextRec *GLXContext;
typedef XID GLXPixmap;
typedef XID GLXDrawable;
typedef XID GLXWindow;
typedef XID GLXPbuffer;
typedef void (APIENTRY *__GLXextFuncPtr)(void);
typedef XID GLXVideoCaptureDeviceNV;
typedef unsigned int GLXVideoDeviceNV;
typedef XID GLXVideoSourceSGIX;
typedef XID GLXFBConfigIDSGIX;
typedef struct __GLXFBConfigRec *GLXFBConfigSGIX;
typedef XID GLXPbufferSGIX;
typedef struct {
    int event_type;      /* GLX_DAMAGED or GLX_SAVED */
    int draw_type;       /* GLX_WINDOW or GLX_PBUFFER */
    unsigned long serial;       /* # of last request processed by server */
    Bool send_event;     /* true if this came for SendEvent request */
    Display *display;    /* display the event was read from */
    GLXDrawable drawable;       /* XID of Drawable */
    unsigned int buffer_mask;   /* mask indicating which buffers are affected */
    unsigned int aux_buffer;    /* which aux buffer was affected */
    int x, y;
    int width, height;
    int count;    /* if nonzero, at least this many more */
} GLXPbufferClobberEvent;
typedef struct {
    int type;
    unsigned long serial;       /* # of last request processed by server */
    Bool send_event;     /* true if this came from a SendEvent request */
    Display *display;    /* Display the event was read from */
    GLXDrawable drawable;       /* drawable on which event was requested in event mask */
    int event_type;
    int64_t ust;
    int64_t msc;
    int64_t sbc;
} GLXBufferSwapComplete;
typedef union __GLXEvent {
    GLXPbufferClobberEvent glxpbufferclobber;
    GLXBufferSwapComplete glxbufferswapcomplete;
    long pad[24];
} GLXEvent;
typedef struct {
    int type;
    unsigned long serial;
    Bool send_event;
    Display *display;
    int extension;
    int evtype;
    GLXDrawable window;
    Bool stereo_tree;
} GLXStereoNotifyEventEXT;
typedef struct {
    int type;
    unsigned long serial;   /* # of last request processed by server */
    Bool send_event; /* true if this came for SendEvent request */
    Display *display;       /* display the event was read from */
    GLXDrawable drawable;   /* i.d. of Drawable */
    int event_type;  /* GLX_DAMAGED_SGIX or GLX_SAVED_SGIX */
    int draw_type;   /* GLX_WINDOW_SGIX or GLX_PBUFFER_SGIX */
    unsigned int mask;      /* mask indicating which buffers are affected*/
    int x, y;
    int width, height;
    int count;       /* if nonzero, at least this many more */
} GLXBufferClobberEventSGIX;
typedef struct {
    char    pipeName[80]; /* Should be [GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX] */
    int     networkId;
} GLXHyperpipeNetworkSGIX;
typedef struct {
    char    pipeName[80]; /* Should be [GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX] */
    int     channel;
    unsigned int participationType;
    int     timeSlice;
} GLXHyperpipeConfigSGIX;
typedef struct {
    char pipeName[80]; /* Should be [GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX] */
    int srcXOrigin, srcYOrigin, srcWidth, srcHeight;
    int destXOrigin, destYOrigin, destWidth, destHeight;
} GLXPipeRect;
typedef struct {
    char pipeName[80]; /* Should be [GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX] */
    int XOrigin, YOrigin, maxHeight, maxWidth;
} GLXPipeRectLimits;
#define GLX_EXTENSION_NAME "GLX"
#define GLX_PbufferClobber 0
#define GLX_BufferSwapComplete 1
#define __GLX_NUMBER_EVENTS 17
#define GLX_BAD_SCREEN 1
#define GLX_BAD_ATTRIBUTE 2
#define GLX_NO_EXTENSION 3
#define GLX_BAD_VISUAL 4
#define GLX_BAD_CONTEXT 5
#define GLX_BAD_VALUE 6
#define GLX_BAD_ENUM 7
#define GLX_USE_GL 1
#define GLX_BUFFER_SIZE 2
#define GLX_LEVEL 3
#define GLX_RGBA 4
#define GLX_DOUBLEBUFFER 5
#define GLX_STEREO 6
#define GLX_AUX_BUFFERS 7
#define GLX_RED_SIZE 8
#define GLX_GREEN_SIZE 9
#define GLX_BLUE_SIZE 10
#define GLX_ALPHA_SIZE 11
#define GLX_DEPTH_SIZE 12
#define GLX_STENCIL_SIZE 13
#define GLX_ACCUM_RED_SIZE 14
#define GLX_ACCUM_GREEN_SIZE 15
#define GLX_ACCUM_BLUE_SIZE 16
#define GLX_ACCUM_ALPHA_SIZE 17
#define GLX_VENDOR 0x1
#define GLX_VERSION 0x2
#define GLX_EXTENSIONS 0x3
#define GLX_WINDOW_BIT 0x00000001
#define GLX_PIXMAP_BIT 0x00000002
#define GLX_PBUFFER_BIT 0x00000004
#define GLX_RGBA_BIT 0x00000001
#define GLX_COLOR_INDEX_BIT 0x00000002
#define GLX_PBUFFER_CLOBBER_MASK 0x08000000
#define GLX_FRONT_LEFT_BUFFER_BIT 0x00000001
#define GLX_FRONT_RIGHT_BUFFER_BIT 0x00000002
#define GLX_BACK_LEFT_BUFFER_BIT 0x00000004
#define GLX_BACK_RIGHT_BUFFER_BIT 0x00000008
#define GLX_AUX_BUFFERS_BIT 0x00000010
#define GLX_DEPTH_BUFFER_BIT 0x00000020
#define GLX_STENCIL_BUFFER_BIT 0x00000040
#define GLX_ACCUM_BUFFER_BIT 0x00000080
#define GLX_CONFIG_CAVEAT 0x20
#define GLX_X_VISUAL_TYPE 0x22
#define GLX_TRANSPARENT_TYPE 0x23
#define GLX_TRANSPARENT_INDEX_VALUE 0x24
#define GLX_TRANSPARENT_RED_VALUE 0x25
#define GLX_TRANSPARENT_GREEN_VALUE 0x26
#define GLX_TRANSPARENT_BLUE_VALUE 0x27
#define GLX_TRANSPARENT_ALPHA_VALUE 0x28
#define GLX_DONT_CARE 0xFFFFFFFF
#define GLX_NONE 0x8000
#define GLX_SLOW_CONFIG 0x8001
#define GLX_TRUE_COLOR 0x8002
#define GLX_DIRECT_COLOR 0x8003
#define GLX_PSEUDO_COLOR 0x8004
#define GLX_STATIC_COLOR 0x8005
#define GLX_GRAY_SCALE 0x8006
#define GLX_STATIC_GRAY 0x8007
#define GLX_TRANSPARENT_RGB 0x8008
#define GLX_TRANSPARENT_INDEX 0x8009
#define GLX_VISUAL_ID 0x800B
#define GLX_SCREEN 0x800C
#define GLX_NON_CONFORMANT_CONFIG 0x800D
#define GLX_DRAWABLE_TYPE 0x8010
#define GLX_RENDER_TYPE 0x8011
#define GLX_X_RENDERABLE 0x8012
#define GLX_FBCONFIG_ID 0x8013
#define GLX_RGBA_TYPE 0x8014
#define GLX_COLOR_INDEX_TYPE 0x8015
#define GLX_MAX_PBUFFER_WIDTH 0x8016
#define GLX_MAX_PBUFFER_HEIGHT 0x8017
#define GLX_MAX_PBUFFER_PIXELS 0x8018
#define GLX_PRESERVED_CONTENTS 0x801B
#define GLX_LARGEST_PBUFFER 0x801C
#define GLX_WIDTH 0x801D
#define GLX_HEIGHT 0x801E
#define GLX_EVENT_MASK 0x801F
#define GLX_DAMAGED 0x8020
#define GLX_SAVED 0x8021
#define GLX_WINDOW 0x8022
#define GLX_PBUFFER 0x8023
#define GLX_PBUFFER_HEIGHT 0x8040
#define GLX_PBUFFER_WIDTH 0x8041
#define GLX_SAMPLE_BUFFERS 100000
#define GLX_SAMPLES 100001
#ifndef GLX_VERSION_1_0
#define GLX_VERSION_1_0 1
GLAPI int GLAD_GLX_VERSION_1_0;
typedef XVisualInfo * (APIENTRYP PFNGLXCHOOSEVISUALPROC)(Display *dpy, int screen, int *attribList);
GLAPI PFNGLXCHOOSEVISUALPROC glad_glXChooseVisual;
#define glXChooseVisual glad_glXChooseVisual
typedef GLXContext (APIENTRYP PFNGLXCREATECONTEXTPROC)(Display *dpy, XVisualInfo *vis, GLXContext shareList, Bool direct);
GLAPI PFNGLXCREATECONTEXTPROC glad_glXCreateContext;
#define glXCreateContext glad_glXCreateContext
typedef void (APIENTRYP PFNGLXDESTROYCONTEXTPROC)(Display *dpy, GLXContext ctx);
GLAPI PFNGLXDESTROYCONTEXTPROC glad_glXDestroyContext;
#define glXDestroyContext glad_glXDestroyContext
typedef Bool (APIENTRYP PFNGLXMAKECURRENTPROC)(Display *dpy, GLXDrawable drawable, GLXContext ctx);
GLAPI PFNGLXMAKECURRENTPROC glad_glXMakeCurrent;
#define glXMakeCurrent glad_glXMakeCurrent
typedef void (APIENTRYP PFNGLXCOPYCONTEXTPROC)(Display *dpy, GLXContext src, GLXContext dst, unsigned long mask);
GLAPI PFNGLXCOPYCONTEXTPROC glad_glXCopyContext;
#define glXCopyContext glad_glXCopyContext
typedef void (APIENTRYP PFNGLXSWAPBUFFERSPROC)(Display *dpy, GLXDrawable drawable);
GLAPI PFNGLXSWAPBUFFERSPROC glad_glXSwapBuffers;
#define glXSwapBuffers glad_glXSwapBuffers
typedef GLXPixmap (APIENTRYP PFNGLXCREATEGLXPIXMAPPROC)(Display *dpy, XVisualInfo *visual, Pixmap pixmap);
GLAPI PFNGLXCREATEGLXPIXMAPPROC glad_glXCreateGLXPixmap;
#define glXCreateGLXPixmap glad_glXCreateGLXPixmap
typedef void (APIENTRYP PFNGLXDESTROYGLXPIXMAPPROC)(Display *dpy, GLXPixmap pixmap);
GLAPI PFNGLXDESTROYGLXPIXMAPPROC glad_glXDestroyGLXPixmap;
#define glXDestroyGLXPixmap glad_glXDestroyGLXPixmap
typedef Bool (APIENTRYP PFNGLXQUERYEXTENSIONPROC)(Display *dpy, int *errorb, int *event);
GLAPI PFNGLXQUERYEXTENSIONPROC glad_glXQueryExtension;
#define glXQueryExtension glad_glXQueryExtension
typedef Bool (APIENTRYP PFNGLXQUERYVERSIONPROC)(Display *dpy, int *maj, int *min);
GLAPI PFNGLXQUERYVERSIONPROC glad_glXQueryVersion;
#define glXQueryVersion glad_glXQueryVersion
typedef Bool (APIENTRYP PFNGLXISDIRECTPROC)(Display *dpy, GLXContext ctx);
GLAPI PFNGLXISDIRECTPROC glad_glXIsDirect;
#define glXIsDirect glad_glXIsDirect
typedef int (APIENTRYP PFNGLXGETCONFIGPROC)(Display *dpy, XVisualInfo *visual, int attrib, int *value);
GLAPI PFNGLXGETCONFIGPROC glad_glXGetConfig;
#define glXGetConfig glad_glXGetConfig
typedef GLXContext (APIENTRYP PFNGLXGETCURRENTCONTEXTPROC)(void);
GLAPI PFNGLXGETCURRENTCONTEXTPROC glad_glXGetCurrentContext;
#define glXGetCurrentContext glad_glXGetCurrentContext
typedef GLXDrawable (APIENTRYP PFNGLXGETCURRENTDRAWABLEPROC)(void);
GLAPI PFNGLXGETCURRENTDRAWABLEPROC glad_glXGetCurrentDrawable;
#define glXGetCurrentDrawable glad_glXGetCurrentDrawable
typedef void (APIENTRYP PFNGLXWAITGLPROC)(void);
GLAPI PFNGLXWAITGLPROC glad_glXWaitGL;
#define glXWaitGL glad_glXWaitGL
typedef void (APIENTRYP PFNGLXWAITXPROC)(void);
GLAPI PFNGLXWAITXPROC glad_glXWaitX;
#define glXWaitX glad_glXWaitX
typedef void (APIENTRYP PFNGLXUSEXFONTPROC)(Font font, int first, int count, int list);
GLAPI PFNGLXUSEXFONTPROC glad_glXUseXFont;
#define glXUseXFont glad_glXUseXFont
#endif
#ifndef GLX_VERSION_1_1
#define GLX_VERSION_1_1 1
GLAPI int GLAD_GLX_VERSION_1_1;
typedef const char * (APIENTRYP PFNGLXQUERYEXTENSIONSSTRINGPROC)(Display *dpy, int screen);
GLAPI PFNGLXQUERYEXTENSIONSSTRINGPROC glad_glXQueryExtensionsString;
#define glXQueryExtensionsString glad_glXQueryExtensionsString
typedef const char * (APIENTRYP PFNGLXQUERYSERVERSTRINGPROC)(Display *dpy, int screen, int name);
GLAPI PFNGLXQUERYSERVERSTRINGPROC glad_glXQueryServerString;
#define glXQueryServerString glad_glXQueryServerString
typedef const char * (APIENTRYP PFNGLXGETCLIENTSTRINGPROC)(Display *dpy, int name);
GLAPI PFNGLXGETCLIENTSTRINGPROC glad_glXGetClientString;
#define glXGetClientString glad_glXGetClientString
#endif
#ifndef GLX_VERSION_1_2
#define GLX_VERSION_1_2 1
GLAPI int GLAD_GLX_VERSION_1_2;
typedef Display * (APIENTRYP PFNGLXGETCURRENTDISPLAYPROC)(void);
GLAPI PFNGLXGETCURRENTDISPLAYPROC glad_glXGetCurrentDisplay;
#define glXGetCurrentDisplay glad_glXGetCurrentDisplay
#endif
#ifndef GLX_VERSION_1_3
#define GLX_VERSION_1_3 1
GLAPI int GLAD_GLX_VERSION_1_3;
typedef GLXFBConfig * (APIENTRYP PFNGLXGETFBCONFIGSPROC)(Display *dpy, int screen, int *nelements);
GLAPI PFNGLXGETFBCONFIGSPROC glad_glXGetFBConfigs;
#define glXGetFBConfigs glad_glXGetFBConfigs
typedef GLXFBConfig * (APIENTRYP PFNGLXCHOOSEFBCONFIGPROC)(Display *dpy, int screen, const int *attrib_list, int *nelements);
GLAPI PFNGLXCHOOSEFBCONFIGPROC glad_glXChooseFBConfig;
#define glXChooseFBConfig glad_glXChooseFBConfig
typedef int (APIENTRYP PFNGLXGETFBCONFIGATTRIBPROC)(Display *dpy, GLXFBConfig config, int attribute, int *value);
GLAPI PFNGLXGETFBCONFIGATTRIBPROC glad_glXGetFBConfigAttrib;
#define glXGetFBConfigAttrib glad_glXGetFBConfigAttrib
typedef XVisualInfo * (APIENTRYP PFNGLXGETVISUALFROMFBCONFIGPROC)(Display *dpy, GLXFBConfig config);
GLAPI PFNGLXGETVISUALFROMFBCONFIGPROC glad_glXGetVisualFromFBConfig;
#define glXGetVisualFromFBConfig glad_glXGetVisualFromFBConfig
typedef GLXWindow (APIENTRYP PFNGLXCREATEWINDOWPROC)(Display *dpy, GLXFBConfig config, Window win, const int *attrib_list);
GLAPI PFNGLXCREATEWINDOWPROC glad_glXCreateWindow;
#define glXCreateWindow glad_glXCreateWindow
typedef void (APIENTRYP PFNGLXDESTROYWINDOWPROC)(Display *dpy, GLXWindow win);
GLAPI PFNGLXDESTROYWINDOWPROC glad_glXDestroyWindow;
#define glXDestroyWindow glad_glXDestroyWindow
typedef GLXPixmap (APIENTRYP PFNGLXCREATEPIXMAPPROC)(Display *dpy, GLXFBConfig config, Pixmap pixmap, const int *attrib_list);
GLAPI PFNGLXCREATEPIXMAPPROC glad_glXCreatePixmap;
#define glXCreatePixmap glad_glXCreatePixmap
typedef void (APIENTRYP PFNGLXDESTROYPIXMAPPROC)(Display *dpy, GLXPixmap pixmap);
GLAPI PFNGLXDESTROYPIXMAPPROC glad_glXDestroyPixmap;
#define glXDestroyPixmap glad_glXDestroyPixmap
typedef GLXPbuffer (APIENTRYP PFNGLXCREATEPBUFFERPROC)(Display *dpy, GLXFBConfig config, const int *attrib_list);
GLAPI PFNGLXCREATEPBUFFERPROC glad_glXCreatePbuffer;
#define glXCreatePbuffer glad_glXCreatePbuffer
typedef void (APIENTRYP PFNGLXDESTROYPBUFFERPROC)(Display *dpy, GLXPbuffer pbuf);
GLAPI PFNGLXDESTROYPBUFFERPROC glad_glXDestroyPbuffer;
#define glXDestroyPbuffer glad_glXDestroyPbuffer
typedef void (APIENTRYP PFNGLXQUERYDRAWABLEPROC)(Display *dpy, GLXDrawable draw, int attribute, unsigned int *value);
GLAPI PFNGLXQUERYDRAWABLEPROC glad_glXQueryDrawable;
#define glXQueryDrawable glad_glXQueryDrawable
typedef GLXContext (APIENTRYP PFNGLXCREATENEWCONTEXTPROC)(Display *dpy, GLXFBConfig config, int render_type, GLXContext share_list, Bool direct);
GLAPI PFNGLXCREATENEWCONTEXTPROC glad_glXCreateNewContext;
#define glXCreateNewContext glad_glXCreateNewContext
typedef Bool (APIENTRYP PFNGLXMAKECONTEXTCURRENTPROC)(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext ctx);
GLAPI PFNGLXMAKECONTEXTCURRENTPROC glad_glXMakeContextCurrent;
#define glXMakeContextCurrent glad_glXMakeContextCurrent
typedef GLXDrawable (APIENTRYP PFNGLXGETCURRENTREADDRAWABLEPROC)(void);
GLAPI PFNGLXGETCURRENTREADDRAWABLEPROC glad_glXGetCurrentReadDrawable;
#define glXGetCurrentReadDrawable glad_glXGetCurrentReadDrawable
typedef int (APIENTRYP PFNGLXQUERYCONTEXTPROC)(Display *dpy, GLXContext ctx, int attribute, int *value);
GLAPI PFNGLXQUERYCONTEXTPROC glad_glXQueryContext;
#define glXQueryContext glad_glXQueryContext
typedef void (APIENTRYP PFNGLXSELECTEVENTPROC)(Display *dpy, GLXDrawable draw, unsigned long event_mask);
GLAPI PFNGLXSELECTEVENTPROC glad_glXSelectEvent;
#define glXSelectEvent glad_glXSelectEvent
typedef void (APIENTRYP PFNGLXGETSELECTEDEVENTPROC)(Display *dpy, GLXDrawable draw, unsigned long *event_mask);
GLAPI PFNGLXGETSELECTEDEVENTPROC glad_glXGetSelectedEvent;
#define glXGetSelectedEvent glad_glXGetSelectedEvent
#endif
#ifndef GLX_VERSION_1_4
#define GLX_VERSION_1_4 1
GLAPI int GLAD_GLX_VERSION_1_4;
typedef __GLXextFuncPtr (APIENTRYP PFNGLXGETPROCADDRESSPROC)(const GLubyte *procName);
GLAPI PFNGLXGETPROCADDRESSPROC glad_glXGetProcAddress;
#define glXGetProcAddress glad_glXGetProcAddress
#endif

#ifdef __cplusplus
}
#endif

#endif
