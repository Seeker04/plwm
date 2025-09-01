#include <stdio.h>

#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xproto.h>

#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrandr.h>

Window x11plwm_DefaultRootWindow(Display *display) {
    return DefaultRootWindow(display);
}

int x11plwm_DefaultScreen(Display *display) {
    return DefaultScreen(display);
}


Colormap x11plwm_DefaultColormap(Display *display, int screen_nr) {
    return DefaultColormap(display, screen_nr);
}

Visual* x11plwm_DefaultVisual(Display *display, int screen_nr) {
    return DefaultVisual(display, screen_nr);
}


static int
xerror(Display __attribute__((unused)) *dpy, const XErrorEvent *ee)
{
	/* copied from dwm */
	if (ee->error_code == BadWindow
	|| (ee->request_code == X_SetInputFocus     && ee->error_code == BadMatch)
	|| (ee->request_code == X_PolyText8         && ee->error_code == BadDrawable)
	|| (ee->request_code == X_PolyFillRectangle && ee->error_code == BadDrawable)
	|| (ee->request_code == X_PolySegment       && ee->error_code == BadDrawable)
	|| (ee->request_code == X_ConfigureWindow   && ee->error_code == BadMatch)
	|| (ee->request_code == X_GrabButton        && ee->error_code == BadAccess)
	|| (ee->request_code == X_GrabKey           && ee->error_code == BadAccess)
	|| (ee->request_code == X_CopyArea          && ee->error_code == BadDrawable))
		return 0;
	fprintf(stderr, "xerror: fatal X error - request code=%d, error code=%d\n",
		ee->request_code, ee->error_code);
	return -1;
}

static int
xerrordummy(const Display __attribute__((unused)) *dpy, const XErrorEvent __attribute__((unused)) *ee)
{
	/* copied from dwm */
	return 0;
}

void x11plwm_set_error_handler(Bool dummy)
{
	if (dummy) {
		XSetErrorHandler((XErrorHandler)xerrordummy);
	}
	else {
		XSetErrorHandler((XErrorHandler)xerror);
	}
}
