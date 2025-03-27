/* MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE
 *
 * plxlib - X11 library bindings to SWI-Prolog for plwm
 *
 */

#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xproto.h>

#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrandr.h>

#include <SWI-Prolog.h>

#define Q(x) #x
#define QUOTE(x) Q(x)
#define GET_MACRO(_1,_2,NAME,...) NAME

#define PL_TRY1(plcall) PL_TRY2(plcall, {})
#define PL_TRY2(plcall, cleanup) \
	if (!plcall) {           \
		cleanup;         \
		return (foreign_t)PL_warning("%s: " QUOTE((plcall)) " failed!", __func__); \
	}
#define PL_TRY(...) GET_MACRO(__VA_ARGS__, PL_TRY2, PL_TRY1,)(__VA_ARGS__)

/* Bindings to swipl predicates */
static foreign_t x_open_display(term_t display_name, term_t display);
static foreign_t x_close_display(term_t display);
static foreign_t x_set_error_handler(term_t to_dummy);
static foreign_t x_set_close_down_mode(term_t display, term_t close_mode);
static foreign_t x_grab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window,
                            term_t owner_events, term_t pointer_mode, term_t keyboard_mode);
static foreign_t x_grab_button(term_t display, term_t button, term_t modifiers, term_t grab_window,
                               term_t owner_events, term_t event_mask, term_t pointer_mode,
                               term_t keyboard_mode, term_t confine_to, term_t cursor);
static foreign_t x_grab_pointer(term_t display, term_t grab_window, term_t owner_events, term_t event_mask,
                                term_t pointer_mode, term_t keyboard_mode, term_t confine_to, term_t cursor, term_t time);
static foreign_t x_grab_server(term_t display);
static foreign_t x_ungrab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window);
static foreign_t x_ungrab_button(term_t display, term_t button, term_t modifiers, term_t grab_window);
static foreign_t x_ungrab_pointer(term_t display, term_t time);
static foreign_t x_ungrab_server(term_t display);
static foreign_t x_keysym_to_keycode(term_t display, term_t keysym, term_t keycode);
static foreign_t x_string_to_keysym(term_t string, term_t keysym);
static foreign_t x_next_event(term_t display, term_t event_return);
static foreign_t x_send_event(term_t display, term_t w, term_t propagate, term_t event_mask, term_t event_send);
static foreign_t x_raise_window(term_t display, term_t w);
static foreign_t x_get_window_attributes(term_t display, term_t w, term_t window_attributes_return, term_t status);
static foreign_t x_move_resize_window(term_t display, term_t w, term_t x, term_t y, term_t width, term_t height);
static foreign_t x_change_window_attributes(term_t display, term_t w, term_t valuemask, term_t event_mask);
static foreign_t x_select_input(term_t display, term_t w, term_t event_mask);
static foreign_t x_map_window(term_t display, term_t w);
static foreign_t x_configure_window(term_t display, term_t w, term_t value_mask, term_t x, term_t y, term_t width,
                                    term_t height, term_t border_width, term_t sibling, term_t stack_mode);
static foreign_t x_set_window_border(term_t display, term_t w, term_t border_pixel);
static foreign_t x_set_input_focus(term_t display, term_t focus, term_t revert_to, term_t time);
static foreign_t x_kill_client(term_t display, term_t resource);
static foreign_t x_sync(term_t display, term_t discard);
static foreign_t x_intern_atom(term_t display, term_t atom_name, term_t only_if_exists, term_t atom);
static foreign_t x_get_class_hint(term_t display, term_t w, term_t res_name, term_t res_class);
static foreign_t x_change_property(term_t display, term_t w, term_t property, term_t atom, term_t format,
                                   term_t mode, term_t data, term_t nelements);
static foreign_t x_delete_property(term_t display, term_t w, term_t property);
static foreign_t x_utf8_text_list_to_text_property(term_t display, term_t list, term_t count, term_t style,
                                                   term_t text_prop_return);
static foreign_t x_get_text_property(term_t display, term_t w, term_t text, term_t property, term_t status);
static foreign_t x_set_text_property(term_t display, term_t w, term_t text_prop, term_t property);
static foreign_t x_create_simple_window(term_t display, term_t parent, term_t x, term_t y, term_t width,
                                        term_t height, term_t border_width, term_t border, term_t background, term_t w);
static foreign_t x_get_transient_for_hint(term_t display, term_t w, term_t prop_window_return, term_t status);
static foreign_t x_get_window_property(term_t display, term_t w, term_t property, term_t delete, term_t req_type,
                                       term_t prop_return);
static foreign_t x_get_wm_protocols(term_t display, term_t w, term_t protocols_return, term_t count_return);
static foreign_t x_get_wm_normal_hints(term_t display, term_t w, term_t hints_return, term_t status);
static foreign_t x_warp_pointer(term_t display, term_t src_w, term_t dest_w, term_t src_x, term_t src_y,
                                term_t src_width, term_t src_height, term_t dest_x, term_t dest_y);

static foreign_t default_root_window(term_t display, term_t w);
static foreign_t default_screen(term_t display, term_t screen);
static foreign_t default_visual(term_t display, term_t screen_number, term_t visual);
static foreign_t default_colormap(term_t display, term_t screen_number, term_t colormap);

/* Xft */
static foreign_t xft_color_alloc_name(term_t display, term_t visual, term_t cmap, term_t name, term_t result);

/* XRandR */
static foreign_t xrr_query_extension(term_t dpy, term_t event_base_return, term_t error_base_return);
static foreign_t xrr_select_input(term_t dpy, term_t window, term_t mask);
static foreign_t xrr_get_screen_resources(term_t dpy, term_t window, term_t screen_resources, term_t outputs);
static foreign_t xrr_free_screen_resources(term_t resources);
static foreign_t xrr_get_output_info(term_t dpy, term_t resources, term_t output_index, term_t output_info);
static foreign_t xrr_get_crtc_info(term_t dpy, term_t resources, term_t crtc, term_t crtc_info);

static foreign_t create_configure_event(term_t display, term_t w, term_t configure_event);
static foreign_t create_clientmessage_event(term_t w, term_t message_type, term_t format, term_t datal0, term_t datal1,
                                            term_t clientmessage);

static foreign_t c_free(term_t ptr);

/* Helpers */
static int build_list(term_t dst, term_t *src, size_t size);
static int xerror(Display __attribute__((unused)) *dpy, const XErrorEvent *ee); /* copied from dwm */
static int xerrordummy(const Display *dpy, const XErrorEvent *ee); /* copied from dwm */

static int rr_event_base = -1;

static PL_extension predicates[] = {
	/* functor name               arity C-callback                 flags   remarks */
	{ "x_open_display"            ,  2, x_open_display             , 0 }, /* pass "" for XOpenDisplay(NULL) */
	{ "x_close_display"           ,  1, x_close_display            , 0 },
	{ "x_set_error_handler"       ,  1, x_set_error_handler        , 0 }, /* it sets xerror() or xerrordummy() */
	{ "x_set_close_down_mode"     ,  2, x_set_close_down_mode      , 0 },
	{ "x_grab_key"                ,  7, x_grab_key                 , 0 },
	{ "x_grab_button"             , 10, x_grab_button              , 0 },
	{ "x_grab_pointer"            ,  9, x_grab_pointer             , 0 }, /* Unused */
	{ "x_grab_server"             ,  1, x_grab_server              , 0 },
	{ "x_ungrab_key"              ,  4, x_ungrab_key               , 0 },
	{ "x_ungrab_button"           ,  4, x_ungrab_button            , 0 },
	{ "x_ungrab_pointer"          ,  2, x_ungrab_pointer           , 0 }, /* Unused */
	{ "x_ungrab_server"           ,  1, x_ungrab_server            , 0 },
	{ "x_keysym_to_keycode"       ,  3, x_keysym_to_keycode        , 0 },
	{ "x_string_to_keysym"        ,  2, x_string_to_keysym         , 0 },
	{ "x_next_event"              ,  2, x_next_event               , 0 },
	{ "x_send_event"              ,  5, x_send_event               , 0 },
	{ "x_raise_window"            ,  2, x_raise_window             , 0 },
	{ "x_get_window_attributes"   ,  4, x_get_window_attributes    , 0 },
	{ "x_move_resize_window"      ,  6, x_move_resize_window       , 0 },
	{ "x_change_window_attributes",  4, x_change_window_attributes , 0 }, /* Only sets event_mask for now! */
	{ "x_select_input"            ,  3, x_select_input             , 0 },
	{ "x_map_window"              ,  2, x_map_window               , 0 },
	{ "x_configure_window"        , 10, x_configure_window         , 0 },
	{ "x_set_window_border"       ,  3, x_set_window_border        , 0 },
	{ "x_set_input_focus"         ,  4, x_set_input_focus          , 0 },
	{ "x_kill_client"             ,  2, x_kill_client              , 0 },
	{ "x_sync"                    ,  2, x_sync                     , 0 },
	{ "x_intern_atom"             ,  4, x_intern_atom              , 0 },
	{ "x_get_class_hint"          ,  4, x_get_class_hint           , 0 },
	{ "x_change_property"         ,  8, x_change_property          , 0 },
	{ "x_delete_property"         ,  3, x_delete_property          , 0 },
	{ "x_utf8_text_list_to_text_property",  5, x_utf8_text_list_to_text_property, 0 },
	{ "x_get_text_property"       ,  5, x_get_text_property        , 0 },
	{ "x_set_text_property"       ,  4, x_set_text_property        , 0 },
	{ "x_create_simple_window"    , 10, x_create_simple_window     , 0 },
	{ "x_get_transient_for_hint"  ,  4, x_get_transient_for_hint   , 0 },
	{ "x_get_window_property"     ,  6, x_get_window_property      , 0 }, /* 4th, 5th, 8th-11th args are omitted */
	{ "x_get_wm_protocols"        ,  4, x_get_wm_protocols         , 0 },
	{ "x_get_wm_normal_hints"     ,  4, x_get_wm_normal_hints      , 0 }, /* supplied_return arg is ignored */
	{ "x_warp_pointer"            ,  9, x_warp_pointer             , 0 }, /* Unused */

	{ "default_root_window"       ,  2, default_root_window        , 0 },
	{ "default_screen"            ,  2, default_screen             , 0 },
	{ "default_visual"            ,  3, default_visual             , 0 },
	{ "default_colormap"          ,  3, default_colormap           , 0 },

	/* Xft */
	{ "xft_color_alloc_name"      ,  5, xft_color_alloc_name       , 0 },

	/* XRandR */
	{ "xrr_query_extension"       ,  3, xrr_query_extension        , 0 },
	{ "xrr_select_input"          ,  3, xrr_select_input           , 0 },
	{ "xrr_get_screen_resources"  ,  4, xrr_get_screen_resources   , 0 }, /* also returns outputs directly */
	{ "xrr_free_screen_resources" ,  1, xrr_free_screen_resources  , 0 },
	{ "xrr_get_output_info"       ,  4, xrr_get_output_info        , 0 }, /* 3rd arg is output idx, last is list attrs */
	{ "xrr_get_crtc_info"         ,  4, xrr_get_crtc_info          , 0 }, /* returns list of useful fields only */

	{ "create_configure_event"    ,  3, create_configure_event     , 0 }, /* must be de-allocated with c_free! */
	{ "create_clientmessage_event",  6, create_clientmessage_event , 0 }, /* must be de-allocated with c_free! */

	{ "c_free"                    ,  1, c_free                     , 0 },
	{ NULL                        ,  0, NULL                       , 0 }
};

install_t
install(void)
{
	PL_register_extensions(predicates);
}

static foreign_t
x_open_display(term_t display_name, term_t display)
{
	char *dname;
	size_t len;
	Display *dp;

	PL_TRY(PL_get_string(display_name, &dname, &len));
	if (!(dp = XOpenDisplay(len == 1 ? NULL : dname))) {
		return (foreign_t)PL_warning("x_open_display/2: XOpenDisplay() failed!");
	}
	PL_TRY(PL_unify_pointer(display, dp));

	PL_succeed;
}

static foreign_t
x_close_display(term_t display)
{
	Display *dp;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));

	XCloseDisplay(dp);
	PL_succeed;
}

static foreign_t
x_set_error_handler(term_t to_dummy)
{
	Bool dummy;

	PL_TRY(PL_get_bool_ex(to_dummy, &dummy));
	if (dummy) {
		XSetErrorHandler((XErrorHandler)xerrordummy);
	}
	else {
		XSetErrorHandler((XErrorHandler)xerror);
	}
	PL_succeed;
}

static foreign_t
x_set_close_down_mode(term_t display, term_t close_mode)
{
	Display *dp;
	int closemode;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(close_mode, &closemode));

	XSetCloseDownMode(dp, closemode);
	PL_succeed;
}

static foreign_t
x_grab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window,
           term_t owner_events, term_t pointer_mode, term_t keyboard_mode)
{
	Display *dp;
	int kcode, mods, pmode, kmode;
	Window win;
	Bool oevents;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(keycode, &kcode));
	PL_TRY(PL_get_integer_ex(modifiers, &mods));
	PL_TRY(PL_get_uint64_ex(grab_window, &win));
	PL_TRY(PL_get_bool_ex(owner_events, &oevents));
	PL_TRY(PL_get_integer_ex(pointer_mode, &pmode));
	PL_TRY(PL_get_integer_ex(keyboard_mode, &kmode));

	XGrabKey(dp, kcode, (unsigned)mods, win, oevents, pmode, kmode);
	PL_succeed;
}

static foreign_t
x_grab_button(term_t display, term_t button, term_t modifiers, term_t grab_window,
              term_t owner_events, term_t event_mask, term_t pointer_mode,
              term_t keyboard_mode, term_t confine_to, term_t cursor)
{
	Display *dp;
	int btn, mods, emask, pmode, kmode;
	Window win, confto, crsr;
	Bool oevents;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(button, &btn));
	PL_TRY(PL_get_integer_ex(modifiers, &mods));
	PL_TRY(PL_get_uint64_ex(grab_window, &win));
	PL_TRY(PL_get_bool_ex(owner_events, &oevents));
	PL_TRY(PL_get_integer_ex(event_mask, &emask));
	PL_TRY(PL_get_integer_ex(pointer_mode, &pmode));
	PL_TRY(PL_get_integer_ex(keyboard_mode, &kmode));
	PL_TRY(PL_get_uint64_ex(confine_to, &confto));
	PL_TRY(PL_get_uint64_ex(cursor, &crsr));

	XGrabButton(dp, (unsigned)btn, (unsigned)mods, win, oevents, (unsigned)emask, pmode, kmode, confto, crsr);
	PL_succeed;
}

static foreign_t
x_grab_pointer(term_t display, term_t grab_window, term_t owner_events, term_t event_mask,
               term_t pointer_mode, term_t keyboard_mode, term_t confine_to, term_t cursor, term_t time)
{
	Display *dp;
	Window gwin, confto;
	Bool oevents;
	int emask, ptrmode, kbmode;
	Cursor csr;
	Time t;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(grab_window, &gwin));
	PL_TRY(PL_get_bool_ex(owner_events, &oevents));
	PL_TRY(PL_get_integer_ex(event_mask, &emask));
	PL_TRY(PL_get_integer_ex(pointer_mode, &ptrmode));
	PL_TRY(PL_get_integer_ex(keyboard_mode, &kbmode));
	PL_TRY(PL_get_uint64_ex(confine_to, &confto));
	PL_TRY(PL_get_uint64_ex(cursor, &csr));
	PL_TRY(PL_get_uint64_ex(time, &t));

	XGrabPointer(dp, gwin, oevents, (unsigned)event_mask, ptrmode, kbmode, confto, csr, t);
	PL_succeed;
}

static foreign_t
x_grab_server(term_t display)
{
	Display *dp;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	XGrabServer(dp);
	PL_succeed;
}

static foreign_t
x_ungrab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window)
{
	Display *dp;
	int kcode, mods;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(keycode, &kcode));
	PL_TRY(PL_get_integer_ex(modifiers, &mods));
	PL_TRY(PL_get_uint64_ex(grab_window, &win));

	XUngrabKey(dp, kcode, (unsigned)mods, win);
	PL_succeed;
}

static foreign_t
x_ungrab_button(term_t display, term_t button, term_t modifiers, term_t grab_window)
{
	Display *dp;
	int btn, mods;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(button, &btn));
	PL_TRY(PL_get_integer_ex(modifiers, &mods));
	PL_TRY(PL_get_uint64_ex(grab_window, &win));

	XUngrabButton(dp, (unsigned)btn, (unsigned)mods, win);
	PL_succeed;
}

static foreign_t
x_ungrab_pointer(term_t display, term_t time)
{
	Display *dp;
	Time t;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(time, &t));

	XUngrabPointer(dp, t);
	PL_succeed;
}

static foreign_t
x_ungrab_server(term_t display)
{
	Display *dp;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	XUngrabServer(dp);
	PL_succeed;
}

static foreign_t
x_keysym_to_keycode(term_t display, term_t keysym, term_t keycode)
{
	Display *dp;
	uint64_t ksym;
	KeyCode kcode;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(keysym, &ksym));

	kcode = XKeysymToKeycode(dp, ksym);

	PL_TRY(PL_unify_integer(keycode, kcode));
	PL_succeed;
}

static foreign_t
x_string_to_keysym(term_t string, term_t keysym)
{
	char *str;
	size_t len;
	uint64_t ksym;

	PL_TRY(PL_get_string_chars(string, &str, &len));

	ksym = XStringToKeysym(str);

	PL_TRY(PL_unify_uint64(keysym, ksym));
	PL_succeed;
}

static foreign_t
x_next_event(term_t display, term_t event_return)
{
	Display *dp;
	XEvent ev;
	term_t subts[18];
	term_t list;
	int stcnt;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));

	XNextEvent(dp, &ev);

	if (ev.type == MapRequest) {
		stcnt = 7;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "maprequest"));
		PL_TRY(PL_put_integer(subts[1], ev.xmaprequest.type      ));
		PL_TRY(PL_put_uint64 (subts[2], ev.xmaprequest.serial    ));
		PL_TRY(PL_put_bool   (subts[3], ev.xmaprequest.send_event));
		PL_TRY(PL_put_pointer(subts[4], ev.xmaprequest.display   ));
		PL_TRY(PL_put_uint64 (subts[5], ev.xmaprequest.parent    ));
		PL_TRY(PL_put_uint64 (subts[6], ev.xmaprequest.window    ));
	}
	else if (ev.type == UnmapNotify) {
		stcnt = 8;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "unmapnotify"));
		PL_TRY(PL_put_integer(subts[1], ev.xunmap.type          ));
		PL_TRY(PL_put_uint64 (subts[2], ev.xunmap.serial        ));
		PL_TRY(PL_put_bool   (subts[3], ev.xunmap.send_event    ));
		PL_TRY(PL_put_pointer(subts[4], ev.xunmap.display       ));
		PL_TRY(PL_put_uint64 (subts[5], ev.xunmap.event         ));
		PL_TRY(PL_put_uint64 (subts[6], ev.xunmap.window        ));
		PL_TRY(PL_put_bool   (subts[7], ev.xunmap.from_configure));
	}
	else if (ev.type == DestroyNotify) {
		stcnt = 7;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "destroynotify"));
		PL_TRY(PL_put_integer(subts[1], ev.xdestroywindow.type      ));
		PL_TRY(PL_put_uint64 (subts[2], ev.xdestroywindow.serial    ));
		PL_TRY(PL_put_bool   (subts[3], ev.xdestroywindow.send_event));
		PL_TRY(PL_put_pointer(subts[4], ev.xdestroywindow.display   ));
		PL_TRY(PL_put_uint64 (subts[5], ev.xdestroywindow.event     ));
		PL_TRY(PL_put_uint64 (subts[6], ev.xdestroywindow.window    ));
	}
	else if (ev.type == EnterNotify) {
		stcnt = 18;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "enternotify"));
		PL_TRY(PL_put_integer(subts[ 1], ev.xcrossing.type       ));
		PL_TRY(PL_put_uint64 (subts[ 2], ev.xcrossing.serial     ));
		PL_TRY(PL_put_bool   (subts[ 3], ev.xcrossing.send_event ));
		PL_TRY(PL_put_pointer(subts[ 4], ev.xcrossing.display    ));
		PL_TRY(PL_put_uint64 (subts[ 5], ev.xcrossing.window     ));
		PL_TRY(PL_put_uint64 (subts[ 6], ev.xcrossing.root       ));
		PL_TRY(PL_put_uint64 (subts[ 7], ev.xcrossing.subwindow  ));
		PL_TRY(PL_put_uint64 (subts[ 8], ev.xcrossing.time       ));
		PL_TRY(PL_put_integer(subts[ 9], ev.xcrossing.x          ));
		PL_TRY(PL_put_integer(subts[10], ev.xcrossing.y          ));
		PL_TRY(PL_put_integer(subts[11], ev.xcrossing.x_root     ));
		PL_TRY(PL_put_integer(subts[12], ev.xcrossing.y_root     ));
		PL_TRY(PL_put_integer(subts[13], ev.xcrossing.mode       ));
		PL_TRY(PL_put_integer(subts[14], ev.xcrossing.detail     ));
		PL_TRY(PL_put_bool   (subts[15], ev.xcrossing.same_screen));
		PL_TRY(PL_put_bool   (subts[16], ev.xcrossing.focus      ));
		PL_TRY(PL_put_integer(subts[17], ev.xcrossing.state      ));
	}
	else if (ev.type == PropertyNotify) {
		stcnt = 8;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "propertynotify"));
		PL_TRY(PL_put_uint64 (subts[1], ev.xproperty.serial    ));
		PL_TRY(PL_put_bool   (subts[2], ev.xproperty.send_event));
		PL_TRY(PL_put_pointer(subts[3], ev.xproperty.display   ));
		PL_TRY(PL_put_uint64 (subts[4], ev.xproperty.window    ));
		PL_TRY(PL_put_uint64 (subts[5], ev.xproperty.atom      ));
		PL_TRY(PL_put_uint64 (subts[6], ev.xproperty.time      ));
		PL_TRY(PL_put_integer(subts[7], ev.xproperty.state     ));
	}
	else if (ev.type == ClientMessage) {
		stcnt = 11;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "clientmessage"));
		PL_TRY(PL_put_integer(subts[ 1], ev.xclient.type               ));
		PL_TRY(PL_put_uint64 (subts[ 2], ev.xclient.serial             ));
		PL_TRY(PL_put_bool   (subts[ 3], ev.xclient.send_event         ));
		PL_TRY(PL_put_pointer(subts[ 4], ev.xclient.display            ));
		PL_TRY(PL_put_uint64 (subts[ 5], ev.xclient.window             ));
		PL_TRY(PL_put_uint64 (subts[ 6], ev.xclient.message_type       ));
		PL_TRY(PL_put_integer(subts[ 7], ev.xclient.format             ));
		PL_TRY(PL_put_uint64 (subts[ 8], (unsigned)ev.xclient.data.l[0])); /* we are lazy and only handle      */
		PL_TRY(PL_put_uint64 (subts[ 9], (unsigned)ev.xclient.data.l[1])); /* the data were are interested in */
		PL_TRY(PL_put_uint64 (subts[10], (unsigned)ev.xclient.data.l[2]));
	}
	else if (ev.type == ConfigureNotify) {
		stcnt = 14;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY(PL_put_atom_chars(subts[0], "configurenotify"));
		PL_TRY(PL_put_integer(subts[ 1], ev.xconfigure.type             ));
		PL_TRY(PL_put_uint64 (subts[ 2], ev.xconfigure.serial           ));
		PL_TRY(PL_put_bool   (subts[ 3], ev.xconfigure.send_event       ));
		PL_TRY(PL_put_pointer(subts[ 4], ev.xconfigure.display          ));
		PL_TRY(PL_put_uint64 (subts[ 5], ev.xconfigure.event            ));
		PL_TRY(PL_put_uint64 (subts[ 6], ev.xconfigure.window           ));
		PL_TRY(PL_put_integer(subts[ 7], ev.xconfigure.x                ));
		PL_TRY(PL_put_integer(subts[ 8], ev.xconfigure.y                ));
		PL_TRY(PL_put_integer(subts[ 9], ev.xconfigure.width            ));
		PL_TRY(PL_put_integer(subts[10], ev.xconfigure.height           ));
		PL_TRY(PL_put_integer(subts[11], ev.xconfigure.border_width     ));
		PL_TRY(PL_put_uint64 (subts[12], ev.xconfigure.above            ));
		PL_TRY(PL_put_bool   (subts[13], ev.xconfigure.override_redirect));
	}
	/* XRandR */
	else if (ev.type == rr_event_base + RRScreenChangeNotify) {
		stcnt = 1;
		subts[0] = PL_new_term_ref();
		PL_TRY(PL_put_atom_chars(subts[0], "rrscreenchangenotify"));
	}
	else {
		/* rest of the event types are very similar */
		stcnt = 15;
		for (int i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		switch (ev.type) {
		case KeyPress:
			PL_TRY(PL_put_atom_chars(subts[0], "keypress"));
			PL_TRY(PL_put_integer   (subts[13], ev.xkey.keycode));
			break;
		case KeyRelease:
			PL_TRY(PL_put_atom_chars(subts[0], "keyrelease"));
			PL_TRY(PL_put_integer   (subts[13], ev.xkey.keycode));
			break;
		case ButtonPress:
			PL_TRY(PL_put_atom_chars(subts[0], "buttonpress"));
			PL_TRY(PL_put_integer   (subts[13], ev.xbutton.button));
			break;
		case ButtonRelease:
			PL_TRY(PL_put_atom_chars(subts[0], "buttonrelease"));
			PL_TRY(PL_put_integer   (subts[13], ev.xbutton.button));
			break;
		case MotionNotify:
			PL_TRY(PL_put_atom_chars(subts[0], "motionnotify"));
			PL_TRY(PL_put_integer   (subts[13], ev.xmotion.is_hint));
			break;
		default:
			PL_TRY(PL_unify_atom_chars(event_return, "unsupported_event"));
			PL_succeed;
		}

		/* common fields */
		PL_TRY(PL_put_uint64 (subts[ 1], ev.xkey.serial     ));
		PL_TRY(PL_put_bool   (subts[ 2], ev.xkey.send_event ));
		PL_TRY(PL_put_pointer(subts[ 3], ev.xkey.display    ));
		PL_TRY(PL_put_uint64 (subts[ 4], ev.xkey.window     ));
		PL_TRY(PL_put_uint64 (subts[ 5], ev.xkey.root       ));
		PL_TRY(PL_put_uint64 (subts[ 6], ev.xkey.subwindow  ));
		PL_TRY(PL_put_uint64 (subts[ 7], ev.xkey.time       ));
		PL_TRY(PL_put_integer(subts[ 8], ev.xkey.x          ));
		PL_TRY(PL_put_integer(subts[ 9], ev.xkey.y          ));
		PL_TRY(PL_put_integer(subts[10], ev.xkey.x_root     ));
		PL_TRY(PL_put_integer(subts[11], ev.xkey.y_root     ));
		PL_TRY(PL_put_integer(subts[12], ev.xkey.state      ));
		PL_TRY(PL_put_bool   (subts[14], ev.xkey.same_screen));
	}

	list = PL_new_term_ref();
	build_list(list, subts, (size_t)stcnt);
	PL_TRY(PL_unify(list, event_return));
	PL_succeed;
}

static foreign_t
x_send_event(term_t display, term_t w, term_t propagate, term_t event_mask, term_t event_send)
{
	Display *dp;
	Window win;
	Bool propag;
	long emask;
	XEvent *esend;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_bool_ex(propagate, &propag));
	PL_TRY(PL_get_long_ex(event_mask, &emask));
	PL_TRY(PL_get_pointer_ex(event_send, (void**)&esend));

	XSendEvent(dp, win, propag, emask, esend);
	PL_succeed;
}

static foreign_t
x_raise_window(term_t display, term_t w)
{
	Display *dp;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	XRaiseWindow(dp, w);
	PL_succeed;
}

static foreign_t
x_get_window_attributes(term_t display, term_t w, term_t window_attributes_return, term_t status)
{
	/*
	Note: this doesn't extract all win attributes, only the ones needed!
	x, y, width and height for now
	*/
	Display *dp;
	Window win;
	XWindowAttributes winattrs;
	term_t subts[4];
	term_t list = PL_new_term_ref();
	Status st;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	st = XGetWindowAttributes(dp, win, &winattrs);

	for (int i = 0; i < 4; ++i) {
		subts[i] = PL_new_term_ref();
	}
	PL_TRY(PL_put_integer(subts[0], winattrs.x));
	PL_TRY(PL_put_integer(subts[1], winattrs.y));
	PL_TRY(PL_put_integer(subts[2], winattrs.width));
	PL_TRY(PL_put_integer(subts[3], winattrs.height));

	build_list(list, subts, 4);
	PL_TRY(PL_unify(list, window_attributes_return));
	PL_TRY(PL_unify_integer(status, st));
	PL_succeed;
}

static foreign_t
x_move_resize_window(term_t display, term_t w, term_t x, term_t y, term_t width, term_t height)
{
	Display *dp;
	Window win;
	int wx, wy, wwidth, wheight;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_integer_ex(x, &wx));
	PL_TRY(PL_get_integer_ex(y, &wy));
	PL_TRY(PL_get_integer_ex(width, &wwidth));
	PL_TRY(PL_get_integer_ex(height, &wheight));

	XMoveResizeWindow(dp, win, wx, wy, (unsigned)wwidth, (unsigned)wheight);
	PL_succeed;
}

static foreign_t
x_change_window_attributes(term_t display, term_t w, term_t valuemask, term_t event_mask)
{
	Display *dp;
	Window win, vmask;
	long emask;
	XSetWindowAttributes wa;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(valuemask, &vmask));
	PL_TRY(PL_get_long_ex(event_mask, &emask));

	wa.event_mask = emask;
	XChangeWindowAttributes(dp, win, vmask, &wa);
	PL_succeed;
}

static foreign_t
x_select_input(term_t display, term_t w, term_t event_mask)
{
	Display *dp;
	long emask;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_long_ex(event_mask, &emask));
	PL_TRY(PL_get_uint64_ex(w, &win));

	XSelectInput(dp, win, emask);
	PL_succeed;
}

static foreign_t
x_map_window(term_t display, term_t w)
{
	Display *dp;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	XMapWindow(dp, win);
	PL_succeed;
}

static foreign_t
x_configure_window(term_t display, term_t w, term_t value_mask, term_t x, term_t y, term_t width,
		   term_t height, term_t border_width, term_t sibling, term_t stack_mode)
{
	Display *dp;
	Window win;
	int vmask, wx, wy, wwidth, wheight, bw, stackm;
	Window sib;
	XWindowChanges wc;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_integer_ex(value_mask, &vmask));
	PL_TRY(PL_get_integer_ex(x, &wx));
	PL_TRY(PL_get_integer_ex(y, &wy));
	PL_TRY(PL_get_integer_ex(width, &wwidth));
	PL_TRY(PL_get_integer_ex(height, &wheight));
	PL_TRY(PL_get_integer_ex(border_width, &bw));
	PL_TRY(PL_get_uint64_ex(sibling, &sib));
	PL_TRY(PL_get_integer_ex(stack_mode, &stackm));
	wc.x = wx; wc.y = wy; wc.width = wwidth; wc.height = wheight;
	wc.border_width = bw; wc.sibling = sib; wc.stack_mode = stackm;

	XConfigureWindow(dp, win, (unsigned)vmask, &wc);
	PL_succeed;
}

static foreign_t
x_set_window_border(term_t display, term_t w, term_t border_pixel)
{
	Display *dp;
	Window win;
	unsigned long bp;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(border_pixel, &bp));

	XSetWindowBorder(dp, win, bp);
	PL_succeed;
}

static foreign_t
x_set_input_focus(term_t display, term_t focus, term_t revert_to, term_t time)
{
	Display *dp;
	Window win, tim;
	int revto;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(focus, &win));
	PL_TRY(PL_get_integer_ex(revert_to, &revto));
	PL_TRY(PL_get_uint64_ex(time, &tim));

	XSetInputFocus(dp, win, revto, tim);
	PL_succeed;
}

static foreign_t
x_kill_client(term_t display, term_t resource)
{
	Display *dp;
	uint64_t res;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(resource, &res));

	XKillClient(dp, res);
	PL_succeed;
}

static foreign_t
x_sync(term_t display, term_t discard)
{
	Display *dp;
	Bool discrd;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_bool_ex(discard, &discrd));

	XSync(dp, discrd);
	PL_succeed;
}

static foreign_t
x_intern_atom(term_t display, term_t atom_name, term_t only_if_exists, term_t atom)
{
	Display *dp;
	char *aname; size_t len;
	Bool ifexists;
	Atom a;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_string_chars(atom_name, &aname, &len));
	PL_TRY(PL_get_bool_ex(only_if_exists, &ifexists));

	a = XInternAtom(dp, aname, ifexists);

	PL_TRY(PL_unify_uint64(atom, a));
	PL_succeed;
}

static foreign_t
x_get_class_hint(term_t display, term_t w, term_t res_name, term_t res_class)
{
	Display *dp;
	Window win;
	XClassHint ch = { NULL, NULL };

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	if (XGetClassHint(dp, win, &ch)) {
		PL_TRY(PL_unify_string_chars(res_name , ch.res_name  ? ch.res_name  : ""));
		PL_TRY(PL_unify_string_chars(res_class, ch.res_class ? ch.res_class : ""));
	} else {
		PL_fail;
	}

	if (ch.res_class) XFree(ch.res_class);
	if (ch.res_name)  XFree(ch.res_name);
	PL_succeed;
}

static foreign_t
x_change_property(term_t display, term_t w, term_t property, term_t atom, term_t format,
                                   term_t mode, term_t data, term_t nelements)
{
	Display *dp;
	Window win;
	Atom prop, a;
	int fmt, md, nelem;
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(data);
	uint64_t *alist, i = 0;
	char *str; size_t len;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(property, &prop));
	PL_TRY(PL_get_uint64_ex(atom, &a));
	PL_TRY(PL_get_integer_ex(format, &fmt));
	PL_TRY(PL_get_integer_ex(mode, &md));
	PL_TRY(PL_get_integer_ex(nelements, &nelem));

	if (PL_is_string(data)) { /* handle the UTF8_STRING case separately */
		PL_TRY(PL_get_string_chars(data, &str, &len));
		XChangeProperty(dp, win, prop, a, fmt, md, (unsigned char *)str, (int)len);
		PL_succeed;
	}

	alist = malloc((size_t)nelem * sizeof(*alist));
	while (PL_get_list(list, head, list)) {
		if (!PL_get_uint64_ex(head, &alist[i++])) {
			free(alist);
			return (foreign_t)PL_warning("x_change_property/8: PL_get_uint64_ex() on 'data[i]' failed!");
		}
	}
	XChangeProperty(dp, win, prop, a, fmt, md, (unsigned char *)alist, nelem);
	free(alist);
	PL_succeed;
}

static foreign_t
x_delete_property(term_t display, term_t w, term_t property)
{
	Display *dp;
	Window win;
	Atom prop;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(property, &prop));

	XDeleteProperty(dp, win, prop);
	PL_succeed;
}

static foreign_t
x_utf8_text_list_to_text_property(term_t display, term_t list, term_t count, term_t style, term_t text_prop_return)
{
	Display *dp;
	char **strs;
	int cnt, sty, i = 0;
	XTextProperty *tprop = NULL;
	term_t head = PL_new_term_ref();
	term_t tlist = PL_copy_term_ref(list);

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(count, &cnt));
	PL_TRY(PL_get_integer_ex(style, &sty));

	tprop = malloc(sizeof(*tprop));
	strs = malloc((size_t)cnt * sizeof(*strs));
	while (PL_get_list(tlist, head, tlist)) {
		if (!PL_get_chars(head, &strs[i++], CVT_ALL|REP_UTF8)) {
			free(strs); free(tprop);
			return (foreign_t)PL_warning("x_utf8_text_list_to_text_property/5: PL_get_chars() on 'list[i]' failed!");
		}
	}
	Xutf8TextListToTextProperty(dp, strs, cnt, (XICCEncodingStyle)sty, tprop);
	free(strs);

	/* Note: tprop must be freed with c_free/1 after no longer needed! */
	PL_TRY(PL_unify_pointer(text_prop_return, tprop));
	PL_succeed;
}

static foreign_t
x_get_text_property(term_t display, term_t w, term_t text, term_t property, term_t status)
{
	Display *dp;
	Window win;
	XTextProperty tprop;
	Atom prop;
	Status st;
	static char stext[256];

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(property, &prop));

	st = XGetTextProperty(dp, win, &tprop, prop);

	if (st != 0) {
		strncpy(stext, (char*)tprop.value, sizeof(stext)-1);
	}
	else {
		stext[0] = '\0';
	}
	PL_TRY(PL_unify_integer(status, st));
	PL_TRY(PL_unify_string_chars(text, stext));
	PL_succeed;
}

static foreign_t
x_set_text_property(term_t display, term_t w, term_t text_prop, term_t property)
{
	Display *dp;
	Window win;
	XTextProperty *tprop;
	Atom prop;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_pointer_ex(text_prop, (void**)&tprop));
	PL_TRY(PL_get_uint64_ex(property, &prop));

	XSetTextProperty(dp, win, tprop, prop);
	PL_succeed;
}

static foreign_t
x_create_simple_window(term_t display, term_t parent, term_t x, term_t y, term_t width,
                       term_t height, term_t border_width, term_t border, term_t background, term_t w)
{
	Display *dp;
	Window pwin, win;
	int wx, wy, ww, wh, bw;
	uint64_t brd, bg;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(parent, &pwin));
	PL_TRY(PL_get_integer_ex(x, &wx));
	PL_TRY(PL_get_integer_ex(y, &wy));
	PL_TRY(PL_get_integer_ex(width, &ww));
	PL_TRY(PL_get_integer_ex(height, &wh));
	PL_TRY(PL_get_integer_ex(border_width, &bw));
	PL_TRY(PL_get_uint64_ex(border, &brd));
	PL_TRY(PL_get_uint64_ex(background, &bg));

	win = XCreateSimpleWindow(dp, pwin, wx, wy, (unsigned)ww, (unsigned)wh, (unsigned)bw, brd, bg);

	PL_TRY(PL_unify_uint64(w, win));
	PL_succeed;
}

static foreign_t
x_get_transient_for_hint(term_t display, term_t w, term_t prop_window_return, term_t status)
{
	Display *dp;
	Window win, wret;
	Status st;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	st = XGetTransientForHint(dp, win, &wret);

	PL_TRY(PL_unify_uint64(prop_window_return, wret));
	PL_TRY(PL_unify_integer(status, st));
	PL_succeed;
}

static foreign_t
x_get_window_property(term_t display, term_t w, term_t property, term_t delete, term_t req_type, term_t prop_return)
{
	/* Note: this is similar to dwm's getatomprop() */
	Display *dp;
	Window win;
	Atom prop, rqtyp, propret = None;
	Bool del;
	Atom atr; int afr; unsigned long nr, bar; /* unused */
	unsigned char *p = NULL;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(property, &prop));
	PL_TRY(PL_get_bool_ex(delete, &del));
	PL_TRY(PL_get_uint64_ex(req_type, &rqtyp));

	if (XGetWindowProperty(dp, win, prop, 0L, sizeof(Atom), del, rqtyp, &atr, &afr, &nr, &bar, &p) == Success && p) {
		propret = *(Atom*)p;
		XFree(p);
	}

	PL_TRY(PL_unify_uint64(prop_return, propret));
	PL_succeed;
}

static foreign_t
x_get_wm_protocols(term_t display, term_t w, term_t protocols_return, term_t count_return)
{
	Display *dp;
	Window win;
	Atom *protocols;
	int cnt;
	term_t listt = PL_new_term_ref();

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	if (XGetWMProtocols(dp, win, &protocols, &cnt) != 0) {
		if (cnt == 0) {
			PL_TRY(PL_unify_nil_ex(protocols_return));
			PL_TRY(PL_unify_integer(count_return, 0));
			PL_succeed;
		}

		term_t *subts = malloc((size_t)cnt * sizeof(term_t));
		for (int i = 0; i < cnt; ++i) {
			subts[i] = PL_new_term_ref();
			PL_TRY(PL_put_uint64(subts[i], protocols[i]), XFree(protocols); free(subts));
		}
		build_list(listt, subts, (size_t)cnt);

		PL_TRY(PL_unify(listt, protocols_return), XFree(protocols); free(subts));
		PL_TRY(PL_unify_integer(count_return, cnt), XFree(protocols); free(subts));

		XFree(protocols);
		free(subts);
		PL_succeed;
	}
	PL_fail;
}

static foreign_t
x_get_wm_normal_hints(term_t display, term_t w, term_t hints_return, term_t status)
{
	Display *dp;
	Window win;
	XSizeHints hints;
	long supret;
	Status st;
	term_t subts[18];
	term_t list = PL_new_term_ref();

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	st = XGetWMNormalHints(dp, win, &hints, &supret); /* supplied_return arg is ignored */

	if (st) {
		for (int i = 0; i < 18; ++i) {
			subts[i] = PL_new_term_ref();
		}
		PL_TRY(PL_put_uint64 (subts[ 0], (unsigned)hints.flags));
		PL_TRY(PL_put_integer(subts[ 1], hints.x));
		PL_TRY(PL_put_integer(subts[ 2], hints.y));
		PL_TRY(PL_put_integer(subts[ 3], hints.width));
		PL_TRY(PL_put_integer(subts[ 4], hints.height));
		PL_TRY(PL_put_integer(subts[ 5], hints.min_width));
		PL_TRY(PL_put_integer(subts[ 6], hints.min_height));
		PL_TRY(PL_put_integer(subts[ 7], hints.max_width));
		PL_TRY(PL_put_integer(subts[ 8], hints.max_height));
		PL_TRY(PL_put_integer(subts[ 9], hints.width_inc));
		PL_TRY(PL_put_integer(subts[10], hints.height_inc));
		PL_TRY(PL_put_integer(subts[11], hints.min_aspect.x));
		PL_TRY(PL_put_integer(subts[12], hints.min_aspect.y));
		PL_TRY(PL_put_integer(subts[13], hints.max_aspect.x));
		PL_TRY(PL_put_integer(subts[14], hints.max_aspect.y));
		PL_TRY(PL_put_integer(subts[15], hints.base_width));
		PL_TRY(PL_put_integer(subts[16], hints.base_height));
		PL_TRY(PL_put_integer(subts[17], hints.win_gravity));
		build_list(list, subts, 18);

		PL_TRY(PL_unify(list, hints_return));
	}
	else {
		PL_TRY(PL_unify_nil_ex(hints_return));
	}

	PL_TRY(PL_unify_integer(status, st));
	PL_succeed;
}

static foreign_t
x_warp_pointer(term_t display, term_t src_w, term_t dest_w, term_t src_x, term_t src_y,
                                term_t src_width, term_t src_height, term_t dest_x, term_t dest_y)
{
	Display *dp;
	Window swin, dwin;
	int sx, sy, sw, sh, dx, dy;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(src_w, &swin));
	PL_TRY(PL_get_uint64_ex(dest_w, &dwin));
	PL_TRY(PL_get_integer_ex(src_x, &sx));
	PL_TRY(PL_get_integer_ex(src_y, &sy));
	PL_TRY(PL_get_integer_ex(src_width, &sw));
	PL_TRY(PL_get_integer_ex(src_height, &sh));
	PL_TRY(PL_get_integer_ex(dest_x, &dx));
	PL_TRY(PL_get_integer_ex(dest_y, &dy));

	XWarpPointer(dp, swin, dwin, sx, sy, (unsigned)sw, (unsigned)sh, dx, dy);
	PL_succeed;
}

static foreign_t
default_root_window(term_t display, term_t w)
{
	Display *dp;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));

	win = DefaultRootWindow(dp);

	PL_TRY(PL_unify_uint64(w, win));
	PL_succeed;
}

static foreign_t
default_screen(term_t display, term_t screen)
{
	Display *dp;
	int scr;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));

	scr = DefaultScreen(dp);

	PL_TRY(PL_unify_integer(screen, scr));
	PL_succeed;
}

static foreign_t
default_visual(term_t display, term_t screen_number, term_t visual)
{
	Display *dp;
	int scrnum;
	Visual *vis;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(screen_number, &scrnum));

	vis = DefaultVisual(dp, scrnum);

	PL_TRY(PL_unify_pointer(visual, vis));

	PL_succeed;
}

static foreign_t
default_colormap(term_t display, term_t screen_number, term_t colormap)
{
	Display *dp;
	int scrnum;
	Colormap cmap;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_integer_ex(screen_number, &scrnum));

	cmap = DefaultColormap(dp, scrnum);

	PL_TRY(PL_unify_uint64(colormap, cmap));

	PL_succeed;
}

static foreign_t
xft_color_alloc_name(term_t display, term_t visual, term_t cmap, term_t name, term_t result)
{
	Display *dp;
	Visual *vis;
	uint64_t cm;
	size_t len;
	char *nam;
	XftColor res;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_pointer_ex(visual, (void**)&vis));
	PL_TRY(PL_get_uint64_ex(cmap, &cm));
	PL_TRY(PL_get_string(name, &nam, &len));

	if (!XftColorAllocName(dp, vis, cm, nam, &res)) {
		return (foreign_t)PL_warning("xft_color_alloc_name/5: XftColorAllocName() failed!");
	}

	PL_TRY(PL_unify_uint64(result, res.pixel));
	PL_succeed;
}

static foreign_t
xrr_query_extension(term_t dpy, term_t event_base_return, term_t error_base_return)
{
	Display *dp;
	int eventbase, errorbase;

	PL_TRY(PL_get_pointer_ex(dpy, (void**)&dp));

	if (XRRQueryExtension(dp, &eventbase, &errorbase)) {
		PL_TRY(PL_unify_integer(event_base_return, eventbase));
		PL_TRY(PL_unify_integer(error_base_return, errorbase));
		rr_event_base = eventbase; /* shortcut: it's simpler to keep this in plx */
		PL_succeed;
	}
	else {
		return (foreign_t)PL_warning("xrr_query_extension: XRRQueryExtension() failed!");
	}
}

static foreign_t xrr_select_input(term_t dpy, term_t window, term_t mask)
{
	Display *dp;
	Window win;
	int msk;

	PL_TRY(PL_get_pointer_ex(dpy, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(window, &win));
	PL_TRY(PL_get_integer_ex(mask, &msk));

	XRRSelectInput(dp, win, msk);
	PL_succeed;
}

static foreign_t
xrr_get_screen_resources(term_t dpy, term_t window, term_t screen_resources, term_t outputs)
{
	Display *dp;
	Window win;
	XRRScreenResources *scrres;
	term_t listt = PL_new_term_ref();

	PL_TRY(PL_get_pointer_ex(dpy, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(window, &win));

	if ((scrres = XRRGetScreenResources(dp, win)) == NULL) {
		return (foreign_t)PL_warning("xrr_get_screen_resources: XRRGetScreenResources() returned NULL!");
	}

	PL_TRY(PL_unify_pointer(screen_resources, scrres), XRRFreeScreenResources(scrres));

	if (scrres->noutput == 0) {
		PL_TRY(PL_unify_nil_ex(outputs), XRRFreeScreenResources(scrres));
	}
	else {
		term_t *subts = malloc((size_t)scrres->noutput * sizeof(term_t));
		for (int i = 0; i < scrres->noutput; ++i) {
			subts[i] = PL_new_term_ref();
			PL_TRY(PL_put_uint64(subts[i], scrres->outputs[i]), XRRFreeScreenResources(scrres); free(subts));
		}
		build_list(listt, subts, (size_t)scrres->noutput);
		PL_TRY(PL_unify(listt, outputs), XRRFreeScreenResources(scrres); free(subts));
		free(subts);
	}
	PL_succeed;
}

static foreign_t
xrr_free_screen_resources(term_t resources)
{
	XRRScreenResources *scrres;

	PL_TRY(PL_get_pointer_ex(resources, (void**)&scrres));
	XRRFreeScreenResources(scrres);
	PL_succeed;
}

static foreign_t
xrr_get_output_info(term_t dpy, term_t resources, term_t output_index, term_t output_info)
{
	Display *dp;
	XRRScreenResources *scrres;
	int oidx;
	XRROutputInfo *oinfo;

	PL_TRY(PL_get_pointer_ex(dpy, (void**)&dp));
	PL_TRY(PL_get_pointer_ex(resources, (void**)&scrres));
	PL_TRY(PL_get_integer_ex(output_index, &oidx));

	if (oidx < 0 || scrres->noutput <= oidx) {
		return (foreign_t)PL_warning("xrr_get_output_info: output_index: %d is out of bounds: 0..%d!",
		                             oidx, scrres->noutput);
	}

	if ((oinfo = XRRGetOutputInfo(dp, scrres, scrres->outputs[oidx])) == NULL) {
		return (foreign_t)PL_warning("xrr_get_output_info: XRRGetOutputInfo() returned NULL!");
	}

	term_t name = PL_new_term_ref();
	term_t connection = PL_new_term_ref();;
	term_t crtc = PL_new_term_ref();;
	PL_TRY(PL_put_string_chars(name, oinfo->name),        XRRFreeOutputInfo(oinfo));
	PL_TRY(PL_put_integer(connection, oinfo->connection), XRRFreeOutputInfo(oinfo));
	PL_TRY(PL_put_uint64(crtc, oinfo->crtc),              XRRFreeOutputInfo(oinfo));

	term_t tlist = PL_new_term_ref();
	PL_TRY(PL_put_nil(tlist),                      XRRFreeOutputInfo(oinfo));
	PL_TRY(PL_cons_list(tlist, crtc, tlist),       XRRFreeOutputInfo(oinfo));
	PL_TRY(PL_cons_list(tlist, connection, tlist), XRRFreeOutputInfo(oinfo));
	PL_TRY(PL_cons_list(tlist, name, tlist),       XRRFreeOutputInfo(oinfo));

	PL_TRY(PL_unify(output_info, tlist), XRRFreeOutputInfo(oinfo));

	XRRFreeOutputInfo(oinfo);
	PL_succeed;
}

static foreign_t
xrr_get_crtc_info(term_t dpy, term_t resources, term_t crtc, term_t crtc_info)
{
	Display *dp;
	XRRScreenResources *scrres;
	XRRCrtcInfo *crtcinfo;
	RRCrtc rrcrtc;

	PL_TRY(PL_get_pointer_ex(dpy, (void**)&dp));
	PL_TRY(PL_get_pointer_ex(resources, (void**)&scrres));
	PL_TRY(PL_get_uint64_ex(crtc, &rrcrtc));
	
	if ((crtcinfo = XRRGetCrtcInfo(dp, scrres, rrcrtc)) == NULL) {
		return (foreign_t)PL_warning("xrr_get_crtc_info: XRRGetCrtcInfo() returned NULL!");
	}

	term_t x = PL_new_term_ref();
	term_t y = PL_new_term_ref();
	term_t w = PL_new_term_ref();
	term_t h = PL_new_term_ref();
	PL_TRY(PL_put_integer(x, crtcinfo->x),      XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_put_integer(y, crtcinfo->y),      XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_put_integer(w, crtcinfo->width),  XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_put_integer(h, crtcinfo->height), XRRFreeCrtcInfo(crtcinfo));

	term_t tlist = PL_new_term_ref();
	PL_TRY(PL_put_nil(tlist),             XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_cons_list(tlist, h, tlist), XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_cons_list(tlist, w, tlist), XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_cons_list(tlist, y, tlist), XRRFreeCrtcInfo(crtcinfo));
	PL_TRY(PL_cons_list(tlist, x, tlist), XRRFreeCrtcInfo(crtcinfo));

	PL_TRY(PL_unify(crtc_info, tlist), XRRFreeCrtcInfo(crtcinfo));

	XRRFreeCrtcInfo(crtcinfo);
	PL_succeed;
}

static foreign_t
create_configure_event(term_t display, term_t w, term_t configure_event)
{
	Display *dp;
	Window win;

	PL_TRY(PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY(PL_get_uint64_ex(w, &win));

	XConfigureEvent *event = malloc(sizeof(XConfigureEvent));
	if (event != NULL) {
		memset(event, 0, sizeof(XConfigureEvent));
		event->type = ConfigureNotify;
		event->display = dp;
		event->event = win;
		event->window = win;
		PL_TRY(PL_unify_pointer(configure_event, event));
		PL_succeed;
	}
	PL_fail;
}

static foreign_t
create_clientmessage_event(term_t w, term_t message_type, term_t format, term_t datal0, term_t datal1,
                           term_t clientmessage)
{
	Window win;
	Atom msgtype;
	int fmt;
	long datal[2];

	PL_TRY(PL_get_uint64_ex(w, &win));
	PL_TRY(PL_get_uint64_ex(message_type, &msgtype));
	PL_TRY(PL_get_integer_ex(format, &fmt));
	PL_TRY(PL_get_long_ex(datal0, datal));
	PL_TRY(PL_get_long_ex(datal1, datal + 1));

	XEvent *event = malloc(sizeof(XEvent));
	if (event != NULL) {
		memset(event, 0, sizeof(XEvent));
		event->type = ClientMessage;
		event->xclient.window = win;
		event->xclient.message_type = msgtype;
		event->xclient.format = fmt;
		event->xclient.data.l[0] = datal[0];
		event->xclient.data.l[1] = datal[1];
		PL_TRY(PL_unify_pointer(clientmessage, event));
		PL_succeed;
	}
	PL_fail;
}

static foreign_t
c_free(term_t ptr)
{
	void *p;

	PL_TRY(PL_get_pointer_ex(ptr, &p));
	
	free(p);
	PL_succeed;
}

static int
build_list(term_t dst, term_t *src, size_t size)
{
	PL_unify_nil_ex(dst);
	for (size_t i = size; 0 < i; --i) {
		if (!PL_cons_list(dst, src[i - 1], dst)) {
			return 0;
		}
	}
	return 1;
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
	fprintf(stderr, "plx: fatal error: request code=%d, error code=%d\n",
		ee->request_code, ee->error_code);
	return -1;
}

static int
xerrordummy(const Display __attribute__((unused)) *dpy, const XErrorEvent __attribute__((unused)) *ee)
{
	/* copied from dwm */
	return 0;
}

