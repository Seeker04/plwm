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
#include <X11/extensions/Xinerama.h>

#include <SWI-Prolog.h>

#define Q(x) #x
#define QUOTE(x) Q(x)

#define PL_TRY2(func, plcall) PL_TRY3(func, plcall, {})
#define PL_TRY3(func, plcall, cleanup) \
	if (!(plcall)) { cleanup; return (foreign_t)PL_warning(func ":" QUOTE((plcall)) " failed!"); }

#define GET_MACRO(_1,_2,_3,NAME,...) NAME
#define PL_TRY(...) GET_MACRO(__VA_ARGS__, PL_TRY3, PL_TRY2)(__VA_ARGS__)

/* Bindings to swipl predicates */
static foreign_t x_open_display(term_t display_name, term_t display);
static foreign_t x_close_display(term_t display);
static foreign_t x_set_error_handler(void);
static foreign_t x_grab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window,
                            term_t owner_events, term_t pointer_mode, term_t keyboard_mode);
static foreign_t x_grab_button(term_t display, term_t button, term_t modifiers, term_t grab_window,
                               term_t owner_events, term_t event_mask, term_t pointer_mode,
                               term_t keyboard_mode, term_t confine_to, term_t cursor);
static foreign_t x_grab_pointer(term_t display, term_t grab_window, term_t owner_events, term_t event_mask,
                                term_t pointer_mode, term_t keyboard_mode, term_t confine_to, term_t cursor, term_t time);
static foreign_t x_ungrab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window);
static foreign_t x_ungrab_button(term_t display, term_t button, term_t modifiers, term_t grab_window);
static foreign_t x_ungrab_pointer(term_t display, term_t time);
static foreign_t x_keysym_to_keycode(term_t display, term_t keysym, term_t keycode);
static foreign_t x_string_to_keysym(term_t string, term_t keysym);
static foreign_t x_next_event(term_t display, term_t event_return);
static foreign_t x_raise_window(term_t display, term_t w);
static foreign_t x_get_window_attributes(term_t display, term_t w, term_t window_attributes_return, term_t status);
static foreign_t x_move_resize_window(term_t display, term_t w, term_t x, term_t y, term_t width, term_t height);
static foreign_t x_change_window_attributes(term_t display, term_t w, term_t valuemask, term_t attributes);
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
static foreign_t x_get_transient_for_hint(term_t display, term_t w, term_t prop_window_return);
static foreign_t x_get_window_property(term_t display, term_t w, term_t property, term_t delete, term_t req_type,
                                       term_t prop_return);
static foreign_t x_get_wm_normal_hints(term_t display, term_t w, term_t hints_return, term_t status);
static foreign_t x_warp_pointer(term_t display, term_t src_w, term_t dest_w, term_t src_x, term_t src_y,
                                term_t src_width, term_t src_height, term_t dest_x, term_t dest_y);

static foreign_t default_root_window(term_t display, term_t w);
static foreign_t default_screen(term_t display, term_t screen);
static foreign_t default_visual(term_t display, term_t screen_number, term_t visual);
static foreign_t default_colormap(term_t display, term_t screen_number, term_t colormap);

static foreign_t xinerama_query_screens(term_t display, term_t screen_info);
static foreign_t xft_color_alloc_name(term_t display, term_t visual, term_t cmap, term_t name, term_t result);

static foreign_t c_free(term_t ptr);

/* Helpers */
static int build_list(term_t dst, term_t *src, size_t size);
static int xerror(Display __attribute__((unused)) *dpy, XErrorEvent *ee); /* copied from dwm */

static PL_extension predicates[] = {
	/* functor                    arity C-callback                 flags   remarks */
	{ "x_open_display"            ,  2, x_open_display             , 0 }, /* pass "" for XOpenDisplay(NULL) */
	{ "x_close_display"           ,  1, x_close_display            , 0 },
	{ "x_set_error_handler"       ,  0, x_set_error_handler        , 0 }, /* handler it sets is xerror() */
	{ "x_grab_key"                ,  7, x_grab_key                 , 0 },
	{ "x_grab_button"             , 10, x_grab_button              , 0 },
	{ "x_grab_pointer"            ,  9, x_grab_pointer             , 0 }, /* Unused */
	{ "x_ungrab_key"              ,  4, x_ungrab_key               , 0 },
	{ "x_ungrab_button"           ,  4, x_ungrab_button            , 0 },
	{ "x_ungrab_pointer"          ,  2, x_ungrab_pointer           , 0 }, /* Unused */
	{ "x_keysym_to_keycode"       ,  3, x_keysym_to_keycode        , 0 },
	{ "x_string_to_keysym"        ,  2, x_string_to_keysym         , 0 },
	{ "x_next_event"              ,  2, x_next_event               , 0 },
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
	{ "x_sync"                    ,  2, x_sync                     , 0 }, /* Unused */
	{ "x_intern_atom"             ,  4, x_intern_atom              , 0 },
	{ "x_get_class_hint"          ,  4, x_get_class_hint           , 0 },
	{ "x_change_property"         ,  8, x_change_property          , 0 },
	{ "x_delete_property"         ,  3, x_delete_property          , 0 },
	{ "x_utf8_text_list_to_text_property",  5, x_utf8_text_list_to_text_property, 0 },
	{ "x_get_text_property"       ,  5, x_get_text_property        , 0 },
	{ "x_set_text_property"       ,  4, x_set_text_property        , 0 },
	{ "x_create_simple_window"    , 10, x_create_simple_window     , 0 },
	{ "x_get_transient_for_hint"  ,  3, x_get_transient_for_hint   , 0 },
	{ "x_get_window_property"     ,  6, x_get_window_property      , 0 }, /* 4th, 5th, 8th-11th args are omitted */
	{ "x_get_wm_normal_hints"     ,  4, x_get_wm_normal_hints      , 0 }, /* supplied_return arg is ignored */
	{ "x_warp_pointer"            ,  9, x_warp_pointer             , 0 }, /* Unused */

	{ "default_root_window"       ,  2, default_root_window        , 0 },
	{ "default_screen"            ,  2, default_screen             , 0 },
	{ "default_visual"            ,  3, default_visual             , 0 },
	{ "default_colormap"          ,  3, default_colormap           , 0 },

	{ "xinerama_query_screens"    ,  2, xinerama_query_screens     , 0 },
	{ "xft_color_alloc_name"      ,  5, xft_color_alloc_name       , 0 },

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

	PL_TRY("x_open_display/2", PL_get_string(display_name, &dname, &len));
	if (!(dp = XOpenDisplay(len == 1 ? NULL : dname))) {
		return (foreign_t)PL_warning("x_open_display/2: XOpenDisplay() failed!");
	}
	PL_TRY("x_open_display/2", PL_unify_pointer(display, dp));

	PL_succeed;
}

static foreign_t
x_close_display(term_t display)
{
	Display *dp;

	PL_TRY("x_close_display/1", PL_get_pointer_ex(display, (void**)&dp));

	XCloseDisplay(dp);
	PL_succeed;
}

static foreign_t
x_set_error_handler(void)
{
	XSetErrorHandler(xerror);
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

	PL_TRY("x_grab_key/7", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_grab_key/7", PL_get_integer_ex(keycode, &kcode));
	PL_TRY("x_grab_key/7", PL_get_integer_ex(modifiers, &mods));
	PL_TRY("x_grab_key/7", PL_get_uint64_ex(grab_window, &win));
	PL_TRY("x_grab_key/7", PL_get_bool_ex(owner_events, &oevents));
	PL_TRY("x_grab_key/7", PL_get_integer_ex(pointer_mode, &pmode));
	PL_TRY("x_grab_key/7", PL_get_integer_ex(keyboard_mode, &kmode));

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

	PL_TRY("x_grab_button/10", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_grab_button/10", PL_get_integer_ex(button, &btn));
	PL_TRY("x_grab_button/10", PL_get_integer_ex(modifiers, &mods));
	PL_TRY("x_grab_button/10", PL_get_uint64_ex(grab_window, &win));
	PL_TRY("x_grab_button/10", PL_get_bool_ex(owner_events, &oevents));
	PL_TRY("x_grab_button/10", PL_get_integer_ex(event_mask, &emask));
	PL_TRY("x_grab_button/10", PL_get_integer_ex(pointer_mode, &pmode));
	PL_TRY("x_grab_button/10", PL_get_integer_ex(keyboard_mode, &kmode));
	PL_TRY("x_grab_button/10", PL_get_uint64_ex(confine_to, &confto));
	PL_TRY("x_grab_button/10", PL_get_uint64_ex(cursor, &crsr));

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

	PL_TRY("x_grab_pointer/9", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_grab_pointer/9", PL_get_uint64_ex(grab_window, &gwin));
	PL_TRY("x_grab_pointer/9", PL_get_bool_ex(owner_events, &oevents));
	PL_TRY("x_grab_pointer/9", PL_get_integer_ex(event_mask, &emask));
	PL_TRY("x_grab_pointer/9", PL_get_integer_ex(pointer_mode, &ptrmode));
	PL_TRY("x_grab_pointer/9", PL_get_integer_ex(keyboard_mode, &kbmode));
	PL_TRY("x_grab_pointer/9", PL_get_uint64_ex(confine_to, &confto));
	PL_TRY("x_grab_pointer/9", PL_get_uint64_ex(cursor, &csr));
	PL_TRY("x_grab_pointer/9", PL_get_uint64_ex(time, &t));

	XGrabPointer(dp, gwin, oevents, (unsigned)event_mask, ptrmode, kbmode, confto, csr, t);
	PL_succeed;
}

static foreign_t
x_ungrab_key(term_t display, term_t keycode, term_t modifiers, term_t grab_window)
{
	Display *dp;
	int kcode, mods;
	Window win;

	PL_TRY("x_ungrab_key/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_ungrab_key/4", PL_get_integer_ex(keycode, &kcode));
	PL_TRY("x_ungrab_key/4", PL_get_integer_ex(modifiers, &mods));
	PL_TRY("x_ungrab_key/4", PL_get_uint64_ex(grab_window, &win));

	XUngrabKey(dp, kcode, (unsigned)mods, win);
	PL_succeed;
}

static foreign_t
x_ungrab_button(term_t display, term_t button, term_t modifiers, term_t grab_window)
{
	Display *dp;
	int btn, mods;
	Window win;

	PL_TRY("x_ungrab_button/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_ungrab_button/4", PL_get_integer_ex(button, &btn));
	PL_TRY("x_ungrab_button/4", PL_get_integer_ex(modifiers, &mods));
	PL_TRY("x_ungrab_button/4", PL_get_uint64_ex(grab_window, &win));

	XUngrabButton(dp, (unsigned)btn, (unsigned)mods, win);
	PL_succeed;
}

static foreign_t
x_ungrab_pointer(term_t display, term_t time)
{
	Display *dp;
	Time t;

	PL_TRY("x_ungrab_pointer/2", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_ungrab_pointer/2", PL_get_uint64_ex(time, &t));

	XUngrabPointer(dp, t);
	PL_succeed;
}

static foreign_t
x_keysym_to_keycode(term_t display, term_t keysym, term_t keycode)
{
	Display *dp;
	uint64_t ksym;
	KeyCode kcode;

	PL_TRY("x_keysym_to_keycode/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_keysym_to_keycode/3", PL_get_uint64_ex(keysym, &ksym));

	kcode = XKeysymToKeycode(dp, ksym);

	PL_TRY("x_keysym_to_keycode/3", PL_unify_integer(keycode, kcode));
	PL_succeed;
}

static foreign_t
x_string_to_keysym(term_t string, term_t keysym)
{
	char *str;
	size_t len;
	uint64_t ksym;

	PL_TRY("x_string_to_keysym/2", PL_get_string_chars(string, &str, &len));

	ksym = XStringToKeysym(str);

	PL_TRY("x_string_to_keysym/2", PL_unify_uint64(keysym, ksym));
	PL_succeed;
}

static foreign_t
x_next_event(term_t display, term_t event_return)
{
	Display *dp;
	XEvent ev;
	term_t subts[18];
	term_t list;
	size_t stcnt, i;

	PL_TRY("x_next_event/2", PL_get_pointer_ex(display, (void**)&dp));

	XNextEvent(dp, &ev);

	if (ev.type == MapRequest) {
		stcnt = 7;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "maprequest"));
		PL_TRY("x_next_event/2", PL_put_integer(subts[1], ev.xmaprequest.type      ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[2], ev.xmaprequest.serial    ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[3], ev.xmaprequest.send_event));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[4], ev.xmaprequest.display   ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[5], ev.xmaprequest.parent    ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[6], ev.xmaprequest.window    ));
	}
	else if (ev.type == DestroyNotify) {
		stcnt = 7;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "destroynotify"));
		PL_TRY("x_next_event/2", PL_put_integer(subts[1], ev.xdestroywindow.type      ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[2], ev.xdestroywindow.serial    ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[3], ev.xdestroywindow.send_event));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[4], ev.xdestroywindow.display   ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[5], ev.xdestroywindow.event     ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[6], ev.xdestroywindow.window    ));
	}
	else if (ev.type == EnterNotify) {
		stcnt = 18;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "enternotify"));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 1], ev.xcrossing.type       ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 2], ev.xcrossing.serial     ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[ 3], ev.xcrossing.send_event ));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[ 4], ev.xcrossing.display    ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 5], ev.xcrossing.window     ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 6], ev.xcrossing.root       ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 7], ev.xcrossing.subwindow  ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 8], ev.xcrossing.time       ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 9], ev.xcrossing.x          ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[10], ev.xcrossing.y          ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[11], ev.xcrossing.x_root     ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[12], ev.xcrossing.y_root     ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[13], ev.xcrossing.mode       ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[14], ev.xcrossing.detail     ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[15], ev.xcrossing.same_screen));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[16], ev.xcrossing.focus      ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[17], ev.xcrossing.state      ));
	}
	else if (ev.type == PropertyNotify) {
		stcnt = 8;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "propertynotify"));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[1], ev.xproperty.serial    ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[2], ev.xproperty.send_event));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[3], ev.xproperty.display   ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[4], ev.xproperty.window    ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[5], ev.xproperty.atom      ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[6], ev.xproperty.time      ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[7], ev.xproperty.state     ));
	}
	else if (ev.type == ClientMessage) {
		stcnt = 11;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "clientmessage"));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 1], ev.xclient.type               ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 2], ev.xclient.serial             ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[ 3], ev.xclient.send_event         ));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[ 4], ev.xclient.display            ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 5], ev.xclient.window             ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 6], ev.xclient.message_type       ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 7], ev.xclient.format             ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 8], (unsigned)ev.xclient.data.l[0])); /* we are lazy and only handle      */
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 9], (unsigned)ev.xclient.data.l[1])); /* the data were are interested in */
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[10], (unsigned)ev.xclient.data.l[2]));
	}
	else if (ev.type == ConfigureNotify) {
		stcnt = 14;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "configurenotify"));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 1], ev.xconfigure.type             ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 2], ev.xconfigure.serial           ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[ 3], ev.xconfigure.send_event       ));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[ 4], ev.xconfigure.display          ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 5], ev.xconfigure.event            ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 6], ev.xconfigure.window           ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 7], ev.xconfigure.x                ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 8], ev.xconfigure.y                ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 9], ev.xconfigure.width            ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[10], ev.xconfigure.height           ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[11], ev.xconfigure.border_width     ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[12], ev.xconfigure.above            ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[13], ev.xconfigure.override_redirect));
	}
	else {
		/* rest of the event types are very similar */
		stcnt = 15;
		for (i = 0; i < stcnt; ++i) { subts[i] = PL_new_term_ref(); }
		switch (ev.type) {
		case KeyPress:
			PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "keypress"));
			PL_TRY("x_next_event/2", PL_put_integer   (subts[13], ev.xkey.keycode));
			break;
		case KeyRelease:
			PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "keyrelease"));
			PL_TRY("x_next_event/2", PL_put_integer   (subts[13], ev.xkey.keycode));
			break;
		case ButtonPress:
			PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "buttonpress"));
			PL_TRY("x_next_event/2", PL_put_integer   (subts[13], ev.xbutton.button));
			break;
		case ButtonRelease:
			PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "buttonrelease"));
			PL_TRY("x_next_event/2", PL_put_integer   (subts[13], ev.xbutton.button));
			break;
		case MotionNotify:
			PL_TRY("x_next_event/2", PL_put_atom_chars(subts[0], "motionnotify"));
			PL_TRY("x_next_event/2", PL_put_integer   (subts[13], ev.xmotion.is_hint));
			break;
		default:
			PL_TRY("x_next_event/2", PL_unify_atom_chars(event_return, "unsupported_event"));
			PL_succeed;
		}

		/* common fields */
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 1], ev.xkey.serial     ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[ 2], ev.xkey.send_event ));
		PL_TRY("x_next_event/2", PL_put_pointer(subts[ 3], ev.xkey.display    ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 4], ev.xkey.window     ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 5], ev.xkey.root       ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 6], ev.xkey.subwindow  ));
		PL_TRY("x_next_event/2", PL_put_uint64 (subts[ 7], ev.xkey.time       ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 8], ev.xkey.x          ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[ 9], ev.xkey.y          ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[10], ev.xkey.x_root     ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[11], ev.xkey.y_root     ));
		PL_TRY("x_next_event/2", PL_put_integer(subts[12], ev.xkey.state      ));
		PL_TRY("x_next_event/2", PL_put_bool   (subts[14], ev.xkey.same_screen));
	}

	list = PL_new_term_ref();
	build_list(list, subts, stcnt);
	PL_TRY("x_next_event/2", PL_unify(list, event_return));
	PL_succeed;
}

static foreign_t
x_raise_window(term_t display, term_t w)
{
	Display *dp;
	Window win;

	PL_TRY("x_raise_window/2", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_raise_window/2", PL_get_uint64_ex(w, &win));

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
	size_t i;
	Status st;

	PL_TRY("x_get_window_attributes/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_get_window_attributes/4", PL_get_uint64_ex(w, &win));

	st = XGetWindowAttributes(dp, win, &winattrs);

	for (i = 0; i < 4; ++i) {
		subts[i] = PL_new_term_ref();
	}
	PL_TRY("x_get_window_attributes/4", PL_put_integer(subts[0], winattrs.x));
	PL_TRY("x_get_window_attributes/4", PL_put_integer(subts[1], winattrs.y));
	PL_TRY("x_get_window_attributes/4", PL_put_integer(subts[2], winattrs.width));
	PL_TRY("x_get_window_attributes/4", PL_put_integer(subts[3], winattrs.height));

	build_list(list, subts, 4);
	PL_TRY("x_get_window_attributes/4", PL_unify(list, window_attributes_return));
	PL_TRY("x_get_window_attributes/4", PL_unify_integer(status, st));
	PL_succeed;
}

static foreign_t
x_move_resize_window(term_t display, term_t w, term_t x, term_t y, term_t width, term_t height)
{
	Display *dp;
	Window win;
	int wx, wy, wwidth, wheight;

	PL_TRY("x_move_resize_window/6", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_move_resize_window/6", PL_get_uint64_ex(w, &win));
	PL_TRY("x_move_resize_window/6", PL_get_integer_ex(x, &wx));
	PL_TRY("x_move_resize_window/6", PL_get_integer_ex(y, &wy));
	PL_TRY("x_move_resize_window/6", PL_get_integer_ex(width, &wwidth));
	PL_TRY("x_move_resize_window/6", PL_get_integer_ex(height, &wheight));

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

	PL_TRY("x_change_window_attributes/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_change_window_attributes/4", PL_get_uint64_ex(w, &win));
	PL_TRY("x_change_window_attributes/4", PL_get_uint64_ex(valuemask, &vmask));
	PL_TRY("x_change_window_attributes/4", PL_get_long_ex(event_mask, &emask));

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

	PL_TRY("x_select_input/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_select_input/3", PL_get_long_ex(event_mask, &emask));
	PL_TRY("x_select_input/3", PL_get_uint64_ex(w, &win));

	XSelectInput(dp, win, emask);
	PL_succeed;
}

static foreign_t
x_map_window(term_t display, term_t w)
{
	Display *dp;
	Window win;

	PL_TRY("x_map_window/2", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_map_window/2", PL_get_uint64_ex(w, &win));

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

	PL_TRY("x_configure_window/10", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_configure_window/10", PL_get_uint64_ex(w, &win));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(value_mask, &vmask));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(x, &wx));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(y, &wy));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(width, &wwidth));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(height, &wheight));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(border_width, &bw));
	PL_TRY("x_configure_window/10", PL_get_uint64_ex(sibling, &sib));
	PL_TRY("x_configure_window/10", PL_get_integer_ex(stack_mode, &stackm));
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

	PL_TRY("x_set_window_border/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_set_window_border/3", PL_get_uint64_ex(w, &win));
	PL_TRY("x_set_window_border/3", PL_get_uint64_ex(border_pixel, &bp));

	XSetWindowBorder(dp, win, bp);
	PL_succeed;
}

static foreign_t
x_set_input_focus(term_t display, term_t focus, term_t revert_to, term_t time)
{
	Display *dp;
	Window win, tim;
	int revto;

	PL_TRY("x_set_input_focus/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_set_input_focus/4", PL_get_uint64_ex(focus, &win));
	PL_TRY("x_set_input_focus/4", PL_get_integer_ex(revert_to, &revto));
	PL_TRY("x_set_input_focus/4", PL_get_uint64_ex(time, &tim));

	XSetInputFocus(dp, win, revto, tim);
	PL_succeed;
}

static foreign_t
x_kill_client(term_t display, term_t resource)
{
	Display *dp;
	uint64_t res;

	PL_TRY("x_kill_client/2", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_kill_client/2", PL_get_uint64_ex(resource, &res));

	XKillClient(dp, res);
	PL_succeed;
}

static foreign_t
x_sync(term_t display, term_t discard)
{
	Display *dp;
	Bool discrd;

	PL_TRY("x_sync/2", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_sync/2", PL_get_bool_ex(discard, &discrd));

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

	PL_TRY("x_intern_atom/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_intern_atom/4", PL_get_string_chars(atom_name, &aname, &len));
	PL_TRY("x_intern_atom/4", PL_get_bool_ex(only_if_exists, &ifexists));

	a = XInternAtom(dp, aname, ifexists);

	PL_TRY("x_intern_atom/4", PL_unify_uint64(atom, a));
	PL_succeed;
}

static foreign_t
x_get_class_hint(term_t display, term_t w, term_t res_name, term_t res_class)
{
	Display *dp;
	Window win;
	XClassHint ch = { NULL, NULL };

	PL_TRY("x_get_class_hint/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_get_class_hint/4", PL_get_uint64_ex(w, &win));

	if (XGetClassHint(dp, win, &ch)) {
		PL_TRY("x_get_class_hint/4", PL_unify_string_chars(res_name , ch.res_name  ? ch.res_name  : ""));
		PL_TRY("x_get_class_hint/4", PL_unify_string_chars(res_class, ch.res_class ? ch.res_class : ""));
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

	PL_TRY("x_change_property/8", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_change_property/8", PL_get_uint64_ex(w, &win));
	PL_TRY("x_change_property/8", PL_get_uint64_ex(property, &prop));
	PL_TRY("x_change_property/8", PL_get_uint64_ex(atom, &a));
	PL_TRY("x_change_property/8", PL_get_integer_ex(format, &fmt));
	PL_TRY("x_change_property/8", PL_get_integer_ex(mode, &md));
	PL_TRY("x_change_property/8", PL_get_integer_ex(nelements, &nelem));

	if (XA_LAST_PREDEFINED < a) { /* handle the UTF8_STRING case separately */
		PL_TRY("x_change_property/8", PL_get_string_chars(data, &str, &len));
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

	PL_TRY("x_delete_property/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_delete_property/3", PL_get_uint64_ex(w, &win));
	PL_TRY("x_delete_property/3", PL_get_uint64_ex(property, &prop));

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

	PL_TRY("x_utf8_text_list_to_text_property/5", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_utf8_text_list_to_text_property/5", PL_get_integer_ex(count, &cnt));
	PL_TRY("x_utf8_text_list_to_text_property/5", PL_get_integer_ex(style, &sty));

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
	PL_TRY("x_utf8_text_list_to_text_property/5", PL_unify_pointer(text_prop_return, tprop));
	PL_succeed;
}

static foreign_t
x_get_text_property(term_t display, term_t w, term_t text, term_t property, term_t status)
{
	Display *dp;
	Window win;
	XTextProperty tprop;
	Atom prop;
	Status sts;
	static char stext[256];

	PL_TRY("x_get_text_property/5", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_get_text_property/5", PL_get_uint64_ex(w, &win));
	PL_TRY("x_get_text_property/5", PL_get_uint64_ex(property, &prop));

	sts = XGetTextProperty(dp, win, &tprop, prop);
	strncpy(stext, (char*)tprop.value, sizeof(stext)-1);

	PL_TRY("x_get_text_property/5", PL_unify_integer(status, sts));
	PL_TRY("x_get_text_property/5", PL_unify_string_chars(text, stext));
	PL_succeed;
}

static foreign_t
x_set_text_property(term_t display, term_t w, term_t text_prop, term_t property)
{
	Display *dp;
	Window win;
	XTextProperty *tprop;
	Atom prop;

	PL_TRY("x_set_text_property/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_set_text_property/4", PL_get_uint64_ex(w, &win));
	PL_TRY("x_set_text_property/4", PL_get_pointer_ex(text_prop, (void**)&tprop));
	PL_TRY("x_set_text_property/4", PL_get_uint64_ex(property, &prop));

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

	PL_TRY("x_create_simple_window/10", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_create_simple_window/10", PL_get_uint64_ex(parent, &pwin));
	PL_TRY("x_create_simple_window/10", PL_get_integer_ex(x, &wx));
	PL_TRY("x_create_simple_window/10", PL_get_integer_ex(y, &wy));
	PL_TRY("x_create_simple_window/10", PL_get_integer_ex(width, &ww));
	PL_TRY("x_create_simple_window/10", PL_get_integer_ex(height, &wh));
	PL_TRY("x_create_simple_window/10", PL_get_integer_ex(border_width, &bw));
	PL_TRY("x_create_simple_window/10", PL_get_uint64_ex(border, &brd));
	PL_TRY("x_create_simple_window/10", PL_get_uint64_ex(background, &bg));

	win = XCreateSimpleWindow(dp, pwin, wx, wy, (unsigned)ww, (unsigned)wh, (unsigned)bw, brd, bg);

	PL_TRY("x_create_simple_window/10", PL_unify_uint64(w, win));
	PL_succeed;
}

static foreign_t
x_get_transient_for_hint(term_t display, term_t w, term_t prop_window_return)
{
	Display *dp;
	Window win, wret;

	PL_TRY("x_get_transient_for_hint/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_get_transient_for_hint/3", PL_get_uint64_ex(w, &win));

	XGetTransientForHint(dp, win, &wret);

	PL_TRY("x_get_transient_for_hint/3", PL_unify_uint64(prop_window_return, wret));
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

	PL_TRY("x_get_window_property/6", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_get_window_property/6", PL_get_uint64_ex(w, &win));
	PL_TRY("x_get_window_property/6", PL_get_uint64_ex(property, &prop));
	PL_TRY("x_get_window_property/6", PL_get_bool_ex(delete, &del));
	PL_TRY("x_get_window_property/6", PL_get_uint64_ex(req_type, &rqtyp));

	if (XGetWindowProperty(dp, win, prop, 0L, sizeof(Atom), del, rqtyp, &atr, &afr, &nr, &bar, &p) == Success && p) {
		propret = *(Atom*)p;
		XFree(p);
	}

	PL_TRY("x_get_window_property/6", PL_unify_uint64(prop_return, propret));
	PL_succeed;
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
	size_t i;

	PL_TRY("x_get_wm_normal_hints/4", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_get_wm_normal_hints/4", PL_get_uint64_ex(w, &win));

	st = XGetWMNormalHints(dp, win, &hints, &supret); /* supplied_return arg is ignored */

	if (st) {
		for (i = 0; i < 18; ++i) {
			subts[i] = PL_new_term_ref();
		}
		PL_TRY("x_get_wm_normal_hints/4", PL_put_uint64 (subts[ 0], (unsigned)hints.flags));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 1], hints.x));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 2], hints.y));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 3], hints.width));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 4], hints.height));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 5], hints.min_width));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 6], hints.min_height));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 7], hints.max_width));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 8], hints.max_height));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[ 9], hints.width_inc));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[10], hints.height_inc));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[11], hints.min_aspect.x));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[12], hints.min_aspect.y));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[13], hints.max_aspect.x));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[14], hints.max_aspect.y));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[15], hints.base_width));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[16], hints.base_height));
		PL_TRY("x_get_wm_normal_hints/4", PL_put_integer(subts[17], hints.win_gravity));
		build_list(list, subts, 18);

		PL_TRY("x_get_wm_normal_hints/4", PL_unify(list, hints_return));
	}
	else {
		PL_TRY("x_get_wm_normal_hints/4", PL_unify_nil_ex(hints_return));
	}

	PL_TRY("x_get_wm_normal_hints/4", PL_unify_integer(status, st));
	PL_succeed;
}

static foreign_t
x_warp_pointer(term_t display, term_t src_w, term_t dest_w, term_t src_x, term_t src_y,
                                term_t src_width, term_t src_height, term_t dest_x, term_t dest_y)
{
	Display *dp;
	Window swin, dwin;
	int sx, sy, sw, sh, dx, dy;

	PL_TRY("x_warp_pointer/9", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("x_warp_pointer/9", PL_get_uint64_ex(src_w, &swin));
	PL_TRY("x_warp_pointer/9", PL_get_uint64_ex(dest_w, &dwin));
	PL_TRY("x_warp_pointer/9", PL_get_integer_ex(src_x, &sx));
	PL_TRY("x_warp_pointer/9", PL_get_integer_ex(src_y, &sy));
	PL_TRY("x_warp_pointer/9", PL_get_integer_ex(src_width, &sw));
	PL_TRY("x_warp_pointer/9", PL_get_integer_ex(src_height, &sh));
	PL_TRY("x_warp_pointer/9", PL_get_integer_ex(dest_x, &dx));
	PL_TRY("x_warp_pointer/9", PL_get_integer_ex(dest_y, &dy));

	XWarpPointer(dp, swin, dwin, sx, sy, (unsigned)sw, (unsigned)sh, dx, dy);
	PL_succeed;
}

static foreign_t
default_root_window(term_t display, term_t w)
{
	Display *dp;
	Window win;

	PL_TRY("default_root_window/2", PL_get_pointer_ex(display, (void**)&dp));

	win = DefaultRootWindow(dp);

	PL_TRY("default_root_window/2", PL_unify_uint64(w, win));
	PL_succeed;
}

static foreign_t
default_screen(term_t display, term_t screen)
{
	Display *dp;
	int scr;

	PL_TRY("default_screen/2", PL_get_pointer_ex(display, (void**)&dp));

	scr = DefaultScreen(dp);

	PL_TRY("default_screen/2", PL_unify_integer(screen, scr));
	PL_succeed;
}

static foreign_t
default_visual(term_t display, term_t screen_number, term_t visual)
{
	Display *dp;
	int scrnum;
	Visual *vis;

	PL_TRY("default_visual/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("default_visual/3", PL_get_integer_ex(screen_number, &scrnum));

	vis = DefaultVisual(dp, scrnum);

	PL_TRY("default_visual/3", PL_unify_pointer(visual, vis));

	PL_succeed;
}

static foreign_t
default_colormap(term_t display, term_t screen_number, term_t colormap)
{
	Display *dp;
	int scrnum;
	Colormap cmap;

	PL_TRY("default_colormap/3", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("default_colormap/3", PL_get_integer_ex(screen_number, &scrnum));

	cmap = DefaultColormap(dp, scrnum);

	PL_TRY("default_colormap/3", PL_unify_uint64(colormap, cmap));

	PL_succeed;
}

static foreign_t
xinerama_query_screens(term_t display, term_t screen_info)
{
	Display *dp;
	XineramaScreenInfo *scrinfo;
	int num, i;
	term_t *subts;
	term_t list = PL_new_term_ref();

	PL_TRY("xinerama_query_screens/2", PL_get_pointer_ex(display, (void**)&dp));

	scrinfo = XineramaQueryScreens(dp, &num);

	if (num == 0) {
		PL_unify_nil_ex(screen_info);
		PL_succeed;
	}

	subts = malloc((size_t)num * 5 * sizeof(term_t));
	for (i = 0; i < num; ++i) {
		subts[i * 5 + 0] = PL_new_term_ref(); PL_TRY("xinerama_query_screens/2", PL_put_integer(subts[i * 5 + 0], scrinfo[i].screen_number), free(subts));
		subts[i * 5 + 1] = PL_new_term_ref(); PL_TRY("xinerama_query_screens/2", PL_put_integer(subts[i * 5 + 1], scrinfo[i].x_org), free(subts));
		subts[i * 5 + 2] = PL_new_term_ref(); PL_TRY("xinerama_query_screens/2", PL_put_integer(subts[i * 5 + 2], scrinfo[i].y_org), free(subts));
		subts[i * 5 + 3] = PL_new_term_ref(); PL_TRY("xinerama_query_screens/2", PL_put_integer(subts[i * 5 + 3], scrinfo[i].width), free(subts));
		subts[i * 5 + 4] = PL_new_term_ref(); PL_TRY("xinerama_query_screens/2", PL_put_integer(subts[i * 5 + 4], scrinfo[i].height), free(subts));
	}
	build_list(list, subts, (size_t)num * 5);

	if (!PL_unify(list, screen_info)) {
		free(subts);
		return (foreign_t)PL_warning("xinerama_query_screens/2: PL_unify() on 'screen_info' failed!");
	}
	free(subts);
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

	PL_TRY("xft_color_alloc_name/5", PL_get_pointer_ex(display, (void**)&dp));
	PL_TRY("xft_color_alloc_name/5", PL_get_pointer_ex(visual, (void**)&vis));
	PL_TRY("xft_color_alloc_name/5", PL_get_uint64_ex(cmap, &cm));
	PL_TRY("xft_color_alloc_name/5", PL_get_string(name, &nam, &len));

	if (!XftColorAllocName(dp, vis, cm, nam, &res)) {
		return (foreign_t)PL_warning("xft_color_alloc_name/5: XftColorAllocName() failed!");
	}

	PL_TRY("xft_color_alloc_name/5", PL_unify_uint64(result, res.pixel));
	PL_succeed;
}

static foreign_t
c_free(term_t ptr)
{
	void *p;

	PL_TRY("c_free/1", PL_get_pointer_ex(ptr, &p));
	
	free(p);
	PL_succeed;
}

static int
build_list(term_t dst, term_t *src, size_t size)
{
	size_t i;
	PL_unify_nil_ex(dst);
	for (i = size; 0 < i; --i) {
		if (!PL_cons_list(dst, src[i - 1], dst)) {
			return 0;
		}
	}
	return 1;
}

static int
xerror(Display __attribute__((unused)) *dpy, XErrorEvent *ee) /* copied from dwm */
{
	if (ee->error_code == BadWindow
	|| (ee->request_code == X_SetInputFocus && ee->error_code == BadMatch)
	|| (ee->request_code == X_PolyText8 && ee->error_code == BadDrawable)
	|| (ee->request_code == X_PolyFillRectangle && ee->error_code == BadDrawable)
	|| (ee->request_code == X_PolySegment && ee->error_code == BadDrawable)
	|| (ee->request_code == X_ConfigureWindow && ee->error_code == BadMatch)
	|| (ee->request_code == X_GrabButton && ee->error_code == BadAccess)
	|| (ee->request_code == X_GrabKey && ee->error_code == BadAccess)
	|| (ee->request_code == X_CopyArea && ee->error_code == BadDrawable))
		return 0;
	fprintf(stderr, "plx: fatal error: request code=%d, error code=%d\n",
		ee->request_code, ee->error_code);
	return -1;
}

