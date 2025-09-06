:- module(plx, []).

:- use_module(library(ffi)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(error)).
:- use_module(library(si)).
:- use_module(library(charsio)).
:- use_module(library(debug)).

% Note: assumes
%    char           = u8
%    int            = i32
%    unsigned int   = u32
%    long           = i64
%    unsigned long  = u64


% define X11 structs
:- initialization(foreign_struct('XRenderColor', [
    u16, u16, u16, u16
])).

:- initialization(foreign_struct('XClassHint', [
    ptr, ptr
])).


:- initialization(foreign_struct('XWindowChanges', [
    i32, i32, i32, i32, i32, u64, i32
])).

:- initialization(foreign_struct('XSetWindowAttributes', [
    u64, u64, u64, u64, i32, i32, i32, u64, u64, i32, i64, i64, i32, u64, u64
])).

:- initialization(foreign_struct('XTextProperty', [
    ptr, u64, i32, u64
])).

% XEvent is a union with the size of 24x long
:- initialization((ffi:array_type(i64, 24, Pad), foreign_struct('XEvent', [Pad]))).

:- initialization(foreign_struct('XAnyEvent', [
    i32, u64, i32, ptr, u64
])).

:- initialization(foreign_struct('XMapRequestEvent', [
    i32, u64, i32, ptr, u64, u64
])).

:- initialization(foreign_struct('XUnmapEvent', [
    i32, u64, i32, ptr, u64, u64, i32
])).

:- initialization(foreign_struct('XDestroyWindowEvent', [
    i32, u64, i32, ptr, u64, u64
])).

:- initialization(foreign_struct('XCrossingEvent', [
    i32, u64, i32, ptr, u64, u64, u64, u64, i32, i32, i32, i32, i32, i32, i32, i32, u32
])).

:- initialization(foreign_struct('XPropertyEvent', [
    i32, u64, i32, ptr, u64, u64, u64, i32
])).

:- initialization(foreign_struct('XConfigureEvent', [
    i32, 
    u64, 
    i32, 
    ptr, 
    u64, 
    u64, 
    i32, 
    i32, 
    i32, 
    i32, 
    i32, 
    u64, 
    i32
])).

:- initialization(foreign_struct('XWindowAttributes', [
    i32, % x
    i32, % y
    i32, % width
    i32, % height
    i32, % border_width
    i32, % depth
    ptr, % visual
    u64, % root
    i32, % class
    i32, % bit_gravity
    i32, % win_gravity
    i32, % backing_store
    u64, % backing_planes
    u64, % backing_pixel
    i32, % save_under
    u64, % colormap
    i32, % map_installed
    i32, % map_state
    i64, % all_event_masks
    i64, % your_event_masks
    i64, % do_not_propagate_masks
    i32, % override_redirect
    ptr % screen
])).

:- initialization(foreign_struct('AspectRatio', [
        i32, % x
        i32  % y
])).

:- initialization(foreign_struct('XSizeHints', [
    u64, % flags
    i32, % x
    i32, % y
    i32, % width
    i32, % height
    i32, % min_width
    i32, % min_height
    i32, % max_width
    i32, % max_height
    i32, % width_inc
    i32, % height_inc
    'AspectRatio', % min_aspect
    'AspectRatio', % max_aspect
    i32, % base_width
    i32, % base_height
    i32  % win_gravity
])).


% technically a union of [c_char; 20], [c_short; 10] and [c_long; 5], but we only use the long
% so we just define it as 5 longs
:- initialization(foreign_struct('ClientMessageData', [
    u64, u64, u64, u64, u64
])).


:- initialization(foreign_struct('XClientMessageEvent', [
    i32, u64, i32, ptr, u64, u64, i32, 'ClientMessageData'
])).

:- initialization(foreign_struct('XKeyEvent', [
    i32, % type 
    u64, % serial
    i32, % send_event
    ptr, % display
    u64, % windows
    u64, % root
    u64, % subwindow
    u64, % time
    i32, % x
    i32, % y
    i32, % x_root
    i32, % y_root
    u32, % state
    u32, % keycode
    i32 % same_screen
])).

:- initialization(foreign_struct('XButtonEvent', [
    i32, % type 
    u64, % serial
    i32, % send_event
    ptr, % display
    u64, % windows
    u64, % root
    u64, % subwindow
    u64, % time
    i32, % x
    i32, % y
    i32, % x_root
    i32, % y_root
    u32, % state
    u32, % button
    i32 % same_screen
])).

:- initialization(foreign_struct('XMotionEvent', [
    i32, % type 
    u64, % serial
    i32, % send_event
    ptr, % display
    u64, % windows
    u64, % root
    u64, % subwindow
    u64, % time
    i32, % x
    i32, % y
    i32, % x_root
    i32, % y_root
    u32, % state
    u8, % us_hint
    i32 % same_screen
])).

% define Xft structs
:- initialization(foreign_struct('XftColor', [
    u64, 'XRenderColor'
])).

% define Xrandr structs
:- initialization(foreign_struct('XRRCrtcInfo', [
    u64, i32, i32, u32, u32, u64, u16, i32, ptr, u16, i32, ptr
])).

:- initialization(foreign_struct('XRRScreenResources', [
    u64, u64, i32, ptr, i32, ptr, i32, ptr
])).

:- initialization(foreign_struct('XRROutputInfo', [
    u64, u64, cstr, i32, u64, u64, u16, u16, i32, ptr, i32, ptr, i32, i32, ptr
])).

% bind to x11plwm wrapper shared library
:- initialization(use_foreign_module("x11plwm.so", [
    'x11plwm_DefaultRootWindow'([ptr], u64),
    'x11plwm_DefaultScreen'([ptr], i32),
    'x11plwm_DefaultColormap'([ptr, i32], u64),
    'x11plwm_DefaultVisual'([ptr, i32], ptr),
    'x11plwm_set_error_handler'([i32], void)
])).

% bind to X11 shared library
:- initialization(use_foreign_module("libX11.so", [
    'XFree'([ptr], i32),

    'XOpenDisplay'([cstr], ptr),
    'XCloseDisplay'([ptr], i32),

    'XSetCloseDownMode'([ptr, i32], i32),

    'XCreateFontCursor'([ptr, i32], u64),

    'XDefineCursor'([ptr, u64, u64], i32),
    'XFreeCursor'([ptr, u64], i32),

    'XGrabKey'([ptr, i32, u32, u64, i32, i32, i32], i32),
    'XGrabButton'([ptr, u32, u32, u64, i32, u32, i32, i32, u64, u64], i32),
    'XGrabPointer'([ptr, u64, i32, u32, i32, i32, u64, u64, u64], i32),
    'XGrabServer'([ptr], i32),

    'XUngrabKey'([ptr, i32, u32, u64], i32),
    'XUngrabButton'([ptr, u32, u32, u64], i32),
    'XUngrabPointer'([ptr, u64], i32),
    'XUngrabServer'([ptr], i32),

    'XKeysymToKeycode'([ptr, u64], i32),
    'XStringToKeysym'([cstr], u64),

    'XNextEvent'([ptr, ptr], i32),
    'XSendEvent'([ptr, u64, i32, u64, ptr], i32),

    'XRaiseWindow'([ptr, u64], i32),
    'XGetWindowAttributes'([ptr, u64, ptr], i32),
    'XChangeWindowAttributes'([ptr, u64, u64, ptr], i32),
    'XMoveResizeWindow'([ptr, u64, i32, i32, u32, u32], i32),

    'XSelectInput'([ptr, u64, i64], i32),

    'XMapWindow'([ptr, u64], i32),
    'XConfigureWindow'([ptr, u64, u32, ptr], i32),
    'XSetWindowBorder'([ptr, u64, u64], i32),

    'XSetInputFocus'([ptr, u64, i32, u64], i32),

    'XKillClient'([ptr, u64], i32),

    'XSync'([ptr, i32], i32),

    'XInternAtom'([ptr, cstr, i32], u64),
    'XGetClassHint'([ptr, u64, ptr], i32),

    'XInternAtom'([ptr, cstr, i32], u64),

    'XGetWindowProperty'([
        ptr, % display
        u64, % ww
        u64, % property
        u64, % long_offset
        u64, % long_length
        i32, % delete
        u64, % req_type
        ptr, % actual_type_return
        ptr, % actual_format_return
        ptr, % nitems_return
        ptr, % bytes_after_return
        ptr  % prop_return
    ], i32),
    'XChangeProperty'([ptr, u64, u64, u64, i32, i32, ptr, i32], i32),
    'XDeleteProperty'([ptr, u64, u64], i32),

    'XCreateSimpleWindow'([ptr, u64, i32, i32, u32, u32, u32, u64, u64], u64),

    'Xutf8TextListToTextProperty'([ptr, ptr, i32, i32, ptr], i32),

    'XGetTextProperty'([ptr, u64, ptr, u64], i32),
    'XSetTextProperty'([ptr, u64, ptr, u64], void),

    'XGetTransientForHint'([ptr, u64, ptr], i32),

    'XWarpPointer'([ptr, u64, u64, i32, i32, u32, u32, i32, i32], i32),

    'XGetWMProtocols'([ptr, u64, ptr, ptr], i32),
    'XGetWMNormalHints'([ptr, u64, ptr, ptr], i32)
])).

% bind to Xft shared library
:- initialization(use_foreign_module("libXft.so", [
    'XftColorAllocName'([ptr, ptr, u64, cstr, ptr], i32)
])).


% bind to Xrandr shared library
:- initialization(use_foreign_module("libXrandr.so", [
    'XRRQueryExtension'([ptr, ptr, ptr], i32),

    'XRRSelectInput'([ptr, u64, i32], void),

    'XRRGetScreenResources'([ptr, u64], ptr),
    'XRRFreeScreenResources'([ptr], void),

    'XRRGetOutputInfo'([ptr, ptr, u64], ptr),
    'XRRFreeOutputInfo'([ptr], void),

    'XRRGetCrtcInfo'([ptr, ptr, u64], ptr),
    'XRRFreeCrtcInfo'([ptr], void)
])).


bool_int(Bool, Int) :-

    (   nonvar(Bool) -> (
            Bool = true -> Int = 1
        ;   Bool = false -> Int = 0
        ;   domain_error(boolean_atom, Bool, bool_int/2)
        )
    ;   nonvar(Int) -> (
            Int = 1 -> Bool = true
        ;   Int = 0 -> Bool = false
        ;   domain_error(boolean_int, Int, bool_int/2)
        )
    ;   instantiation_error([Bool, Int], bool_int/2)
    )
.

c_free(Ptr) :-
    % for the c dellocator the type doesn't matter
    % as it ignores the types layout and just calls free(Ptr) under the hood.
    % so we use just u8 as we need to pass some valid type
    ffi:deallocate(c, u8, Ptr).

x_open_display(DpName, DpPtr) :-
    ( DpName = "" -> DpArg = 0
    ; DpArg = DpName
    ),
    ffi:'XOpenDisplay'(DpArg, DpPtr).

x_close_display(Dp) :- ffi:'XCloseDisplay'(Dp, _).

x_set_close_down_mode(Dp, Mode) :- ffi:'XSetCloseDownMode'(Dp, Mode, _).

default_root_window(DpPtr, Win) :-
    ffi:'x11plwm_DefaultRootWindow'(DpPtr, Win).

default_screen(DpPtr, Screen) :-
    ffi:'x11plwm_DefaultScreen'(DpPtr, Screen).

default_colormap(DpPtr, Screen, Cm) :-
    ffi:'x11plwm_DefaultColormap'(DpPtr, Screen, Cm).

default_visual(DpPtr, Screen, VisPtr) :-
    ffi:'x11plwm_DefaultVisual'(DpPtr, Screen, VisPtr).

x_set_error_handler(false) :- ffi:'x11plwm_set_error_handler'(0).
x_set_error_handler(true) :- ffi:'x11plwm_set_error_handler'(1).

x_intern_atom(Dp, Name, IfExists, Atom) :-
    bool_int(IfExists, If),
    ffi:'XInternAtom'(Dp, Name, If, Atom).

x_get_window_property(Dp, Win, Prop, Del, ReqType, PropRet) :-
    bool_int(Del, Delete),
    with_locals([
        let(AtrPtr, u64, 0),
        let(AfrPtr, i32, 0),
        let(NrPtr, u64, 0),
        let(BarPtr, u64, 0),
        let(PPtr, ptr, 0)
    ],
    (
        ffi:'XGetWindowProperty'(Dp, Win, Prop, 0, 8 /*sizeof(Atom)*/, Delete, ReqType, AtrPtr, AfrPtr, NrPtr, BarPtr, PPtr, Ret),
        (
            (
                Ret == 0, 
                ffi:read_ptr(ptr, PPtr, P), 
                P \= 0
            ) ->    ffi:read_ptr(u64, P, Property), ffi:'XFree'(P, _)
            ;       Property = 0
        )
    )),
    PropRet = Property.

x_change_property(Dp, Win, Prop, Atom, Format, Mode, Data, NElements) :-
    (chars_si(Data) ->
        % special treatment for strings
        chars_utf8bytes(Data, Codes),
        ElemType = u8,
        ArrayValues = Codes
    ;   ElemType = u64,
        ArrayValues = Data
    ),
    length(ArrayValues, Len),
    ( Len = NElements -> true
    ; throw(error(assert(Len = NElements), x_change_property/8))
    ),
    ffi:array_type(ElemType, Len, ArrayType),
    with_locals([
        let(ArrayPtr, ArrayType, [ArrayType | ArrayValues])
    ],
        ffi:'XChangeProperty'(Dp, Win, Prop, Atom, Format, Mode, ArrayPtr, Len, _)
    ).

x_delete_property(Dp, Win, Property) :- ffi:'XDeleteProperty'(Dp, Win, Property, _).

x_get_window_attributes(Dp, Win, WinAttrRet, Status) :-
    length(Init, 23), maplist(=(0), Init),
    with_locals(
        [
            let(Ret, 'XWindowAttributes', ['XWindowAttributes' | Init ])
        ],
        (
            ffi:'XGetWindowAttributes'(Dp, Win, Ret, St),
            ffi:read_ptr('XWindowAttributes', Ret, ['XWindowAttributes' , X, Y, Width, Height | _])
        )),
        WinAttrRet = [X, Y, Width, Height],
        Status = St.



x_change_window_attributes(Dp, Win, ValueMask, EventMask) :-
    with_locals([
        let(WinAttributesPtr, 'XSetWindowAttributes', ['XSetWindowAttributes', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, EventMask, 0, 0, 0, 0])
    ],
        ffi:'XChangeWindowAttributes'(Dp, Win, ValueMask, WinAttributesPtr, _)
    ).

x_get_class_hint(Dp, Win , ResName, ResClass) :-
    with_locals(
        [
            let(Ch, 'XClassHint', ['XClassHint', 0, 0])
        ],
        (
            ffi:'XGetClassHint'(Dp, Win, Ch, Bool),
            Bool \= 0,
            ffi:read_ptr('XClassHint', Ch, ['XClassHint', NamePtr, ClassPtr]),
            (NamePtr = 0 -> Name = "" ; ffi:read_ptr(cstr, NamePtr, Name), ffi:'XFree'(NamePtr, _)),
            (ClassPtr = 0 -> Class = "" ; ffi:read_ptr(cstr, ClassPtr, Class), ffi:'XFree'(ClassPtr, _))
        )
    ),
    ResName = Name,
    ResClass = Class.

x_create_simple_window(Dp, Parent, X, Y, Width, Height, BorderW, Border, Background, Win) :-
    ffi:'XCreateSimpleWindow'(Dp, Parent, X, Y, Width, Height, BorderW, Border, Background, Win).

x_select_input(Dp, Win, EventMask) :-
    ffi:'XSelectInput'(Dp, Win, EventMask, _).

x_create_font_cursor(Dp, Shape, Cursor) :-
    ffi:'XCreateFontCursor'(Dp, Shape, Cursor).

x_define_cursor(Dp, Win, Cursor) :-
    ffi:'XDefineCursor'(Dp, Win, Cursor, _).

x_free_cursor(Dp, Cursor) :- ffi:'XFreeCursor'(Dp, Cursor, _).

x_grab_key(Dp, KeyCode, Modifiers, GrabWindow, OwnerEvents, PointerMode, KeyboardMode) :-
    bool_int(OwnerEvents, Oes),
    ffi:'XGrabKey'(Dp, KeyCode, Modifiers, GrabWindow, Oes, PointerMode, KeyboardMode, _).

x_ungrab_key(Dp, KeyCode, Modifiers, GrabWindow) :-
    ffi:'XUngrabKey'(Dp, KeyCode, Modifiers, GrabWindow, _).

x_grab_button(Dp, Button, Modifiersm, GrabWindow, OwnerEvents, EventMask, PointerMode, KeyboardMode, ConfineTo, Cursor) :-
    bool_int(OwnerEvents, Oes),
    ffi:'XGrabButton'(Dp, Button, Modifiersm, GrabWindow, Oes, EventMask, PointerMode, KeyboardMode, ConfineTo, Cursor, _).

x_ungrab_button(Dp, Button, Modifiers, GrabWindow) :-
    ffi:'XUngrabButton'(Dp, Button, Modifiers, GrabWindow, _).

x_grab_pointer(Dp, GWin, OwenerEvents, EventMask, PointerMode, KeyboardMode, ConfinedTo, Cursor, Time) :-
    bool_int(OwenerEvents, Oe),
    ffi:'XGrabPointer'(Dp, GWin, Oe, EventMask, PointerMode, KeyboardMode, ConfinedTo, Cursor, Time).

x_ungrab_pointer(Dp, Time) :- ffi:'XUngrabPointer'(Dp, Time).

x_grab_server(Dp) :- ffi:'XGrabServer'(Dp, _).

x_ungrab_server(Dp) :- ffi:'XUngrabServer'(Dp, _).

x_string_to_keysym(KeyName, KeySymbol) :-
    ffi:'XStringToKeysym'(KeyName, KeySymbol).

x_keysym_to_keycode(Dp, KeySymbol, KeyCode) :-
    ffi:'XKeysymToKeycode'(Dp, KeySymbol, KeyCode).

gen_local_str(Atom, let(_Ptr, ArrayType, [ArrayType | Utf8Nul ])) :-
    atom_chars(Atom, String),
    chars_utf8bytes(String, Utf8),
    append(Utf8, [0], Utf8Nul),
    length(Utf8Nul, Len),
    ffi:array_type(u8, Len, ArrayType).


x_utf8_text_list_to_text_property(Dp, List, Count, Style, TextPropReturn) :- call_with_error_context(x_utf8_text_list_to_text_property_(Dp, List, Count, Style, TextPropReturn), predicate-x_utf8_text_list_to_text_property/5).

x_utf8_text_list_to_text_property_(Dp, List, Count, Style, TextPropReturn) :-
    length(List, Len),
    ( Len = Count -> true
    ; throw(error(assert(Len = Count), x_utf8_text_list_to_text_property/5))
    ),
    maplist(
        gen_local_str,
        List,
        Locals
    ),
    maplist(arg(1), Locals, Ptrs),
    ffi:array_type(ptr, Count, PointerList),
    with_locals(
        Locals,
        with_locals([
            let(Strs, PointerList, [PointerList | Ptrs])
        ],(
            ffi:allocate(c, 'XTextProperty', ['XTextProperty' , 0, 0, 0, 0], TProp),
            ffi:'Xutf8TextListToTextProperty'(Dp, Strs, Len, Style, TProp, _)
        ))
    ),
    TProp = TextPropReturn.

x_get_text_property(Dp, Win, Text, Property, Status) :-
    with_locals([
        let(TProp, 'XTextProperty', ['XTextProperty', 0, 0, 0, 0])
    ],(
        ffi:'XGetTextProperty'(Dp, Win, TProp, Property, St),
        (   St = 0 -> 
            Txt = ""
        ;   ffi:read_ptr('XTextProperty', TProp, ['XTextProperty', ValuePtr, _, _, _]),
            ffi:read_ptr(cstr, ValuePtr, Txt)
        )
    )),
    Status = St,
    Text = Txt.

x_get_transient_for_hint(Dp, Win, PropWindow, Status) :-
    with_locals([
        let(WRet, u64, 0)
    ],(
        ffi:'XGetTransientForHint'(Dp, Win, WRet, St),
        ffi:read_ptr(u64, WRet, Pw)
    )),
    Pw = PropWindow,
    St = Status.

x_set_text_property(Dp, Win, TextProp, Property) :- 
    ffi:'XSetTextProperty'(Dp, Win, TextProp, Property).

x_send_event(Dp, Win, Propagate, EventMask, EventSend) :-
    bool_int(Propagate, P),
    ffi:'XSendEvent'(Dp, Win, P, EventMask, EventSend, _).

x_next_event(Dp, EventReturn) :- call_with_error_context(x_next_event_(Dp, EventReturn), predicate-x_next_event/2).

x_next_event_(Dp, EventReturn) :-
    ffi:array_type(i64, 24, Pad),
    length(L, 24),
    maplist(call(=, 0), L),
    with_locals([
        let(EventPtr, 'XEvent', ['XEvent', [Pad | L]])
    ],(
       ffi:'XNextEvent'(Dp, EventPtr, _),
       ffi:read_ptr('XAnyEvent', EventPtr, ['XAnyEvent' , EventId | _]),
       event_type_id_atom(EventId, EventAtom),
       $decode_event(EventAtom, EventPtr, EventReturn)
    )).

term_expansion(event_type_id_atom(Id, Atom), (event_type_id_atom(Id, Atom) :- !)).

event_type_id_atom(2, keypress).
event_type_id_atom(3, keyrelease).
event_type_id_atom(4, buttonpress).
event_type_id_atom(5, buttonrelease).
event_type_id_atom(6, motionnotify).
event_type_id_atom(7, enternotify).
event_type_id_atom(17, destroynotify).
event_type_id_atom(18, unmapnotify).
event_type_id_atom(20, maprequest).
event_type_id_atom(22, configurenotify).
event_type_id_atom(28, propertynotify).
event_type_id_atom(33, clientmessage).

event_type_id_atom(EventId, EventAtom) :- rr_event_base(RrEventBase),  RrEventId is EventId - RrEventBase, rr_event_type_id_atom(RrEventId, EventAtom), !.

event_type_id_atom(_, unsupported_event).

rr_event_type_id_atom(0, rrscreenchangenotify).

% TODO SendEvent needs to be converted from int to bool bool_int

decode_event(maprequest, EventPtr, [maprequest, Type, Serial, SendEvent, Display, Parent, Window]) :- !,
    ffi:read_ptr('XMapRequestEvent', EventPtr, ['XMapRequestEvent', Type, Serial, Se, Display, Parent, Window]),
    bool_int(SendEvent, Se).

decode_event(unmapnotify, EventPtr, [unmapnotify, Type, Serial, SendEvent, Display, Event, Window, FromConfigure]) :- !,
    ffi:read_ptr('XUnmapEvent', EventPtr, [ 'XUnmapEvent' , Type, Serial, Se, Display, Event, Window, Fc ]),
    bool_int(SendEvent, Se),
    bool_int(FromConfigure, Fc).
    
decode_event(destroynotify, EventPtr, [destroynotify, Type, Serial, SendEvent, Display, Event, Window]) :- !,
    ffi:read_ptr('XDestroyWindowEvent', EventPtr, [ 'XDestroyWindowEvent', Type, Serial, Se, Display, Event, Window ]),
    bool_int(SendEvent, Se).

decode_event(enternotify, EventPtr, [enternotify, Type, Serial, SendEvent, Display, Window, Root, SubWindow, Time, X, Y, XRoot, YRoot, Mode, Detail, SameScreen, Focus, State]) :- !,
    ffi:read_ptr('XCrossingEvent', EventPtr, [ 'XCrossingEvent' , Type, Serial, Se, Display, Window, Root, SubWindow, Time, X, Y, XRoot, YRoot, Mode, Detail, Sc, F, State ]),
    bool_int(SendEvent, Se),
    bool_int(SameScreen, Sc),
    bool_int(Focus, F).

decode_event(propertynotify, EventPtr, [propertynotify, Serial, SendEvent, Display, Window, Atom, Time, State]) :- !,
    ffi:read_ptr('XPropertyEvent', EventPtr, [ 'XPropertyEvent' , _Type, Serial, Se, Display, Window, Atom, Time, State ]),
    bool_int(SendEvent, Se).

decode_event(clientmessage, EventPtr, [clientmessage, Type, Serial, SendEvent, Display, Window, MessageType, Format, L0, L1, L2]) :- !,
    ffi:read_ptr('XClientMessageEvent', EventPtr, [ 'XClientMessageEvent', Type, Serial, Se, Display, Window, MessageType, Format, ['ClientMessageData', L0, L1, L2, _, _]]),
    bool_int(SendEvent, Se).
    
decode_event(configurenotify, EventPtr, [configurenotify, Type, Serial, SendEvent, Display, Event, Window, X, Y, Width, Height, BorderWidth, Above, OverrideRedirect]) :- !,
    ffi:read_ptr('XConfigureEvent', EventPtr, [ 'XConfigureEvent', Type, Serial, Se, Display, Event, Window, X, Y, Width, Height, BorderWidth, Above, Or ]),
    bool_int(SendEvent, Se),
    bool_int(OverrideRedirect, Or).
    
decode_event(rrscreenchangenotify, _EventPtr, [rrscreenchangenotify]) :- !.
    
decode_event(keypress, EventPtr, [keypress | EventData ]) :- !,
    ffi:read_ptr('XKeyEvent', EventPtr, [ 'XKeyEvent',_,_,_,_,_,_,_,_,_,_,_,_,_,KeyCode,_]), 
    movement_common(EventPtr, KeyCode, EventData).

decode_event(keyrelease, EventPtr, [keyrelease | EventData ]) :- !,
    ffi:read_ptr('XKeyEvent', EventPtr, [ 'XKeyEvent',_,_,_,_,_,_,_,_,_,_,_,_,_,KeyCode,_]), 
    movement_common(EventPtr, KeyCode, EventData).

decode_event(buttonpress, EventPtr, [buttonpress | EventData]) :- !,
    ffi:read_ptr('XButtonEvent', EventPtr, [ 'XButtonEvent',_,_,_,_,_,_,_,_,_,_,_,_,_,Button,_]), 
    movement_common(EventPtr, Button, EventData).
    
decode_event(buttonrelease, EventPtr, [buttonrelease | EventData ]) :- !,
    ffi:read_ptr('XButtonEvent', EventPtr, [ 'XButtonEvent',_,_,_,_,_,_,_,_,_,_,_,_,_,Button,_]), 
    movement_common(EventPtr, Button, EventData).
    
decode_event(motionnotify, EventPtr, [ motionnotify | EventData]) :- !,
    $ffi:read_ptr('XMotionEvent', EventPtr, Data), 
    $(Data = [ 'XMotionEvent',_,_,_,_,_,_,_,_,_,_,_,_,_,IsHint,_]),
    $movement_common(EventPtr, IsHint, EventData).

decode_event(unsupported_event, _, "unsupported_event").

movement_common(EventPtr, Value, [Serial, SendEvent, Display, Window, Root, SubWindow, Time, X, Y, XRoot, YRoot, State, Value, SameScreen]) :-
    $ffi:read_ptr('XKeyEvent', EventPtr, ['XKeyEvent', _, Serial, Se, Display, Window, Root, SubWindow, Time, X, Y, XRoot, YRoot, State, _, Sc]),
    bool_int(SendEvent, Se),
    bool_int(SameScreen, Sc).


x_raise_window(Dp, Win) :- ffi:'XRaiseWindow'(Dp, Win, _).

x_move_resize_window(Dp, Win, X, Y, Width, Height) :- ffi:'XMoveResizeWindow'(Dp, Win, X, Y, Width, Height, _).

x_map_window(Dp, Win) :- ffi:'XMapWindow'(Dp, Win, _).

x_configure_window(Dp, Win, ValueMask, X, Y, Width, Height, BorderWidth, Sibling, StackMode) :-
    with_locals([
        let(WinChange, 'XWindowChanges', ['XWindowChanges' , X, Y, Width, Height, BorderWidth, Sibling, StackMode ])
    ],
        ffi:'XConfigureWindow'(Dp, Win, ValueMask, WinChange ,_)
    ).

x_set_window_border(Dp, Win, BorderPixel) :- ffi:'XSetWindowBorder'(Dp, Win, BorderPixel, _).

x_set_input_focus(Dp, Focus, RevertTo, Time) :- ffi:'XSetInputFocus'(Dp, Focus, RevertTo, Time, _).

x_kill_client(Dp, Resource) :- ffi:'XKillClient'(Dp, Resource, _).

x_sync(Dp, Discard) :- 
    bool_int(Discard, D),
    ffi:'XSync'(Dp, D, _).

x_get_wm_protocols(Dp, Win, ProtocolRet, CountRet) :-
    with_locals([
        let(ProtosPtrPtr, ptr, 0),
        let(CntPtr, i32, 0)
    ],(
        ffi:'XGetWMProtocols'(Dp, Win, ProtosPtrPtr, CntPtr, Ret),
        Ret \= 0,
        ffi:read_ptr(i32, CntPtr, Cnt),
        (Cnt == 0 -> 
            Protos = []
        ;   ffi:array_type(u64, Cnt, ArrayType),
            ffi:read_ptr(ptr, ProtosPtrPtr, ProtosPtr),
            ffi:read_ptr(ArrayType, ProtosPtr, [ ArrayType | Protos ]),
            ffi:'XFree'(ProtosPtr, _)
        )
    )),
    CountRet = Cnt,
    ProtocolRet = Protos.

x_get_wm_normal_hints(Dp, Win, HintsRet, Status) :-
    with_locals([
        let(HintsPtr, 'XSizeHints', ['XSizeHints', 0,0,0,0,0,0,0,0,0,0,0,['AspectRatio', 0, 0],['AspectRatio', 0, 0],0,0,0]),
        let(SupRetPtr, i64, 0)
    ],(
        ffi:'XGetWMNormalHints'(Dp, Win, HintsPtr, SupRetPtr, St),
        (St == 0 -> Hints = [],
            ffi:read_ptr('XSizeHints', HintsPtr, ['XSizeHints', 
                Flags, 
                X, Y, 
                Width, Height, 
                MinWidth, MinHeight, 
                MaxWidth,MaxHeight,
                WidthInc, HeightInc,
                ['AspectRatio', MinAspectX, MinAspectY],
                ['AspectRatio', MaxAspectX, MaxAspectY],
                BaseWidth, BaseHeight,
                WinGravity
            ]),
            Hints = [
                Flags,
                X,
                Y,
                Width,
                Height,
                MinWidth,
                MinHeight,
                MaxWidth,
                MaxHeight,
                WidthInc,
                HeightInc,
                MinAspectX,
                MinAspectY,
                MaxAspectX,
                MaxAspectY,
                BaseWidth,
                BaseHeight,
                WinGravity
            ]
        )
    )),
    Status = St,
    HintsRet = Hints.

x_warp_pointer(Dp, SrcWin, DestWin, SrcX, SrcY, SrcWidth, SrcHeight, DestX, DestY) :-
    ffi:'XWarpPointer'(Dp, SrcWin, DestWin, SrcX, SrcY, SrcWidth, SrcHeight, DestX, DestY, _).

create_configure_event(Dp, Win, ConfigureEvent) :- 
    event_type_id_atom(ConfigureNotify, configurenotify),
    ffi:allocate(c, 'XConfigureEvent', ['XConfigureEvent', ConfigureNotify, 0, 0, Dp, Win, Win, 0, 0, 0, 0, 0, 0, 0], ConfigureEventPtr),
    ConfigureEvent = ConfigureEventPtr.

create_clientmessage_event(Win, MsgType, Format, Datal0, Datal1, ClientMsg) :-
    event_type_id_atom(ClientMessage, clientmessage),
    ffi:allocate(c, 'XClientMessageEvent', ['XClientMessageEvent', ClientMessage, 0, 0, 0, Win, MsgType, Format, ['ClientMessageData', Datal0, Datal1, 0, 0, 0]], ClientMsgPtr),
    ClientMsg = ClientMsgPtr.

:- dynamic(rr_event_base/1).

xrr_query_extension(Dp, Event, Error) :-
    with_locals([
        let(EventPtr, i32, 0),
        let(ErrorPtr, i32, 0)
    ],
    (
        ffi:'XRRQueryExtension'(Dp, EventPtr, ErrorPtr, Res),
        ( Res = 1 -> true
        ; writeln("XRRQueryExtension() failed!"), false
        ),
        ffi:read_ptr(i32, EventPtr, Event),
        ffi:read_ptr(i32, ErrorPtr, Error),
        retractall(rr_event_base(_)),
        assertz(rr_event_base(Event))
    )).

xrr_select_input(Dp, Win, Mask) :- ffi:'XRRSelectInput'(Dp, Win, Mask).

xrr_get_screen_resources(Dp, Win, ScreenResources, Out) :-
    ffi:'XRRGetScreenResources'(Dp, Win, SR),
    (
        SR = 0 -> writeln("xrr_get_screen_resources: XRRGetScreenResources() returned NULL!"), false
        ; true
    ),
    ( ScreenResources = SR -> true
    ; ffi:'XRRFreeScreenResources'(SR), false
    ),
    ffi:read_ptr('XRRScreenResources', SR, Val),
    ['XRRScreenResources', _timestamp, _configTimestamp, _ncrtc, _crtcs, Noutput, Outputs | _] = Val,
    build_outputs(Noutput, Outputs, Outs),
    ( Out = Outs -> true
    ; ffi:'XRRFreeScreenResources'(SR), false
    ).

build_outputs(Counter, Ptr, Out) :-
    (Counter = 0 -> Out = []
    ; Out = [O|Os],
      ffi:read_ptr(u64, Ptr, O),
      Rem is Counter - 1,
      Next is Ptr + 8,
      build_outputs(Rem, Next, Os)
    ).

xrr_get_output_info(Dp, ScreenResources, OutIdx,OutInfo) :-
    ffi:read_ptr('XRRScreenResources', ScreenResources, Val),
    ['XRRScreenResources', _timestamp, _configTimestamp, _ncrtc, _crtcs, Noutput, Outputs | _] = Val,
    (
        (OutIdx < 0 ; Noutput =< OutIdx) -> format("xrr_get_output_info: output_index: ~d is out of bounds: 0..~d!~n", [OutIdx, Noutput]), false
        ; true
    ),
    EntryPtr is Outputs + (8 * OutIdx),
    ffi:read_ptr(u64, EntryPtr, Output),
    ffi:'XRRGetOutputInfo'(Dp, ScreenResources, Output, OutputPtr),
    (
        OutputPtr = 0 -> writeln("xrr_get_output_info: XRRGetOutputInfo() returned NULL!"), false
        ; true
    ),
    ffi:read_ptr('XRROutputInfo', OutputPtr, OutputStruct),
    ffi:'XRRFreeOutputInfo'(OutputPtr),
    ['XRROutputInfo', _timestamp, Crtc, AtomName, _nameLen, _width, _height, Connection | _] = OutputStruct,
    atom_chars(AtomName, Name),
    OutInfo = [Name, Connection, Crtc].

xrr_free_screen_resources(Sr) :- ffi:'XRRFreeScreenResources'(Sr).

xft_color_alloc_name(Dp, Vis, ColorMap, Name, Res) :-
    with_locals([
        let(ResPtr, 'XftColor', ['XftColor', 0, ['XRenderColor', 0,0,0,0]])
    ],
    (
        ffi:'XftColorAllocName'(Dp, Vis, ColorMap, Name, ResPtr, _),
        ffi:read_ptr('XftColor', ResPtr, ResStruct),
        ['XftColor', Res, _] = ResStruct
    )).

xrr_get_crtc_info(Dp, ScreenResources, Crtc, CrtcInfo) :-
    ffi:'XRRGetCrtcInfo'(Dp, ScreenResources, Crtc, CrtcInfoPtr),
    ( CrtcInfoPtr = 0 -> writeln("xrr_get_crtc_info: XRRGetCrtcInfo() returned NULL!"), false; true),
    ffi:read_ptr('XRRCrtcInfo', CrtcInfoPtr, Info),
    ffi:'XRRFreeCrtcInfo'(CrtcInfoPtr),
    ['XRRCrtcInfo', _timestamp, X, Y, W, H | _ ] = Info,
    CrtcInfo = [X, Y, W, H].
