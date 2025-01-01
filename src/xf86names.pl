% MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

% Created from <X11/XF86keysym.h> on 2023-01-25 (commented out keymaps were not kept)
% For additional notes, see the original file

:- module(xf86names, []).

%! name_ksym(++Name:string, -Ksym:integer) is det
%
%  Maps key names to key codes.
%
%  @arg Name name of key as defined in <X11/XF86keysym.h>
%  @arg Ksym key code associated with the given name

% ModeLock
name_ksym("ModeLock",           0x1008FF01).  % Mode Switch Lock

% Backlight controls.
name_ksym("MonBrightnessUp",    0x1008FF02).  % Monitor/panel brightness
name_ksym("MonBrightnessDown",  0x1008FF03).  % Monitor/panel brightness
name_ksym("KbdLightOnOff",      0x1008FF04).  % Keyboards may be lit
name_ksym("KbdBrightnessUp",    0x1008FF05).  % Keyboards may be lit
name_ksym("KbdBrightnessDown",  0x1008FF06).  % Keyboards may be lit
name_ksym("MonBrightnessCycle", 0x1008FF07).  % Monitor/panel brightness

% Keys found on some "Internet" keyboards.
name_ksym("Standby",            0x1008FF10).  % System into standby mode
name_ksym("AudioLowerVolume",   0x1008FF11).  % Volume control down
name_ksym("AudioMute",          0x1008FF12).  % Mute sound from the system
name_ksym("AudioRaiseVolume",   0x1008FF13).  % Volume control up
name_ksym("AudioPlay",          0x1008FF14).  % Start playing of audio >
name_ksym("AudioStop",          0x1008FF15).  % Stop playing audio
name_ksym("AudioPrev",          0x1008FF16).  % Previous track
name_ksym("AudioNext",          0x1008FF17).  % Next track
name_ksym("HomePage",           0x1008FF18).  % Display user's home page
name_ksym("Mail",               0x1008FF19).  % Invoke user's mail program
name_ksym("Start",              0x1008FF1A).  % Start application
name_ksym("Search",             0x1008FF1B).  % Search
name_ksym("AudioRecord",        0x1008FF1C).  % Record audio application

% These are sometimes found on PDA's (e.g. Palm, PocketPC or elsewhere)
name_ksym("Calculator",         0x1008FF1D).  % Invoke calculator program
name_ksym("Memo",               0x1008FF1E).  % Invoke Memo taking program
name_ksym("ToDoList",           0x1008FF1F).  % Invoke To Do List program
name_ksym("Calendar",           0x1008FF20).  % Invoke Calendar program
name_ksym("PowerDown",          0x1008FF21).  % Deep sleep the system
name_ksym("ContrastAdjust",     0x1008FF22).  % Adjust screen contrast
name_ksym("RockerUp",           0x1008FF23).  % Rocker switches exist up
name_ksym("RockerDown",         0x1008FF24).  % and down
name_ksym("RockerEnter",        0x1008FF25).  % and let you press them

% Some more "Internet" keyboard symbols
name_ksym("Back",               0x1008FF26).  % Like back on a browser
name_ksym("Forward",            0x1008FF27).  % Like forward on a browser
name_ksym("Stop",               0x1008FF28).  % Stop current operation
name_ksym("Refresh",            0x1008FF29).  % Refresh the page
name_ksym("PowerOff",           0x1008FF2A).  % Power off system entirely
name_ksym("WakeUp",             0x1008FF2B).  % Wake up system from sleep
name_ksym("Eject",              0x1008FF2C).  % Eject device (e.g. DVD)
name_ksym("ScreenSaver",        0x1008FF2D).  % Invoke screensaver
name_ksym("WWW",                0x1008FF2E).  % Invoke web browser
name_ksym("Sleep",              0x1008FF2F).  % Put system to sleep
name_ksym("Favorite",           0x1008FF30).  % Show favorite locations
name_ksym("AudioPause",         0x1008FF31).  % Pause audio playing
name_ksym("AudioMedia",         0x1008FF32).  % Launch media collection app
name_ksym("MyComputer",         0x1008FF33).  % Display "My Computer" window
name_ksym("VendorHome",         0x1008FF34).  % Display vendor home web site
name_ksym("LightBulb",          0x1008FF35).  % Light bulb keys exist
name_ksym("Shop",               0x1008FF36).  % Display shopping web site
name_ksym("History",            0x1008FF37).  % Show history of web surfing
name_ksym("OpenURL",            0x1008FF38).  % Open selected URL
name_ksym("AddFavorite",        0x1008FF39).  % Add URL to favorites list
name_ksym("HotLink",            0x1008FF3A).  % Show "hot" links
name_ksym("BrightnessAdjust",   0x1008FF3B).  % Invoke brightness adj. UI
name_ksym("Finance",            0x1008FF3C).  % Display financial site
name_ksym("Community",          0x1008FF3D).  % Display user's community
name_ksym("AudioRewind",        0x1008FF3E).  % "rewind" audio track
name_ksym("BackForward",        0x1008FF3F).  % ???
name_ksym("Launch0",            0x1008FF40).  % Launch Application
name_ksym("Launch1",            0x1008FF41).  % Launch Application
name_ksym("Launch2",            0x1008FF42).  % Launch Application
name_ksym("Launch3",            0x1008FF43).  % Launch Application
name_ksym("Launch4",            0x1008FF44).  % Launch Application
name_ksym("Launch5",            0x1008FF45).  % Launch Application
name_ksym("Launch6",            0x1008FF46).  % Launch Application
name_ksym("Launch7",            0x1008FF47).  % Launch Application
name_ksym("Launch8",            0x1008FF48).  % Launch Application
name_ksym("Launch9",            0x1008FF49).  % Launch Application
name_ksym("LaunchA",            0x1008FF4A).  % Launch Application
name_ksym("LaunchB",            0x1008FF4B).  % Launch Application
name_ksym("LaunchC",            0x1008FF4C).  % Launch Application
name_ksym("LaunchD",            0x1008FF4D).  % Launch Application
name_ksym("LaunchE",            0x1008FF4E).  % Launch Application
name_ksym("LaunchF",            0x1008FF4F).  % Launch Application

name_ksym("ApplicationLeft",    0x1008FF50).  % switch to application, left
name_ksym("ApplicationRight",   0x1008FF51).  % switch to application, right
name_ksym("Book",               0x1008FF52).  % Launch bookreader
name_ksym("CD",                 0x1008FF53).  % Launch CD/DVD player
name_ksym("Calculater",         0x1008FF54).  % Launch Calculater
name_ksym("Clear",              0x1008FF55).  % Clear window, screen
name_ksym("Close",              0x1008FF56).  % Close window
name_ksym("Copy",               0x1008FF57).  % Copy selection
name_ksym("Cut",                0x1008FF58).  % Cut selection
name_ksym("Display",            0x1008FF59).  % Output switch key
name_ksym("DOS",                0x1008FF5A).  % Launch DOS (emulation)
name_ksym("Document",           0x1008FF5B).  % Open documents window
name_ksym("Excel",              0x1008FF5C).  % Launch spread sheet
name_ksym("Explorer",           0x1008FF5D).  % Launch file explorer
name_ksym("Game",               0x1008FF5E).  % Launch game
name_ksym("Go",                 0x1008FF5F).  % Go to URL
name_ksym("iTouch",             0x1008FF60).  % Logitech iTouch- don't use
name_ksym("LogOff",             0x1008FF61).  % Log off system
name_ksym("Market",             0x1008FF62).  % ??
name_ksym("Meeting",            0x1008FF63).  % enter meeting in calendar
name_ksym("MenuKB",             0x1008FF65).  % distinguish keyboard from PB
name_ksym("MenuPB",             0x1008FF66).  % distinguish PB from keyboard
name_ksym("MySite",             0x1008FF67).  % Favourites
name_ksym("New",                0x1008FF68).  % New (folder, document...
name_ksym("News",               0x1008FF69).  % News
name_ksym("OfficeHome",         0x1008FF6A).  % Office home (old Staroffice)
name_ksym("Open",               0x1008FF6B).  % Open
name_ksym("Option",             0x1008FF6C).  % ??
name_ksym("Paste",              0x1008FF6D).  % Paste
name_ksym("Phone",              0x1008FF6E).  % Launch phone; dial number
name_ksym("Q",                  0x1008FF70).  % Compaq's Q - don't use
name_ksym("Reply",              0x1008FF72).  % Reply e.g., mail
name_ksym("Reload",             0x1008FF73).  % Reload web page, file, etc.
name_ksym("RotateWindow",       0x1008FF74).  % Rotate windows e.g. xrandr
name_ksym("RotationPB",         0x1008FF75).  % don't use
name_ksym("RotationKB",         0x1008FF76).  % don't use
name_ksym("Save",               0x1008FF77).  % Save (file, document, state
name_ksym("ScrollUp",           0x1008FF78).  % Scroll window/contents up
name_ksym("ScrollDown",         0x1008FF79).  % Scrool window/contentd down
name_ksym("ScrollClick",        0x1008FF7A).  % Use XKB mousekeys instead
name_ksym("Send",               0x1008FF7B).  % Send mail, file, object
name_ksym("Spell",              0x1008FF7C).  % Spell checker
name_ksym("SplitScreen",        0x1008FF7D).  % Split window or screen
name_ksym("Support",            0x1008FF7E).  % Get support (??)
name_ksym("TaskPane",           0x1008FF7F).  % Show tasks
name_ksym("Terminal",           0x1008FF80).  % Launch terminal emulator
name_ksym("Tool",               0x1008FF81).  % toolbox of desktop/app.
name_ksym("Travel",             0x1008FF82).  % ??
name_ksym("UserPB",             0x1008FF84).  % ??
name_ksym("User1KB",            0x1008FF85).  % ??
name_ksym("User2KB",            0x1008FF86).  % ??
name_ksym("Video",              0x1008FF87).  % Launch video player
name_ksym("WheelButton",        0x1008FF88).  % button from a mouse wheel
name_ksym("Word",               0x1008FF89).  % Launch word processor
name_ksym("Xfer",               0x1008FF8A).
name_ksym("ZoomIn",             0x1008FF8B).  % zoom in view, map, etc.
name_ksym("ZoomOut",            0x1008FF8C).  % zoom out view, map, etc.

name_ksym("Away",               0x1008FF8D).  % mark yourself as away
name_ksym("Messenger",          0x1008FF8E).  % as in instant messaging
name_ksym("WebCam",             0x1008FF8F).  % Launch web camera app.
name_ksym("MailForward",        0x1008FF90).  % Forward in mail
name_ksym("Picture",            0x1008FF91).  % Show pictures
name_ksym("Music",              0x1008FF92).  % Launch music application

name_ksym("Battery",            0x1008FF93).  % Display battery information
name_ksym("Bluetooth",          0x1008FF94).  % Enable/disable Bluetooth
name_ksym("WLAN",               0x1008FF95).  % Enable/disable WLAN
name_ksym("UWB",                0x1008FF96).  % Enable/disable UWB

name_ksym("AudioForward",       0x1008FF97).  % fast-forward audio track
name_ksym("AudioRepeat",        0x1008FF98).  % toggle repeat mode
name_ksym("AudioRandomPlay",    0x1008FF99).  % toggle shuffle mode
name_ksym("Subtitle",           0x1008FF9A).  % cycle through subtitle
name_ksym("AudioCycleTrack",    0x1008FF9B).  % cycle through audio tracks
name_ksym("CycleAngle",         0x1008FF9C).  % cycle through angles
name_ksym("FrameBack",          0x1008FF9D).  % video: go one frame back
name_ksym("FrameForward",       0x1008FF9E).  % video: go one frame forward
name_ksym("Time",               0x1008FF9F).  % display, or shows an entry for time seeking
name_ksym("Select",             0x1008FFA0).  % Select button on joypads and remotes
name_ksym("View",               0x1008FFA1).  % Show a view options/properties
name_ksym("TopMenu",            0x1008FFA2).  % Go to a top-level menu in a video

name_ksym("Red",                0x1008FFA3).  % Red button
name_ksym("Green",              0x1008FFA4).  % Green button
name_ksym("Yellow",             0x1008FFA5).  % Yellow button
name_ksym("Blue",               0x1008FFA6).  % Blue button

name_ksym("Suspend",            0x1008FFA7).  % Sleep to RAM
name_ksym("Hibernate",          0x1008FFA8).  % Sleep to disk
name_ksym("TouchpadToggle",     0x1008FFA9).  % Toggle between touchpad/trackstick
name_ksym("TouchpadOn",         0x1008FFB0).  % The touchpad got switched on
name_ksym("TouchpadOff",        0x1008FFB1).  % The touchpad got switched off

name_ksym("AudioMicMute",       0x1008FFB2).  % Mute the Mic from the system

name_ksym("Keyboard",           0x1008FFB3).    % User defined keyboard related action

name_ksym("WWAN",               0x1008FFB4).  % Toggle WWAN (LTE, UMTS, etc.) radio
name_ksym("RFKill",             0x1008FFB5).  % Toggle radios on/off

name_ksym("AudioPreset",        0x1008FFB6).  % Select equalizer preset, e.g. theatre-mode

name_ksym("RotationLockToggle ",0x1008FFB7).  % Toggle screen rotation lock on/off

name_ksym("FullScreen",         0x1008FFB8).  % Toggle fullscreen

% Keys for special action keys (hot keys)
% Virtual terminals on some operating systems
name_ksym("Switch_VT_1",        0x1008FE01).
name_ksym("Switch_VT_2",        0x1008FE02).
name_ksym("Switch_VT_3",        0x1008FE03).
name_ksym("Switch_VT_4",        0x1008FE04).
name_ksym("Switch_VT_5",        0x1008FE05).
name_ksym("Switch_VT_6",        0x1008FE06).
name_ksym("Switch_VT_7",        0x1008FE07).
name_ksym("Switch_VT_8",        0x1008FE08).
name_ksym("Switch_VT_9",        0x1008FE09).
name_ksym("Switch_VT_10",       0x1008FE0A).
name_ksym("Switch_VT_11",       0x1008FE0B).
name_ksym("Switch_VT_12",       0x1008FE0C).

name_ksym("Ungrab",             0x1008FE20).  % force ungrab
name_ksym("ClearGrab",          0x1008FE21).  % kill application with grab
name_ksym("Next_VMode",         0x1008FE22).  % next video mode available
name_ksym("Prev_VMode",         0x1008FE23).  % prev. video mode available
name_ksym("LogWindowTree",      0x1008FE24).  % print window tree to log
name_ksym("LogGrabInfo",        0x1008FE25).  % print all active grabs to log

% Reserved range for evdev symbols: 0x10081000-0x10081FFF
name_ksym("BrightnessAuto",          0x100810F4).  % v3.16 KEY_BRIGHTNESS_AUTO
name_ksym("DisplayOff",              0x100810F5).  % v2.6.23 KEY_DISPLAY_OFF
name_ksym("Info",                    0x10081166).  %       KEY_INFO
name_ksym("AspectRatio",             0x10081177).  % v5.1  KEY_ASPECT_RATIO
name_ksym("DVD",                     0x10081185).  %       KEY_DVD
name_ksym("Audio",                   0x10081188).  %       KEY_AUDIO
name_ksym("ChannelUp",               0x10081192).  %       KEY_CHANNELUP
name_ksym("ChannelDown",             0x10081193).  %       KEY_CHANNELDOWN
name_ksym("Break",                   0x1008119B).  %       KEY_BREAK
name_ksym("VideoPhone",              0x100811A0).  % v2.6.20 KEY_VIDEOPHONE
name_ksym("ZoomReset",               0x100811A4).  % v2.6.20 KEY_ZOOMRESET
name_ksym("Editor",                  0x100811A6).  % v2.6.20 KEY_EDITOR
name_ksym("GraphicsEditor",          0x100811A8).  % v2.6.20 KEY_GRAPHICSEDITOR
name_ksym("Presentation",            0x100811A9).  % v2.6.20 KEY_PRESENTATION
name_ksym("Database",                0x100811AA).  % v2.6.20 KEY_DATABASE
name_ksym("Voicemail",               0x100811AC).  % v2.6.20 KEY_VOICEMAIL
name_ksym("Addressbook",             0x100811AD).  % v2.6.20 KEY_ADDRESSBOOK
name_ksym("DisplayToggle",           0x100811AF).  % v2.6.20 KEY_DISPLAYTOGGLE
name_ksym("SpellCheck",              0x100811B0).  % v2.6.24 KEY_SPELLCHECK
name_ksym("ContextMenu",             0x100811B6).  % v2.6.24 KEY_CONTEXT_MENU
name_ksym("MediaRepeat",             0x100811B7).  % v2.6.26 KEY_MEDIA_REPEAT
name_ksym("10ChannelsUp",            0x100811B8).  % v2.6.38 KEY_10CHANNELSUP
name_ksym("10ChannelsDown",          0x100811B9).  % v2.6.38 KEY_10CHANNELSDOWN
name_ksym("Images",                  0x100811BA).  % v2.6.39 KEY_IMAGES
name_ksym("NotificationCenter",      0x100811BC).  % v5.10 KEY_NOTIFICATION_CENTER
name_ksym("PickupPhone",             0x100811BD).  % v5.10 KEY_PICKUP_PHONE
name_ksym("HangupPhone",             0x100811BE).  % v5.10 KEY_HANGUP_PHONE
name_ksym("Fn",                      0x100811D0).  %       KEY_FN
name_ksym("Fn_Esc",                  0x100811D1).  %       KEY_FN_ESC
name_ksym("FnRightShift",            0x100811E5).  % v5.10 KEY_FN_RIGHT_SHIFT
name_ksym("Numeric0",                0x10081200).  % v2.6.28 KEY_NUMERIC_0
name_ksym("Numeric1",                0x10081201).  % v2.6.28 KEY_NUMERIC_1
name_ksym("Numeric2",                0x10081202).  % v2.6.28 KEY_NUMERIC_2
name_ksym("Numeric3",                0x10081203).  % v2.6.28 KEY_NUMERIC_3
name_ksym("Numeric4",                0x10081204).  % v2.6.28 KEY_NUMERIC_4
name_ksym("Numeric5",                0x10081205).  % v2.6.28 KEY_NUMERIC_5
name_ksym("Numeric6",                0x10081206).  % v2.6.28 KEY_NUMERIC_6
name_ksym("Numeric7",                0x10081207).  % v2.6.28 KEY_NUMERIC_7
name_ksym("Numeric8",                0x10081208).  % v2.6.28 KEY_NUMERIC_8
name_ksym("Numeric9",                0x10081209).  % v2.6.28 KEY_NUMERIC_9
name_ksym("NumericStar",             0x1008120A).  % v2.6.28 KEY_NUMERIC_STAR
name_ksym("NumericPound",            0x1008120B).  % v2.6.28 KEY_NUMERIC_POUND
name_ksym("NumericA",                0x1008120C).  % v4.1  KEY_NUMERIC_A
name_ksym("NumericB",                0x1008120D).  % v4.1  KEY_NUMERIC_B
name_ksym("NumericC",                0x1008120E).  % v4.1  KEY_NUMERIC_C
name_ksym("NumericD",                0x1008120F).  % v4.1  KEY_NUMERIC_D
name_ksym("CameraFocus",             0x10081210).  % v2.6.33 KEY_CAMERA_FOCUS
name_ksym("WPSButton",               0x10081211).  % v2.6.34 KEY_WPS_BUTTON
name_ksym("CameraZoomIn",            0x10081215).  % v2.6.39 KEY_CAMERA_ZOOMIN
name_ksym("CameraZoomOut",           0x10081216).  % v2.6.39 KEY_CAMERA_ZOOMOUT
name_ksym("CameraUp",                0x10081217).  % v2.6.39 KEY_CAMERA_UP
name_ksym("CameraDown",              0x10081218).  % v2.6.39 KEY_CAMERA_DOWN
name_ksym("CameraLeft",              0x10081219).  % v2.6.39 KEY_CAMERA_LEFT
name_ksym("CameraRight",             0x1008121A).  % v2.6.39 KEY_CAMERA_RIGHT
name_ksym("AttendantOn",             0x1008121B).  % v3.10 KEY_ATTENDANT_ON
name_ksym("AttendantOff",            0x1008121C).  % v3.10 KEY_ATTENDANT_OFF
name_ksym("AttendantToggle",         0x1008121D).  % v3.10 KEY_ATTENDANT_TOGGLE
name_ksym("LightsToggle",            0x1008121E).  % v3.10 KEY_LIGHTS_TOGGLE
name_ksym("ALSToggle",               0x10081230).  % v3.13 KEY_ALS_TOGGLE
name_ksym("Buttonconfig",            0x10081240).  % v3.16 KEY_BUTTONCONFIG
name_ksym("Taskmanager",             0x10081241).  % v3.16 KEY_TASKMANAGER
name_ksym("Journal",                 0x10081242).  % v3.16 KEY_JOURNAL
name_ksym("ControlPanel",            0x10081243).  % v3.16 KEY_CONTROLPANEL
name_ksym("AppSelect",               0x10081244).  % v3.16 KEY_APPSELECT
name_ksym("Screensaver",             0x10081245).  % v3.16 KEY_SCREENSAVER
name_ksym("VoiceCommand",            0x10081246).  % v3.16 KEY_VOICECOMMAND
name_ksym("Assistant",               0x10081247).  % v4.13 KEY_ASSISTANT
name_ksym("EmojiPicker",             0x10081249).  % v5.13 KEY_EMOJI_PICKER
name_ksym("Dictate",                 0x1008124A).  % v5.17 KEY_DICTATE
name_ksym("BrightnessMin",           0x10081250).  % v3.16 KEY_BRIGHTNESS_MIN
name_ksym("BrightnessMax",           0x10081251).  % v3.16 KEY_BRIGHTNESS_MAX
name_ksym("KbdInputAssistPrev",      0x10081260).  % v3.18 KEY_KBDINPUTASSIST_PREV
name_ksym("KbdInputAssistNext",      0x10081261).  % v3.18 KEY_KBDINPUTASSIST_NEXT
name_ksym("KbdInputAssistPrevgroup", 0x10081262).  % v3.18 KEY_KBDINPUTASSIST_PREVGROUP
name_ksym("KbdInputAssistNextgroup", 0x10081263).  % v3.18 KEY_KBDINPUTASSIST_NEXTGROUP
name_ksym("KbdInputAssistAccept",    0x10081264).  % v3.18 KEY_KBDINPUTASSIST_ACCEPT
name_ksym("KbdInputAssistCancel",    0x10081265).  % v3.18 KEY_KBDINPUTASSIST_CANCEL
name_ksym("RightUp",                 0x10081266).  % v4.7  KEY_RIGHT_UP
name_ksym("RightDown",               0x10081267).  % v4.7  KEY_RIGHT_DOWN
name_ksym("LeftUp",                  0x10081268).  % v4.7  KEY_LEFT_UP
name_ksym("LeftDown",                0x10081269).  % v4.7  KEY_LEFT_DOWN
name_ksym("RootMenu",                0x1008126A).  % v4.7  KEY_ROOT_MENU
name_ksym("MediaTopMenu",            0x1008126B).  % v4.7  KEY_MEDIA_TOP_MENU
name_ksym("Numeric11",               0x1008126C).  % v4.7  KEY_NUMERIC_11
name_ksym("Numeric12",               0x1008126D).  % v4.7  KEY_NUMERIC_12
name_ksym("AudioDesc",               0x1008126E).  % v4.7  KEY_AUDIO_DESC
name_ksym("3DMode",                  0x1008126F).  % v4.7  KEY_3D_MODE
name_ksym("NextFavorite",            0x10081270).  % v4.7  KEY_NEXT_FAVORITE
name_ksym("StopRecord",              0x10081271).  % v4.7  KEY_STOP_RECORD
name_ksym("PauseRecord",             0x10081272).  % v4.7  KEY_PAUSE_RECORD
name_ksym("VOD",                     0x10081273).  % v4.7  KEY_VOD
name_ksym("Unmute",                  0x10081274).  % v4.7  KEY_UNMUTE
name_ksym("FastReverse",             0x10081275).  % v4.7  KEY_FASTREVERSE
name_ksym("SlowReverse",             0x10081276).  % v4.7  KEY_SLOWREVERSE
name_ksym("Data",                    0x10081277).  % v4.7  KEY_DATA
name_ksym("OnScreenKeyboard",        0x10081278).  % v4.12 KEY_ONSCREEN_KEYBOARD
name_ksym("PrivacyScreenToggle",     0x10081279).  % v5.5  KEY_PRIVACY_SCREEN_TOGGLE
name_ksym("SelectiveScreenshot",     0x1008127A).  % v5.6  KEY_SELECTIVE_SCREENSHOT
name_ksym("Macro1",                  0x10081290).  % v5.5  KEY_MACRO1
name_ksym("Macro2",                  0x10081291).  % v5.5  KEY_MACRO2
name_ksym("Macro3",                  0x10081292).  % v5.5  KEY_MACRO3
name_ksym("Macro4",                  0x10081293).  % v5.5  KEY_MACRO4
name_ksym("Macro5",                  0x10081294).  % v5.5  KEY_MACRO5
name_ksym("Macro6",                  0x10081295).  % v5.5  KEY_MACRO6
name_ksym("Macro7",                  0x10081296).  % v5.5  KEY_MACRO7
name_ksym("Macro8",                  0x10081297).  % v5.5  KEY_MACRO8
name_ksym("Macro9",                  0x10081298).  % v5.5  KEY_MACRO9
name_ksym("Macro10",                 0x10081299).  % v5.5  KEY_MACRO10
name_ksym("Macro11",                 0x1008129A).  % v5.5  KEY_MACRO11
name_ksym("Macro12",                 0x1008129B).  % v5.5  KEY_MACRO12
name_ksym("Macro13",                 0x1008129C).  % v5.5  KEY_MACRO13
name_ksym("Macro14",                 0x1008129D).  % v5.5  KEY_MACRO14
name_ksym("Macro15",                 0x1008129E).  % v5.5  KEY_MACRO15
name_ksym("Macro16",                 0x1008129F).  % v5.5  KEY_MACRO16
name_ksym("Macro17",                 0x100812A0).  % v5.5  KEY_MACRO17
name_ksym("Macro18",                 0x100812A1).  % v5.5  KEY_MACRO18
name_ksym("Macro19",                 0x100812A2).  % v5.5  KEY_MACRO19
name_ksym("Macro20",                 0x100812A3).  % v5.5  KEY_MACRO20
name_ksym("Macro21",                 0x100812A4).  % v5.5  KEY_MACRO21
name_ksym("Macro22",                 0x100812A5).  % v5.5  KEY_MACRO22
name_ksym("Macro23",                 0x100812A6).  % v5.5  KEY_MACRO23
name_ksym("Macro24",                 0x100812A7).  % v5.5  KEY_MACRO24
name_ksym("Macro25",                 0x100812A8).  % v5.5  KEY_MACRO25
name_ksym("Macro26",                 0x100812A9).  % v5.5  KEY_MACRO26
name_ksym("Macro27",                 0x100812AA).  % v5.5  KEY_MACRO27
name_ksym("Macro28",                 0x100812AB).  % v5.5  KEY_MACRO28
name_ksym("Macro29",                 0x100812AC).  % v5.5  KEY_MACRO29
name_ksym("Macro30",                 0x100812AD).  % v5.5  KEY_MACRO30
name_ksym("MacroRecordStart",        0x100812B0).  % v5.5  KEY_MACRO_RECORD_START
name_ksym("MacroRecordStop",         0x100812B1).  % v5.5  KEY_MACRO_RECORD_STOP
name_ksym("MacroPresetCycle",        0x100812B2).  % v5.5  KEY_MACRO_PRESET_CYCLE
name_ksym("MacroPreset1",            0x100812B3).  % v5.5  KEY_MACRO_PRESET1
name_ksym("MacroPreset2",            0x100812B4).  % v5.5  KEY_MACRO_PRESET2
name_ksym("MacroPreset3",            0x100812B5).  % v5.5  KEY_MACRO_PRESET3
name_ksym("KbdLcdMenu1",             0x100812B8).  % v5.5  KEY_KBD_LCD_MENU1
name_ksym("KbdLcdMenu2",             0x100812B9).  % v5.5  KEY_KBD_LCD_MENU2
name_ksym("KbdLcdMenu3",             0x100812BA).  % v5.5  KEY_KBD_LCD_MENU3
name_ksym("KbdLcdMenu4",             0x100812BB).  % v5.5  KEY_KBD_LCD_MENU4
name_ksym("KbdLcdMenu5",             0x100812BC).  % v5.5  KEY_KBD_LCD_MENU5

