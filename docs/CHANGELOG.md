# [v0.4](https://github.com/Seeker04/plwm/releases/tag/v0.4) (2025-05-21)

## Added
- Hooks implementation ([#56](https://github.com/Seeker04/plwm/issues/56))
- WM_TAKE_FOCUS protocol implementation ([#77](https://github.com/Seeker04/plwm/issues/77))
- Mirrored monitor handling ([#8](https://github.com/Seeker04/plwm/issues/8))
- Auto tests in CI with Docker and GitHub Actions ([#67](https://github.com/Seeker04/plwm/issues/67), [#86](https://github.com/Seeker04/plwm/issues/86))
- More unit tests, coverage at 29% ([#62](https://github.com/Seeker04/plwm/issues/62), [#63](https://github.com/Seeker04/plwm/issues/63))
- Documentation updates including a video demo of layouts and animations

## Removed
- `startupcmd` setting superseded by `hooks` on `start`

## Changed
- Replaced Xinerama with XRandR ([#50](https://github.com/Seeker04/plwm/issues/50))

## Fixed
- Ill-structured rules and layout overrides in config were silently ignored
- Fixed handling of hidden windows (e.g. Steam, Gimp) with `UnmapNotify` ([#6](https://github.com/Seeker04/plwm/issues/6), [#53](https://github.com/Seeker04/plwm/issues/53))
- Moving window to other monitor not always updated layout ([#79](https://github.com/Seeker04/plwm/issues/79))
- Some non-fatal issues used `PL_warning` which killed plwm, added simple warning output instead
- Position fix for above/below monitor configs ([#84](https://github.com/Seeker04/plwm/issues/84))
- Mouse cursor was not visible until first window opening ([#89](https://github.com/Seeker04/plwm/issues/89))

# [v0.3](https://github.com/Seeker04/plwm/releases/tag/v0.3) (2025-01-23)

## Added
- Support for `$XDG_CONFIG_HOME` and `/etc/` configs
- Separate border width setting for focused window ([#27](https://github.com/Seeker04/plwm/issues/27))
- Support multiple selection in menus `pull_from` and `delete_workspace` ([#41](https://github.com/Seeker04/plwm/issues/41))
- `menu:close_windows` and `menu:keep_windows` ([#40](https://github.com/Seeker04/plwm/issues/40))
- Simple move/resize animations ([#45](https://github.com/Seeker04/plwm/issues/45))
- Feature to run custom action on modkey + mouse scroll ([#71](https://github.com/Seeker04/plwm/issues/71))
- Documentation comments for predicates ([#68](https://github.com/Seeker04/plwm/issues/68))
- Unit tests ([#21](https://github.com/Seeker04/plwm/issues/21))

## Changed
- Smaller code/documentation refactors

## Fixed
- _NET_WORKAREA was not always updated (e.g. when switching monitors)
- XFree deallocation was missing after XineramaQueryScreens call
- Keep monitor focus when a window is destroyed on another ([#55](https://github.com/Seeker04/plwm/issues/55))
- Window moved to other workspace no longer steals input ([#23](https://github.com/Seeker04/plwm/issues/23))
- Smaller issues in README.md

---

# [v0.2](https://github.com/Seeker04/plwm/releases/tag/v0.2) (2024-07-12)

## Added

- **Command FIFO implementation** ([#46](https://github.com/Seeker04/plwm/issues/46))
- Logging of caught exceptions
- Added `attach_bottom` setting ([#13](https://github.com/Seeker04/plwm/issues/13))
- Extended documentation (e.g. dependency install guide, .dekstop file example)
- Screenshot with default settings
- Logo

## Removed
- Gaps from the default config

## Changed
- Omit monitors and workspaces from menus when there's only a single choice ([#2](https://github.com/Seeker04/plwm/issues/2))
- Various code cleanup and refactoring

## Fixed
- Fixed a bug when a visible window got destroyed on not the active monitor
- Snapping now considers whether the window is moved or resized ([#42](https://github.com/Seeker04/plwm/issues/42))
- Fixed inconsistency between reindex_workspace keymap and its documentation
- Fail instead of throw in win_properties/2 to avoid rare crashes ([#38](https://github.com/Seeker04/plwm/issues/38))
- Added error handling for `XGetTextProperty` call ([#36](https://github.com/Seeker04/plwm/issues/36)) - fixes rare crashes
- Comma separation for `SEE ALSO` list in plwm(1)
- Newly spawned floating windows now respect space reserved for bars ([#1](https://github.com/Seeker04/plwm/issues/1))
- Compilation on Ubuntu (different swipl header path, order of lib dependencies)
- Several smaller issues in README.md

---

# [v0.1](https://github.com/Seeker04/plwm/releases/tag/v0.1) (2023-11-10)

Initial version

