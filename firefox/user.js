// Play nice with xmonad, i.e., disable menu bar toggling on Alt.
user_pref("ui.key.menuAccessKeyFocuses", false);

// Bye pocket.
user_pref("extensions.pocket.enabled", false);

// Remove page headers/footers when printing.
user_pref("print.print_footerleft", "");
user_pref("print.print_footerright", "");
user_pref("print.print_headerleft", "");
user_pref("print.print_headerright", "");

// Force hardware decoding.
user_pref("media.hardware-video-decoding.force-enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("gfx.x11-egl.force-enabled", true);
user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);

// Disable fullscreen warning.
user_pref("full-screen-api.warning.timeout", 0);
