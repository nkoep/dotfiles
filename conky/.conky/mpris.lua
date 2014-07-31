local bit = require "bit"
local bin_or = bit.bor
local string = require "string"

local lgi = require "lgi"
local Gio = lgi.Gio
local GLib = lgi.GLib

local TEMPLATE = [[
%s
${font pftempestafivecondensed:size=6}${color1}
${voffset 155}${offset 42}%s
${voffset 6}${offset 50}${color2}%s]]

local bus = Gio.DBusProxy.new_for_bus_sync(
    Gio.BusType.SESSION,
    bin_or(Gio.DBusProxyFlags.NONE, Gio.DBusProxyFlags.DO_NOT_AUTO_START),
    nil,
    "org.mpris.MediaPlayer2.blaplay",
    "/org/mpris/MediaPlayer2",
    "org.freedesktop.DBus.Properties")

function conky_mpris()
    local function trim(s, limit)
        if #s > limit then
            return string.sub(s, 1, limit) .. "..."
        end
        return s
    end

    local metadata = bus:call_sync(
        "Get",
        GLib.Variant("(ss)", {"org.mpris.MediaPlayer2.Player", "Metadata"}),
        bin_or(Gio.DBusCallFlags.NONE, Gio.DBusCallFlags.NO_AUTO_START),
        -1)
    if metadata == nil then
        return ""
    end
    metadata = metadata.value[1].value

    local title = metadata["xesam:title"]
    local artist = metadata["xesam:artist"][1]
    if title == nil or artist == nil then
        return ""
    end
    title = trim(title, 33)
    artist = trim(artist, 48)
    local cover = metadata["mpris:artUrl"] or ""
    if cover ~= "" then
        cover = string.format("${image %s -p 50, 8 -s 200x200 -n}",
                              string.sub(cover, #"file://"+1))
    end

    return string.format(TEMPLATE, cover, artist, title)
end

