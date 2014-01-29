#!/bin/sh

function trim() {
    s=$1
    lim=$2
    if [[ ${#s} -gt $lim ]]; then
        echo ${s:0:$lim}"..."
    else
        echo $s
    fi
}

IFS=$'\n'
prev=''
prev2=''
cover='\n'
for l in $(dbus-send --print-reply --session \
           --dest=org.mpris.MediaPlayer2.blaplay \
           /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get \
           string:"org.mpris.MediaPlayer2.Player" string:"Metadata"); do
    if [[ "$prev" == *xesam:title* ]]; then
        title=`echo ${l##*string \"} | sed 's/\"$//'`
        title=$(trim $title 33)
    elif [[ "$prev2" == *xesam:artist* ]]; then
        artist=`echo ${l##*string \"} | sed 's/\"$//'`
        artist=$(trim $artist 48)
    elif [[ "$prev" == *mpris:artUrl* ]]; then
        cover=`echo ${l##*string \"file://} | sed 's/\"$//'`
        cover="\${image "$cover" -p 50, 8 -s 200x200 -n}\n"
    fi
    prev2=$prev
    prev=$l
done

echo -e "$cover\${font pftempestafivecondensed:size=6}\${color1}
\${voffset 170}\${offset 42}$artist
\${voffset 5}\${offset 50}\${color2}$title"

