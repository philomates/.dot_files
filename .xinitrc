xset -b &
# xset +fp /usr/share/fonts/local
xset fp rehash
xbindkeys &
xrandr --output DisplayPort-0 --auto --rotate left --output DVI-0 --auto --rotate left --left-of DisplayPort-0 &
conky &
nitrogen --restore &
exec dbus-launch awesome
