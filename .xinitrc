conky &
xset -b &
xset +fp /usr/share/fonts/local
xset fp rehash
xbindkeys &
sh /home/mates/home_repo/scripts/low_battery.sh &
sh /home/mates/home_repo/scripts/trackpad-toggle.sh &
exec ck-launch-session dbus-launch awesome
