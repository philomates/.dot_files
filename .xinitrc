# conky &
xset -b &
xset fp rehash
xbindkeys &
nm-applet &

sh /home/mates/home_repo/scripts/random_wallpaper.sh &

setxkbmap -option ctrl:nocaps
setxkbmap -option compose:paus

exec dbus-launch awesome
