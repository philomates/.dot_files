# conky &
xset -b &
xset fp rehash
xbindkeys &
#nitrogen --restore &
nm-applet &
#sh /home/mates/scripts/setup_mouse.sh
sh /home/mates/scripts/toggleDual.sh
#sh /home/mates/home_repo/scripts/random_wallpaper.sh &

setxkbmap -option ctrl:nocaps
setxkbmap -option compose:paus
xmodmap ~/.Xmodmap

exec dbus-launch awesome
