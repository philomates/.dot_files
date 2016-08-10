conky &
xset -b &
xset fp rehash
xbindkeys &
#nitrogen --restore &
nm-applet &
sh /home/mates/home_repo/scripts/trackpad-toggle.sh &
sh /home/mates/home_repo/scripts/random_wallpaper.sh &
xinput set-prop "TPPS/2 IBM TrackPoint" "Device Accel Constant Deceleration" 0.45

TOUCHSCREEN_ID=`xinput list | grep -a Touchscreen | awk '/id=/{ print $5 }' | awk -F= '{ print $2 }'`
xinput disable $TOUCHSCREEN_ID

setxkbmap -option ctrl:nocaps

exec dbus-launch awesome
