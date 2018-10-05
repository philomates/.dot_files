
MOUSE_ID=`xinput list | grep "slave  pointer" | grep -i Logitech | awk -F= '{ print $2}' | awk '{print $1}'| tail -n1`
xinput set-button-map $MOUSE_ID 1 0 3 4 5 6 7 8 9 2 11 12 13 14 15 16 17 18 19 20
MOUSE_ID=`xinput list | grep "slave  pointer" | grep -i Logitech | awk -F= '{ print $2}' | awk '{print $1}'| head -n1`
xinput set-button-map $MOUSE_ID 1 0 3 4 5 6 7 8 9 2 11 12 13 14 15 16 17 18 19 20
