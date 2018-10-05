#!/bin/bash

IN="eDP-1"
EXT1="DP-2"
EXT2="HDMI-1"

if (xrandr | grep "$EXT1" | grep " connected 1440")
    then
      xrandr --output $EXT2 --off
      xrandr --output $IN --auto --output $EXT1 --off
      ps aux | grep "[0-9]\ conky" | awk '{print $2}' | xargs kill -9
      conky&
    else
        if (xrandr | grep "$EXT1" | grep " connected")
            then
            xrandr --output $IN --off --output $EXT1 --auto --rotate left --right-of $EXT2 --output $EXT2 --auto --rotate right
            ps aux | grep "[0-9]\ conky" | awk '{print $2}' | xargs kill -9
            conky&
        fi
fi

