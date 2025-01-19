#!/bin/bash
# Low battery notifier

# Kill already running processes
already_running="$(ps -fC 'grep' -N | grep '$0' | wc -l)"
if [[ $already_running -gt 1 ]]; then
    echo "Killing already running instance of '$'."
    pkill -f --older 1 "$0"
fi

# Get path
path="$( dirname "$(readlink -f "$0")" )"

while [[ 0 -eq 0 ]]; do
    battery_status="$(cat /sys/class/power_supply/BAT0/status)"
    if [ "$?" = 1 ] ; then
	echo "Exiting, not monitoring non-existant battery.";
	exit 0
    fi
    battery_charge="$(cat /sys/class/power_supply/BAT0/capacity)"
    if [ "$?" = 1 ] ; then
	echo "Exiting, not monitoring non-existant battery.";
	exit 0
    fi

    if [[ $battery_status == 'Discharging' && $battery_charge -le 85 ]]; then
	if   [[ $battery_charge -le 15 ]]; then
	    notify-send --icon="$path/battery_low.svg" --urgency=critical "Battery critical!" "${battery_charge}%"
	    sleep 180
	elif [[ $battery_charge -le 25 ]]; then
	    notify-send --icon="$path/battery_low.svg" --urgency=critical "Battery critical!" "${battery_charge}%"
	    sleep 240
	elif [[ $battery_charge -le 40 ]]; then
	    notify-send --icon="$path/battery_low.svg" "Battery low!" "${battery_charge}%"
	    sleep 360
	elif [[ $battery_charge -le 60 ]]; then
	    notify-send --icon="$path/battery_low.svg" "Battery low!" "${battery_charge}%"
	    sleep 480
	else
	    notify-send --icon="$path/battery_low.svg" "Battery low!" "${battery_charge}%"
	    sleep 600
	fi
    else
	sleep 600
    fi
done
