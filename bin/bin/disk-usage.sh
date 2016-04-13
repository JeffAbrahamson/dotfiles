#!/bin/bash

# Monitor disk usage on fileserver.
# This is a bit hard-coded.

# Return size in megabytes for partition $1.
get_size()
{
    df -BM "$1" | grep "$1" | awk '{print $3}' | tr -d M
}

# Write time and size in megabytes to partition $1 to datafile $2.
record_size()
{
    now=$(date +%s)
    size=$(get_size "$1")
    echo $now $size >> "$2"
}

record_size /d1 $HOME/data/d1
record_size /d2 $HOME/data/d2
record_size /d3 $HOME/data/d3
