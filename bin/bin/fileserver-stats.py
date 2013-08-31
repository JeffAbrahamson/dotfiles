#!/usr/bin/python


import os
import subprocess
import sys


def refresh_data():
    """Refresh data from the raspberry pi fileserver."""
    os.system('rsync fileserver:data/d1 $HOME/data/raspberry/')
    os.system('rsync fileserver:data/d2 $HOME/data/raspberry/')
    os.system('rsync fileserver:bin/disk-usage.sh $HOME/data/raspberry/')
    

def read_series(filename):
    """Read a time series (seconds since the epoch in the first
    column, disk usage in MB in the second) and return a dictionary of
    {seconds -> MB}.
    """
    unsorted_points = dict()
    with open(filename, 'r') as series_fp:
        for line in series_fp:
            [ seconds_str, value_str ] = line.split()
            unsorted_points[int(seconds_str)] = float(value_str)
    return(unsorted_points)


def combine_series(filename, points1, points2):
    """Given two dicts from read_series, output a combined file.

    File format is seconds, point1, point2.
    Points may be empty if not present.
    """
    seconds = list(set(points1.keys()).union(set(points2.keys())))
    seconds.sort()
    p1 = 0
    p2 = 0
    with open(filename, 'w') as fp:
        for second in seconds:
            if second in points1:
                p1 = points1[second] / 1000
            if second in points2:
                p2 = points2[second] / 1000
            # Note that missing values simply use the last known value
            fp.write('{0}\t{1}\t{2}\n'.format(second, p1, p2))


def plot_it(filename):
    """Plot a raspbery pi fileserver disk usage table.

    File format is (column 1) seconds since the epoch, and (column
    two) MB used on disk.
    """
    plot_instructions = """
set xdata time
set timefmt "%s"
set format x "%Y-%m-%d"
set terminal png size 1360,717    # my laptop screen size (was x718)
set output '| display png:-'
"""
    plot_instructions += """
plot "%s" using 1:2 title "Disk usage (GB) SB" with lines lt 1 lw 2, \\
"%s" using 1:3 title "Disk usage (GB) JMA" with lines lt 4 lw 2
""" % (filename, filename)
    pipe_fd = subprocess.Popen(['gnuplot'], stdin=subprocess.PIPE)
    pipe_fd.communicate(plot_instructions)
    pipe_fd.wait()


def main():
    """Plot disk usage for the fileserver."""
    refresh_data()
    
    file_sb = os.getenv('HOME') + '/data/raspberry/d1'
    series_sb = read_series(file_sb)
    
    file_jma = os.getenv('HOME') + '/data/raspberry/d2'
    series_jma = read_series(file_jma)

    plot_file = "/tmp/fileserver.plot"
    combine_series(plot_file, series_sb, series_jma)
    plot_it(plot_file)
    #os.remove(plot_file)


if __name__ == "__main__":
    main()
