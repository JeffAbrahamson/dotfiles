#!/usr/bin/python3

"""Plot network reliability.

I have files of sampled connection tests: ping success/failure and
speedtest results since 2017 or so.  This program joins them together
and creates a plot that shows daily stats over that time period.  The
goal isn't to understand any particular failure but to identify broad
trends and properties of the connection provided by our ISP.

Running this on a laptop presents a very different story (different
ISPs at different times) than on an ethernet-connected fixed host
(measures a single ISP).

"""

import argparse
import datetime
import glob
import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pandas.plotting import register_matplotlib_converters

def get_filenames(dirname):
    """Discover the data files we care about.
    """
    return set(glob.glob(dirname + '/speedtest-*') + \
               glob.glob(dirname + '/*ping') + \
               glob.glob(dirname + '/*up'))

def join_on_time(filenames):
    """Read the files whose names we are passed.

    Expect them all to have a first column that is seconds since the
    epoch and a second that is the data payload.

    Join them based on timestamp.  Ideally, this should permit a
    second or so of slush, because there was a time before my
    collecting program fixed a timestamp before gathering all its
    data.

    Return as a pandas dataframe.

    """
    dframe = None
    for filename in filenames:
        new_dframe = pd.read_csv(filename, sep=r'\s+',
                                 names=['timestamp', filename],
                                 index_col='timestamp')
        if dframe is None:
            dframe = new_dframe
        else:
            dframe = dframe.join(new_dframe, how='outer')
    return dframe

def plot_ping_latency_by_day(dframe):
    """Plot.
    """
    max_latency = 400           # Clip below this value.
    ping_latency_df = dframe.loc[:, ['-ping' in x and 'speedtest' not in x
                                     for x in dframe.columns]].copy()
    # Compute mean ping latency at each time point.  Note that mean
    # will ignore NaN columns.  Then we drop NaN, since we want each
    # row that has at least one valid entry (double).
    ping_latency_series = ping_latency_df.mean(axis=1).dropna()
    mean_latency_df = pd.DataFrame(ping_latency_series, columns=['latency'])
    mean_latency_df['clipped_latency'] = mean_latency_df.latency.apply(
        lambda val : min(max_latency, val))
    mean_latency_df['index'] = mean_latency_df.index
    mean_latency_df['date'] = mean_latency_df['index'].apply(
        datetime.date.fromtimestamp)
    plt.scatter(mean_latency_df.date, mean_latency_df.clipped_latency, s=1, c='orange')

    date_group = mean_latency_df.loc[:, ['latency', 'date']].groupby('date')
    lower_percentile = 5
    upper_percentile = 95
    alpha = .1
    dframe_p_lower = date_group.agg(
        lambda val: min(max_latency, np.percentile(val, lower_percentile)))
    dframe_p_upper = date_group.agg(
        lambda val: min(max_latency, np.percentile(val, upper_percentile)))
    plt.scatter(dframe_p_lower.index, dframe_p_lower.ewm(alpha=alpha).mean(), s=1, c='darkblue')
    plt.scatter(dframe_p_upper.index, dframe_p_upper.ewm(alpha=alpha).mean(), s=2, c='slateblue')

    plt.title('Mean ping latency by day, clipped to ' + str(max_latency))
    plt.ylabel('Ping latency, seconds')
    plt.xlabel('EWMA of ' + str(lower_percentile) + ' and ' + \
               str(upper_percentile) + \
               ' percentiles with alpha = ' + str(alpha))
    plt.ylim((0, None))
    plt.show()

def plot_ping_uptime_by_day(dframe):
    """Plot.
    """
    ping_uptime_df = dframe.loc[:, ['-up' in x and 'speedtest' not in x
                                    for x in dframe.columns]].copy()
    ping_uptime_series = ping_uptime_df.mean(axis=1).dropna()
    mean_update_df = pd.DataFrame(ping_uptime_series, columns=['uptime'])
    mean_update_df['index'] = mean_update_df.index
    mean_update_df['date'] = mean_update_df['index'].apply(
        datetime.date.fromtimestamp)
    plt.scatter(mean_update_df.date, mean_update_df.uptime, s=10, c='orange')

    date_group = mean_update_df.loc[:, ['uptime', 'date']].groupby('date')
    lower_percentile = 5
    upper_percentile = 95
    alpha = .05
    dframe_p_lower = date_group.agg(
        lambda val: np.percentile(val, lower_percentile))
    dframe_p_upper = date_group.agg(
        lambda val: np.percentile(val, upper_percentile))
    plt.scatter(dframe_p_lower.index, dframe_p_lower.ewm(alpha=alpha).mean(), s=1, c='darkblue')
    plt.scatter(dframe_p_upper.index, dframe_p_upper.ewm(alpha=alpha).mean(), s=1, c='slateblue')

    plt.title('Mean uptime via multi-host ping by day')
    plt.ylabel('Uptime fraction')
    plt.xlabel('EWMA of ' + str(lower_percentile) + ' and ' + \
               str(upper_percentile) + \
               ' percentiles with alpha = ' + str(alpha))
    plt.ylim((-0.1, 1.1))
    plt.show()

    image_name = None

def plot_speedtest_download_by_day(dframe):
    """Plot.
    """
    dframe = None

def plot_speedtest_upload_by_day(dframe):
    """Plot.
    """
    dframe = None

def plot_speedtest_latency_by_day(dframe):
    """Plot.
    """
    dframe = None

def plot_speedtest_uptime_by_day(dframe):
    """Plot.
    """
    dframe = None

def main():
    """Do what we do.
    """
    # Set up matplotlib converters.
    register_matplotlib_converters()

    parser = argparse.ArgumentParser()
    parser.add_argument('--dir', type=str, required=True,
                        help='Directory in which to find data files')
    args = parser.parse_args()

    filenames = get_filenames(args.dir)
    joined_data = join_on_time(filenames)
    #plot_ping_latency_by_day(joined_data)
    plot_ping_uptime_by_day(joined_data)

    plot_speedtest_download_by_day(joined_data)
    plot_speedtest_upload_by_day(joined_data)
    plot_speedtest_latency_by_day(joined_data)
    plot_speedtest_uptime_by_day(joined_data)

if '__main__' == __name__:
    main()
