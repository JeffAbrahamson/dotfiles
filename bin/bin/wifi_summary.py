#!/usr/bin/python3

"""Display a visualsiation of performance of a wifi SSID.
"""

import argparse
import os
import pandas as pd
#import matplotlib
#matplotlib.use('TkAgg') 
import matplotlib.pyplot as plt
import matplotlib.cm as cm
#import pyplot as plt
import pylab

def get_data(ssid):
    """Assemble wifi data into a pandas DataFrame.
    """
    data_dir_base = os.getenv('HOME') + '/data/hosts/'
    hosts = ['morning', 'starshine', 'birdsong', 'nantes']
    # Eventually use all the hosts.  For now, just use starshine.
    # It's probably the only one that matters.
    data_dir = data_dir_base + hosts[0] + '/speedtest-'
    df_ping = pd.read_table(data_dir + 'ping', sep=' ', names=['timestamp', 'ping'])
    df_download = pd.read_table(data_dir + 'download', sep=' ', names=['timestamp', 'download'])
    df_upload = pd.read_table(data_dir + 'upload', sep=' ', names=['timestamp', 'upload'])
    df_ssid = pd.read_table(data_dir + 'ssid', sep=' ', names=['timestamp', 'ssid'])

    df = df_ping.set_index('timestamp').join(df_download.set_index('timestamp'), how='inner')
    df = df.join(df_upload.set_index('timestamp'), how='inner')
    df = df.join(df_ssid.set_index('timestamp'), how='inner')
    if ssid != '':
        df = df.loc[df.ssid == ssid]
    # The timestamp is now the index and not a column.  Oops.
    time_min = df.index.min()
    timespan = df.index.max() - time_min
    color_min = 0
    color_max = 256 - 64
    df['color'] = (df.index - df.index.min()) / float(timespan) * color_max
    plt.scatter(df.upload, df.download, s=20 * df.ping, c=df.color, cmap=cm.gray, alpha=.2)
    plt.xlabel("Upload speed")
    plt.ylabel("Download speed")
    plt.title("{ssid} wifi performance (point size is ping time, larger is worse)".format(
        ssid=ssid))
    plt.xlim(0, df.upload.max() * 1.1)
    plt.ylim(0, df.download.max() * 1.1)
    #pylab.show()
    gc = plt.gcf()
    xdim = 16
    ydim = 12
    gc.set_size_inches(xdim, ydim)
    gc.savefig('/tmp/wifi-{ssid}.png'.format(ssid=ssid), dpi=196)

def main():
    """Gather arguments and plot.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument('--ssid', type=str, required=False,
                        default='',
                        help='if present, filter to this ssid')
    args = parser.parse_args()
    get_data(args.ssid)

if '__main__' == __name__:
    main()
