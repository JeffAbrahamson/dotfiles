#!/usr/bin/python3

"""Display a visualsiation of performance of a wifi SSID.
"""

import os
import pandas as pd
#import matplotlib
#matplotlib.use('TkAgg') 
import matplotlib.pyplot as plt
#import pyplot as plt
import pylab

def get_data():
    """Assemble wifi data into a pandas DataFrame.
    """
    data_dir_base = os.getenv('HOME') + '/data/hosts/'
    hosts = ['starshine', 'birdsong', 'nantes']
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
    df = df.loc[df.ssid == 'bguest']
    plt.scatter(df.upload, df.download, s=20 * df.ping)
    plt.xlabel("Upload speed")
    plt.ylabel("Download speed")
    plt.title("bguest wifi performance (point size is ping time, larger is worse)")
    #pylab.show()
    gc = plt.gcf()
    xdim = 16
    ydim = 12
    gc.set_size_inches(xdim, ydim)
    gc.savefig('/tmp/bguest.png', dpi=196)

def main():
    """Gather arguments and plot.
    """
    get_data()

if '__main__' == __name__:
    main()
