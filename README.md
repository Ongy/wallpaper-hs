# Wallpaper-hs

Simple wrapper for `feh` on heterogeneous monitor setup.

If you are like me and like to have a wallpaper on your screen, you may have found `feh` to be a good tool to set such wallpapers.

Sadly feh doesn't do the best job at picking wallpapers at random.

## What it does

`wallpaper-hs` makes this slightly nicer:

 * first it recursivly indexes a wallpaper directory
 * it creates a list of suitable wallpapers for each monitor
   > suitable here means that it will have at most 10% of black bars
 * It then picks a random picture for each monitor independantlly

## Config

The config file should be written to `$(XDG_CONFIG_HOME)/wallpaper-hs/config`

Currently the only option to set is (here with an example):
 > wallpaper-dir: "/usr/local/share/wallpapers"'