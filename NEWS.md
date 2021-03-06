# grfxtools 0.3.1

Update of `ColorPal()` function:

The function parameter options now clearly distinguish between setting the
number of input colours for the palette and setting the number of (interpolated)
return colours.

# grfxtools 0.3.0

Several new functions added:

* `Quartz()` [for Mac OS users only]: wrapper for the `quartz()` device using
  per default a 4:3 aspect ratio for on-screen plotting or saving to a file and
  including the setting of default graphical parameters which are deliberately
  different from base R's settings to produce visually more appealing plots.
* `Par()`: function to set graphical parameters specified as its arguments via
  passing them on to a call to `par()`. The function's default settings are set
  to produce visually more appealing plots compared to base R's default
  settings.
* `ErrorBars()`: function to add error bars to an existing point plot.
* `PlotStamp()`: function to add session information to an existing base plot:
  script name, date of plot creation, etc., placed in the upper right corner of
  the plot, so that for later reference you know when and how you created the
  plot.
* `LabelAxis()`: function to format nice axis labels involving common math
  expressions for label and units.
* `ColorPal()`: wrapper function to quickly obtain a colour palette from the
  ColorBrewer 2.0 collection for use in standard plots, image plots, or filled
  contour plots.

# grfxtools 0.2.0

Changes for a proper and more self-contained package:

* Consistent use of `::` syntax to call functions from base and other packages
  to remove unnecessary function or package imports into the NAMESPACE.
* `rgeos` is now only a suggested package to avoid problems with external
  libraries needed on Linux systems.
* The `wrld_simpl` dataset is now an internal dataset in the package to render
  unnecessary the full import of the `maptools` package into the `grxftools`
  NAMESPACE.
* Use of `rlang` `.data` pronoun to avoid R CMD check notes.
* Use of ascii escape character in `ggpolar()` to avoid R CMD check warning.
* Update of function documentations and changes to adhere to style guide.

# grfxtools 0.1.0

* Initial package version with number and scope of the functions incl. their
  documentation identical to their original versions in deprecated package
  'ecustools' which they were a part of ('ecustools'
  [`main`](https://github.com/EarthSystemDiagnostics/ecustools/tree/master)
  branch as of 2020-11-20).
