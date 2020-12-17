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
  'ecustools' which they were a part of.
