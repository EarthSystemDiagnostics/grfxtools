#' Axis label expression
#'
#' Obtain an axis label involving common math expressions for label and units.
#'
#' This function provides some keywords to automatically format common math
#' expressions for axis labels and units.
#'
#' For \code{label}:
#' \describe{
#'   \item{"oxy":}{notation for oxygen isotopic composition; i.e., a greek delta
#'     followed by a roman "O" prefixed by superscript "18".}
#'   \item{"dtr":}{notation for hydrogen isotopic composition; i.e., a greek delta
#'     followed by a roman "H" prefixed by superscript "2".}
#'   \item{"dtr.var":}{alternative notation for hydrogen isotopic composition:
#'     greek delta followed by a roman "D".}
#' }
#' For \code{unit}:
#' \describe{
#'   \item{"percent":}{the percent sign.}
#'   \item{"permil":}{the per mille sign.}
#'   \item{"celsius":}{degree Celsius.}
#' }
#'
#' @param label character string with the name of the quantity referred to on
#'   the axis; see details for the available keywords to automatically produce
#'   common math expressions of axis names.
#' @param prefix optional character string preceding the axis \code{label} with
#'   an empty space automatically being inserted between them.
#' @param suffix optional character string following the axis \code{label} with
#'   an empty space automatically being inserted between them.
#' @param unit character string with an abbreviation of the physical unit of
#'   the quantity displayed on the axis; see details for the available keywords
#'   to automatically produce common math expressions of units. The unit is
#'   placed after the \code{label}, with an empty space inserted between them
#'   and surrounded by brackets as specified in \code{bracket.type}.
#' @param time.unit character string with an abbreviation of the physical unit
#'   of time needed to determine the unit of power spectral density when
#'   \code{axis.type == "psd"}; see the examples.
#' @param unit.type character; the type of axis for determining the format
#'   of the axis unit. Default \code{"standard"} means to use the unit as is;
#'   other possible options are frequency axis, \code{"freq"}, trend axis,
#'   \code{"trend"}, and power spectral density axis, \code{"psd"}. For a
#'   frequency axis, the passed \code{unit} is displayed to the power of -1; for
#'   a trend axis, the passed \code{unit} is multiplied by the unit of time
#'   given in \code{time.unit} to the power of -1; and for a power spectral
#'   density axis, the \code{unit} is displayed to the power of 2 multiplied by
#'   the unit of time given in \code{time.unit}; see the examples.
#' @param bracket.type the type of bracket to use for surrounding the
#'   \code{unit}; possible options are "round" brackets (default), "square"
#'   brackets, or "curly" \code{\{.\}} brackets.
#' @param dot logical; shall a multiplication sign (as a dot) be placed between
#'   the units for axis types \code{"trend"} and \code{"psd"}? Defaults to
#'   \code{FALSE}.
#' @param font an integer which specifies which font to use for the label
#'   text. Defaults to the current axis label font setting of \code{par()}; see
#'   \code{?par} for the available options. Note that the font setting has no
#'   effect on greek and other symbols.
#'
#' @return a language object to be passed on to \code{xlab}, \code{ylab}
#'   etc. parameter settings in plot calls, or directly to \code{text} or
#'   \code{mtext} calls.
#' @author Thomas Münch
#' @examples
#'
#' plot(1 : 15, type = "n", xlab = "", ylab = "")
#'
#' # preformatted isotope labels and units using the keywords
#' text(1, 15, adj = 0, labels = LabelAxis(label = "oxy"))
#' text(1, 14, adj = 0, labels = LabelAxis(label = "dtr"))
#' # change font setting
#' # (but note that greek letters and other symbols remain unchanged)
#' text(1, 13, adj = 0, labels = LabelAxis(label = "dtr.var", font = 2))
#'
#' # use normal text as label
#' text(1, 12, adj = 0, labels = LabelAxis(label = "Some other isotope species"))
#'
#' # use a prefix together with a preformatted label
#' text(1, 11, adj = 0, labels = LabelAxis(label = "oxy", prefix = "Ice core"))
#'
#' # use a suffix together with a preformatted label
#' text(1, 10, adj = 0, labels = LabelAxis(label = "oxy", suffix = "anomaly"))
#'
#' # use both
#' text(1, 9, adj = 0, labels = LabelAxis(label = "oxy",
#'      prefix = "Ice core", suffix = "anomaly"))
#'
#' # temperature unit in degree Celsius
#' text(1, 8, adj = 0,
#'      labels =  LabelAxis(label = "Temperature", unit = "celsius"))
#'
#' # automatic formatting of unit for a frequency axis
#' text(1, 7, adj = 0,
#'      labels = LabelAxis(label = "Frequency", unit = "yr",
#'                         unit.type = "freq"))
#'
#' # automatic formatting of unit for a trend axis
#' text(1, 6, adj = 0,
#'      labels = LabelAxis(label = "Temperature trend", unit = "celsius",
#'                         time.unit = "yr", unit.type = "trend"))
#'
#' # automatic formatting of unit for a power spectral density axis
#' text(1, 5, adj = 0,
#'      labels = LabelAxis(label = "PSD", unit = "permil",
#'                         time.unit = "yr", unit.type = "psd"))
#' # use a multiplication dot
#' text(1, 4, adj = 0,
#'      labels = LabelAxis(label = "PSD", unit = "K",
#'                         time.unit = "s", unit.type = "psd", dot = TRUE))
#'
#' # note that for the previous examples two units are mandatory:
#' \dontrun{
#'   LabelAxis(label = "trend", unit = "permil", unit.type = "trend")
#'   LabelAxis(label = "PSD", unit = "permil", unit.type = "psd")
#' }
#' 
#' # pass preformatted math expression as label (needs to use 'bquote'!)
#' text(1, 3, adj = 0,
#'      labels = LabelAxis(label = bquote("T"["2m"]), unit = "celsius"))
#'
#' # use other brackets around units
#' text(1, 2, adj = 0,
#'      labels = LabelAxis(label = "I like squares", unit = "unit",
#'                         bracket.type = "square"))
#' text(1, 1, adj = 0,
#'      labels = LabelAxis(label = "I like curly hair", unit = "unit",
#'                         bracket.type = "curly"))
#'
#' @export
LabelAxis <- function(label = "oxy", prefix = "", suffix = "",
                      unit = "permil", time.unit = NULL, unit.type = "standard",
                      bracket.type = "round", dot = FALSE,
                      font = graphics::par()$font.lab) {

  if (!unit.type %in% c("standard", "freq", "trend", "psd")) {
    stop("'unit.type' must be one of 'standard', 'freq', 'trend' or 'psd'.",
         call. = FALSE)
  }
  
  if (bracket.type == "round") {
    ob <- "("; cb <- ")"
  } else if (bracket.type == "square") {
    ob <- "["; cb <- "]"
  } else if (bracket.type == "curly") {
    ob <- "{"; cb <- "}"
  } else {
    stop("'bracket.type' must be one of 'round', 'square' or 'curly'.",
         call. = FALSE)
  }

  if (unit.type == "trend" & !length(time.unit)) {
    stop("Need a unit of time for the trend axis.", call. = FALSE)
  }
  if (unit.type == "psd" & !length(time.unit)) {
    stop("Need a unit of time for the PSD axis.", call. = FALSE)
  }

  if (label == "oxy")    label <- bquote(delta^{"18"} * "O")
  if (label == "dtr")    label <- bquote(delta^{"2"} * "H")
  if (label == "dtr.var") label <- bquote(delta * "D")

  if (unit == "permil")  unit <- "\u2030"
  if (unit == "percent") unit <- "\u0025"
  if (unit == "celsius") unit <- bquote(degree * "C")

  if (unit.type == "freq")  unit <- bquote(.(unit)^{"-1"})

  if (dot) {
    if (unit.type == "trend") unit <- bquote(.(unit) %.% .(time.unit)^{"-1"})
    if (unit.type == "psd")   unit <- bquote(.(unit)^{"2"} %.% .(time.unit))
  } else {
    if (unit.type == "trend") unit <- bquote(.(unit) ~ .(time.unit)^{"-1"})
    if (unit.type == "psd")   unit <- bquote(.(unit)^{"2"} ~ .(time.unit))
  }

  if (nchar(prefix)) prefix <- paste(prefix, "")
  if (nchar(suffix)) suffix <- paste("", suffix)

  bquote.font(
    bquote(.(prefix) * .(label) * .(suffix) * " " * .(ob) * .(unit) * .(cb)),
    font = font)
}

#' Font selection for bquote
#'
#' A wrapper function for \code{\link[base]{bquote}} to select a text font.
#'
#' @param obj a language object.
#' @param font an integer which specifies which font to use for text; see
#'   \code{?par} for the available options.
#' @return a language object.
#' @author Thomas Münch
bquote.font <- function(obj, font = 1) {

    if (!is.numeric(font) | is.na(match(font, 1 : 5))) {
        stop("Unknown font setting.")
    }

    if (font == 1) {
        bquote(plain(.(obj)))
    } else if (font == 2) {
        bquote(bold(.(obj)))
    } else if (font == 3) {
        bquote(italic(.(obj)))
    } else if (font == 4) {
        bquote(bolditalic(.(obj)))
    } else if (font == 5) {
        bquote(symbol(.(obj)))
    }
}
