% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{set_theme}
\alias{set_theme}
\alias{add_theme}
\title{Add a theme of to an scplot}
\usage{
set_theme(object, theme, ...)

add_theme(...)
}
\arguments{
\item{object}{An scplot object (class \code{scplot}) returned from the \code{scplot()}
function.}

\item{theme}{A character string with a predefined graphical theme or a theme
object created with \code{\link[=new_theme]{new_theme()}}.}

\item{...}{Further character strings or \code{scplot-theme} objects that are
"added" on top.}
}
\value{
An object of class \code{scplot} (see\code{\link[=scplot]{scplot()}}) with a changed \code{theme}
element.
}
\description{
Possible themes are: \verb{'basic', 'grid', 'default', 'small', 'tiny', 'big', 'minimal', 'dark', 'sienna', 'phase_color', 'phase_shade', 'grid2'}.
}
