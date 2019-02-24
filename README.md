# rigel-viz

[![Build Status](https://travis-ci.org/ocramz/rigel-viz.png)](https://travis-ci.org/ocramz/rigel-viz)

:star2: *Rigel (also called Beta Orionis) is the seventh-brightest star in the night sky, slightly less visible than Vega* :star2:

A (mid-level, simplified, opinionated) Haskell wrapper for [`vega-lite`](https://vega.github.io/vega-lite/), currently targeting version 3 of the `vega-lite` schema.

### Aims / definitions

* mid-level :
  * types which can take one of a few possible values are represented by sum types, not by strings.
  * glyph colours are encoded via the `colour` Haskell library

* simplified : the generated `vega-lite` JSON is not normalized, i.e. has some redundancies. This reflects the internal representation but also makes it easier to reason "locally" (i.e. code sections don't visibly exploit inheritance from higher layers).

* opinionated : part of the `vega-lite` API is not used at all. For example, there is no support for data preprocessing (e.g. summarization etc.). This forces the user to use the host language for preprocessing, which is bound to be more expressive and robust.

