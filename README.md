# bc

A better basic calculator. Like the original `bc`, but with a better
prompt. Work in progress. Very incapable right now.

[![asciicast](https://asciinema.org/a/75ay3m4mx5i93tbfu7dahc55u.png)](https://asciinema.org/a/75ay3m4mx5i93tbfu7dahc55u)

## What?

The idea behind `bc` is to display partial results to the right of the
cursor in yellow while we type. A little example can be found in the video
above. Minor syntactic cleanups have been developed as well (e.g. no `local`
keyword). We have infinite precision integers and 90 decimal mantissa precision
floats.

## TODO

- A good readline copy (haskeline is sadly out of the question for this project)
- Comment parsing
- Closures that can modify the parent environment (but do away with the `local` keyword).
- Better arbitrary precision floats
