# bc

A better basic calculator. Like the original `bc`, but with a better
prompt. Work in progress. Very incapable right now.

[![asciicast](https://asciinema.org/a/75ay3m4mx5i93tbfu7dahc55u.png)](https://asciinema.org/a/75ay3m4mx5i93tbfu7dahc55u)

## What?

The idea behind `bc` is to display partial results to the right of the
cursor in yellow while we type. A little example can be found in the video
above. Other than that, we want to be compatible with the original `bc`, though
somewhat faster. We have infinite precision integers and double precision floats.

## TODO

Basically everything except calculating stuff with numbers. This includes:

- A parser (foundations are laid)
- An evaluator (the current just does a tree rewrite on binary operations; this is
  cute, but not capable enough for the full `bc` language, of course)
- A good readline copy (haskeline is sadly out of the question for this project)
