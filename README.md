# Takens Hotgym Demo

Demonstrates the plotting of lagged time-series data from Numenta's Hot Gym dataset.

## Usage

LightTable - open `core.clj` and press `Ctrl+Shift+Enter` to evaluate the file.

Emacs - run cider, open `core.clj` and press `C-c C-k` to evaluate the file.

REPL - run `(require 'takens.core)`.

## Keyboard Commands

- `0` - zero the time-series (start again)
- `m` - toggle smoothing
- `p` - pause/resume
- `g` - toggle display of coordinate graphs
- `c` - toggle display of cuboid showing current point
- `b` - toggle display of visit-density boxes
- `-` - decrease frame rate
- `=` - increase frame rate
- `i` - print camera state to console

## License

Copyright © 2015 Fergal Byrne

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
