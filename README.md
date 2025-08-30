# slime-sysdef
## Project
The slime-sysdef module provides system utilities for SLIME using the
[SYSDEF](https://github.com/galdor/sysdef) Common Lisp system management
facility.

## Usage
You can install slime-sysdef as any other Emacs package. If you are using
[Straight](https://github.com/radian-software/straight.el) (and you should), it is as simple as:
```lisp
(use-package slime-sysdef
  :straight (:type git :host github :repo "galdor/slime-sysdef"))
```

Or with Emacs >=30:
```lisp
(use-package slime-sysdef
  :vc (:url "https://github.com/galdor/slime-sysdef"))
```

You must then add the `slime-sysdef-init` to the `slime-connected-hook`, e.g.
in the `use-package` form of Slime:
```lisp
(use-package slime
  :hook
  ((slime-connected-hook . slime-sysdef-init)))
```

Or manually:
```lisp
(add-hook 'slime-connected-hook 'slime-sysdef-init)
```

## Licensing
The slime-sysdef package is open source software distributed under the
[ISC](https://opensource.org/licenses/ISC) license.

## Contact
If you have an idea or a question, feel free to email me at
<nicolas@n16f.net>.
