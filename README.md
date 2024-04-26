# ELAIZA
<img src="./image.webp" width="256px" align="right" alt="A cyberpunk-themed scene with a sleeker, less muscular hacker bunny wearing dark glasses, a stylish futuristic hoodie, and a visible black tank top beneath. The bunny, now with a slimmer, more toned build, exhibits a concentrated look as it types on an old-fashioned computer terminal. A half-eaten orange carrot is next to the keyboard. The scene is vibrantly lit with neon lights, enhancing the cyberpunk ambiance. A speech bubble above the bunny reads 'What's up Doc?' in a bold, cyber font, and the screen displays 'ELAIZA' in green monospaced font."/>

An updated Emacs Doctor.

## Installation Doom Emacs
Add the following to your `packages.el`
``` emacs-lisp
(package! elaiza :recipe (:host 'github :repo "SFTtech/emacs-elaiza" :branch "main")
```

For now, to simply use all integrated backends use the following configuration in your `config.el`

``` emacs-lisp
(use-package! elaiza
  :config (setq elaiza-available-backends
  elaiza-backends-integrations-alist))
```
