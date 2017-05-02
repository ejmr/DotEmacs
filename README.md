GNU Emacs Configuration
=======================

This is my personal configuration for [GNU Emacs][Emacs].  I load
almost all packages via `use-package` but without any `:ensure t`
settings, meaning the configuration will not work out-of-the-box for
most users, unless they install *all* of those packages.

Aspects and tidbits that may be useful for other Emacs users:

* I have a lot of [Hydras](https://github.com/abo-abo/hydra).
* Very quick movement around buffers and windows using combinations of
  packages like [Avy](https://github.com/abo-abo/avy)
  and [`key-seq`](https://github.com/vlevit/key-seq.el).
* `s-1` and `s-w` are prefix-keys for useful help and window commands,
  respectively.
* Heavy use of [Ivy](http://oremacs.com/swiper/) and Counsel with
  various packages, including some custom commands like
  `ejmr-counsel-file-register`.
* Configuration for many programming languages and tools such
  as [Quickrun](https://github.com/syohex/emacs-quickrun),
  [Dumb Jump](https://github.com/jacktasia/dumb-jump), et al.
* Add-ons and a few home-grown tools for Dired.
* A setup to easily write content in Markdown, AsciiDoc, Fountain, and
  so on, along with an interface to [Pandoc](http://pandoc.org/).
  
If you find anything useful within my Emacs configuration, feel free
to copy-paste it, modify it, do whatever you like.



[Emacs]: https://www.gnu.org/software/emacs/ "Official GNU Emacs Homepage"
