# My personal ~/.emacs.d/

- For lower version, you need to manually put `package.el` into `~/.emacs.d`. 
One `package.el` compatible with emacs 23 is  <http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el>

### Usage

```
$ rm -rf .emacs.d
$ git clone https://github.com/ghxiao/.emacs.d.git
# on emacs < 24.2, you need package.el
$ wget \
http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el \
~/.emacs.d/
$ git submodule init
$ git submodule foreach git pull
```
