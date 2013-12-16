#!/bin/bash

cd $(pwd)/$(dirname $0)
(echo bash; cd bash  && ./install.sh)
(echo bin; cd bin   && ./install.sh)
(echo emacs; cd emacs && ./install.sh)
(echo templates; cd templates && ./install.sh)
(echo readline; cd readline && ./install.sh)
(echo ratpoison; cd ratpoison && ./install.sh)
