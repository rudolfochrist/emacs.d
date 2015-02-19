# emacs.d

My Emacs configuration files

### Prerequisites

1. Install [SBCL](http://www.sbcl.org/) (or any other CL implementation)
2. Install [Quicklisp](http://www.quicklisp.org/)
3. Install Quicklisp Slime Helper: `(ql:quickload "quicklisp-slime-helper")`
4. Install CLHS: `(ql:quickload "clhs")`
   1. Configure CLHS: `(clhs:install-clhs-use-local)`

### Installation

    $ cd $HOME
    $ git clone https://github.com/rudolfochrist/emacs.d.git .emacs.d
