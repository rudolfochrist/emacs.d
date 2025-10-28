#!/bin/sh

app_env="${APP_ENV:-production}"
APP_ENV="$app_env" screen -UdmS __PROJECT-NAME__ /bin/sh -c 'sbcl --core __PROJECT-NAME__.image --no-userinit; exec sh'
