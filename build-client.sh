#!/bin/bash

stack build --stack-yaml=client/stack.yaml
rm -f api/static/all.js
cp $(stack path --stack-yaml=client/stack.yaml --local-install-root)/bin/app.jsexe/all.js api/static/app.js
#fswatch -o client | xargs -n1 -I{} ./build-client.sh &
