#!/bin/bash

fswatch -o $1 | xargs -n1 -I{} stack exec -- stack exec -- liquid -i api $1
