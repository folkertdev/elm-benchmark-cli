#!/bin/bash

elm make Main.elm --optimize --output elm.js >/dev/null && node run.js
