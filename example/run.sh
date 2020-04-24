#!/bin/bash

elm make Main.elm --output elm.js >/dev/null && node run.js
