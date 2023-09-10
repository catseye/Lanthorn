#!/bin/sh

APPLIANCES="tests/appliances/lanthorn.md"

falderal $APPLIANCES README.md || exit 1
