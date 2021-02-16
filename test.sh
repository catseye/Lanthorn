#!/bin/sh

cat >/tmp/fixtures.md <<EOF
    -> Functionality "Evaluate Lanthorn Program" is implemented by
    -> shell command "bin/lanthorn eval %(test-body-file)"

    -> Functionality "Pretty-print Lanthorn Program" is implemented by
    -> shell command "bin/lanthorn pretty %(test-body-file)"

    -> Functionality "Desugar Lanthorn Program" is implemented by
    -> shell command "bin/lanthorn desugar %(test-body-file)"
EOF

falderal /tmp/fixtures.md README.md || exit 1
