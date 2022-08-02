howto use fixRefs.js

step 1: get ooapi bundle from specification/v5-rc
file 1: ooapiv5-refs.json
> redocly bundle spec.yaml -o ooapiv5-refs --ext json

step 2: fix references
file 2: ooapiv5-norefs.json
> nodejs fixRefs.js

step 3: copy to destination
file 3: ooapiv5.json
> cp ooapiv5-norefs.json ../resources/ooapiv5.json
