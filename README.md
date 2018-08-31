# `dhall-json 1.2.3`

[![Build Status](https://travis-ci.org/dhall-lang/dhall-json.png)](https://travis-ci.org/dhall-lang/dhall-json.png)
[![Hackage](https://img.shields.io/hackage/v/dhall-json.svg)](https://hackage.haskell.org/package/dhall-json)

This `dhall-json` package provides a Dhall to JSON compiler and a Dhall to YAML
compiler.  The reason this package is called `dhall-json` is that the Haskell
`yaml` library uses the same data structure as Haskell's `aeson` library for
JSON

## Quick start

If you have Nix installed then you can build and run this package using:

```bash
$ nix-build -A dhall-json release.nix
$ result/bin/dhall-to-json <<< "{ foo = 1, bar = True }"
{"foo":1,"bar":true}
$ result/bin/dhall-to-json <<< "List/head Integer ([] : List Integer)"
null
$ result/bin/dhall-to-yaml <<< "{ foo = [1, 2, 3] : List Integer, bar = { baz = True } }"
foo:
- 1
- 2
- 3
bar:
  baz: true
```

## Development status


I don't expect this library to change unless:

* ... the Dhall language changes, which is possible but not very likely
* ... there are bugs, which is unlikely given how simple the implementation is

## License (BSD 3-clause)

    Copyright (c) 2017 Gabriel Gonzalez
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright notice,
          this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright notice,
          this list of conditions and the following disclaimer in the documentation
          and/or other materials provided with the distribution.
        * Neither the name of Gabriel Gonzalez nor the names of other contributors
          may be used to endorse or promote products derived from this software
          without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
