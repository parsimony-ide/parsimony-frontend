[Parsimony][parsimony] is a web-based development environment for constructing
parsers by example -- provide examples of correct parses, and Parsimony will
attempt to guess syntax rules based on those examples.

This repository contains the frontend code that runs on the browser-side.

## External Build Dependencies

Most dependencies will download automatically as part of the build process.
There are, however, two dependencies that you must install yourself:

1. Download the [Boost Library 1.62.0 headers][boost] and place them under
   `include/boost` in the project directory.
1. Install [Emscripten 1.35.0][emscripten]. Ensure `emcc` is on your PATH.
1. Install [Leiningen][leiningen]. Ensure `lein` is on your PATH.

## Building

Run this incantation from the shell.

``` 
$ make; lein garden once; lein cljsbuild once production
```

The `resources/public` directory now contains all compiled HTML, JS, and CSS
assets that comprise the frontend.

[parsimony]: https://parsimony-ide.github.io/ 
[boost]: http://www.boost.org/users/history/version_1_62_0.html
[emscripten]: https://kripken.github.io/emscripten-site/docs/getting_started/downloads.html
[leiningen]: https://leiningen.org/
