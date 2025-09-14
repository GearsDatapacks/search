# Search

A tool for searching the Gleam package index for JavaScript FFI code. Used in the
upgrade of code to Gleam's new JavaScript API.

## How to use

```sh
$ git clone https://github.com/GearsDatapacks/search
$ cd search
$ gleam run
```

Search uses [`pack`](https://github.com/GearsDatapacks/pack) to download Gleam
packages, so see documentation for that for information on how it downloads and
stored data.

After all packages have been downloaded, it will search each package for JavaScript
FFI files using Gleam custom types; these are the files which need to be updated.
After the search is complete, a report will be printed listing every file which
was found and needs to be updated.

For information on CLI flags which can be applied to change behaviour, run
`gleam run -- --help`.
