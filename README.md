# Search

A tool for searching the Gleam package index for JavaScript FFI code. Used in the
upgrade of code to Gleam's new JavaScript API.

## How to use

```sh
$ git clone https://github.com/GearsDatapacks/search
$ cd search
$ gleam run
```

Simply running `gleam run` will perform the search. It will create a file called
`packages.txt` which stores a list of Gleam packages along with the number of
downloads each has. The download count is used to prioritise updating more commonly
used packages. It will also create a `packages` directory, which contains the
source code of all Gleam packages index by the Gleam package index.

The `packages.txt` file is stored to save querying the entire index again if for
some reason you need to redownload Hex packages.

After all packages have been downloaded, it will search each package for JavaScript
FFI files using Gleam custom types; these are the files which need to be updated.
After the search is complete, a report will be printed listing every file which
was found and needs to be updated.

For information on CLI flags which can be applied to change behaviour, run
`gleam run -- --help`.
