import argv
import filepath
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import just
import pack
import simplifile as file

const out_file = "results.txt"

pub type Package {
  Package(name: String, latest_version: String, downloads: Int)
}

pub fn main() -> Nil {
  let args = argv.load().arguments

  use <- bool.lazy_guard(list.contains(args, "--help"), print_help_text)

  let refresh_packages = list.contains(args, "--refresh-packages")
  let write_to_file = list.contains(args, "--write-to-file")

  let assert Ok(pack) =
    pack.load(pack.Options(
      write_to_file: True,
      refresh_package_list: refresh_packages,
      write_packages_to_disc: True,
      read_packages_from_disc: True,
    ))

  let packages =
    pack
    |> pack.packages
    |> list.map(fn(package) {
      Package(
        name: package.name,
        latest_version: package.latest_version,
        downloads: list.fold(package.releases, 0, fn(downloads, release) {
          downloads + release.downloads
        }),
      )
    })
    |> list.sort(fn(a, b) { int.compare(b.downloads, a.downloads) })

  let assert Ok(package_files) = pack.download_packages(pack)

  let reports = list.reverse(scan_packages(packages, package_files))
  print_reports(reports, write_to_file)

  Nil
}

fn print_help_text() -> Nil {
  io.println(
    "
USAGE: search [FLAGS]

Searches Gleam packages for JavaScript code using the old API for custom types.

FLAGS:
  --refresh-packages  Normally, if a search has already been performed, `search`
    will use the cached information in `packages.txt`. If for some reason ou want
    to completely refresh the information, this will ignore the cache file.

  --write-to-file  Write the gathered information to a `results.txt` file,
    instead of printing to stdout.
",
  )
}

fn print_reports(reports: List(Report), write_to_file: Bool) -> Nil {
  let packages = list.map(reports, fn(report) { report.package }) |> list.unique

  let report =
    "Found "
    <> int.to_string(list.length(reports))
    <> " JavaScript files in "
    <> int.to_string(list.length(packages))
    <> " packages.\n\nThe following packages use JavaScript FFI:\n"

  let #(report, last, files) =
    list.fold(reports, #(report, "", []), fn(current, report) {
      let #(acc, current_name, current_files) = current
      case report.package == current_name {
        True -> #(acc, current_name, [report.file, ..current_files])
        False if current_name == "" -> #(acc, report.package, [report.file])
        False -> {
          let acc = acc <> "- " <> current_name <> ":\n"
          let acc =
            list.fold(current_files, acc, fn(acc, file) {
              acc <> "  - " <> file <> "\n"
            })

          #(acc, report.package, [report.file])
        }
      }
    })

  let report = report <> "- " <> last <> ":\n"
  let report =
    list.fold(files, report, fn(acc, file) { acc <> "  - " <> file <> "\n" })

  case write_to_file {
    True -> {
      io.print("\nWriting data to file...")
      let assert Ok(Nil) = file.write(report, to: out_file)
      io.println(" Done")
      Nil
    }
    False -> io.println(report)
  }
}

type Report {
  Report(package: String, file: String)
}

fn scan_packages(
  packages: List(Package),
  package_files: Dict(String, List(pack.File)),
) -> List(Report) {
  let package_count = int.to_string(list.length(packages))

  use reports, package, i <- list.index_fold(packages, [])

  case dict.get(package_files, package.name) {
    Error(_) -> reports
    Ok(files) -> {
      io.print("Scanning " <> package.name <> "...")

      let reports =
        list.fold(files, reports, fn(reports, file) {
          case file {
            pack.BinaryFile(..) -> reports
            pack.TextFile(name:, contents:) ->
              case filepath.extension(name) {
                Ok("mjs") | Ok("js") | Ok("cjs") ->
                  scan_javascript_file(
                    name,
                    contents,
                    package.name,
                    reports,
                    files,
                    package_files,
                  )
                _ -> reports
              }
          }
        })

      io.println(
        " Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")",
      )

      reports
    }
  }
}

fn scan_javascript_file(
  name: String,
  contents: String,
  package: String,
  reports: List(Report),
  files: List(pack.File),
  packages: Dict(String, List(pack.File)),
) -> List(Report) {
  let #(tokens, _) =
    just.new(contents)
    |> just.ignore_whitespace
    |> just.ignore_comments
    |> just.tokenise

  let has_gleam_import = search_imports(tokens, name, files, packages)

  case has_gleam_import {
    False -> reports
    True -> {
      let assert Ok(#(_, file_path_within_package)) =
        string.split_once(name, "src/")
      [Report(package:, file: file_path_within_package), ..reports]
    }
  }
}

fn search_imports(
  tokens: List(just.Token),
  file: String,
  files: List(pack.File),
  packages: Dict(String, List(pack.File)),
) -> Bool {
  case tokens {
    [] -> False
    [
      just.Import,
      just.Star,
      just.ContextualKeyword(just.From),
      just.String(contents: module, ..),
      ..tokens
    ]
    | [
        just.Import,
        just.Identifier(_),
        just.ContextualKeyword(just.From),
        just.String(contents: module, ..),
        ..tokens
      ]
    | [
        just.Import,
        just.Star,
        just.ContextualKeyword(just.As),
        just.Identifier(_),
        just.ContextualKeyword(just.From),
        just.String(contents: module, ..),
        ..tokens
      ] ->
      case is_gleam_module(module, file, files, packages) {
        True -> True
        False -> search_imports(tokens, file, files, packages)
      }
    [just.Import, just.LeftBrace, ..tokens] ->
      case parse_import(tokens, []) {
        Ok(#(module, imports, tokens)) ->
          case
            // We also check if any of the imports start with capital letters. This
            // indicates we are importing a custom type and not just a function.
            is_gleam_module(module, file, files, packages)
            && list.any(imports, is_capitalised)
          {
            True -> True
            False -> search_imports(tokens, file, files, packages)
          }
        Error(tokens) -> search_imports(tokens, file, files, packages)
      }
    [_, ..tokens] -> search_imports(tokens, file, files, packages)
  }
}

fn is_capitalised(string: String) -> Bool {
  case string {
    "A" <> _
    | "B" <> _
    | "C" <> _
    | "D" <> _
    | "E" <> _
    | "F" <> _
    | "G" <> _
    | "H" <> _
    | "I" <> _
    | "J" <> _
    | "K" <> _
    | "L" <> _
    | "M" <> _
    | "N" <> _
    | "O" <> _
    | "P" <> _
    | "Q" <> _
    | "R" <> _
    | "S" <> _
    | "T" <> _
    | "U" <> _
    | "V" <> _
    | "W" <> _
    | "X" <> _
    | "Y" <> _
    | "Z" <> _ -> True
    _ -> False
  }
}

fn parse_import(
  tokens: List(just.Token),
  imports: List(String),
) -> Result(#(String, List(String), List(just.Token)), List(just.Token)) {
  case tokens {
    [] -> Error([])
    [
      just.Identifier(name),
      just.ContextualKeyword(just.As),
      just.Identifier(_),
      ..tokens
    ]
    | [just.Identifier(name), ..tokens] ->
      parse_import(tokens, [name, ..imports])
    [just.Comma, ..tokens] -> parse_import(tokens, imports)
    [
      just.RightBrace,
      just.ContextualKeyword(just.From),
      just.String(contents: module, ..),
      ..tokens
    ] -> Ok(#(module, imports, tokens))
    [_, ..] -> Error(tokens)
  }
}

fn is_gleam_module(
  module: String,
  file: String,
  files: List(pack.File),
  packages: Dict(String, List(pack.File)),
) -> Bool {
  use <- bool.guard(filepath.base_name(module) == "gleam.mjs", True)
  use <- bool.guard(
    !{ string.starts_with(module, "./") || string.starts_with(module, "../") },
    False,
  )
  use <- bool.guard(!string.ends_with(module, ".mjs"), False)

  let path =
    file
    |> filepath.directory_name
    |> filepath.join(string.replace(module, ".mjs", ".gleam"))
    |> filepath.expand
  case path {
    Error(_) -> False
    Ok(path) -> gleam_module_exists(path, files, packages)
  }
}

fn gleam_module_exists(
  path: String,
  files: List(pack.File),
  packages: Dict(String, List(pack.File)),
) -> Bool {
  case path {
    "src/" <> _ | "dev/" <> _ | "test/" <> _ ->
      list.any(files, fn(file) { file.name == path })
    _ ->
      case string.split_once(path, "/") {
        Error(_) -> False
        Ok(#(package_name, path)) -> {
          let path = filepath.join("src", path)
          case dict.get(packages, package_name) {
            Error(_) -> False
            Ok(files) -> list.any(files, fn(file) { file.name == path })
          }
        }
      }
  }
}
