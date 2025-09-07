import argv
import filepath
import gleam/bool
import gleam/dynamic/decode
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import just
import simplifile as file

const packages_api_url = "https://packages.gleam.run/api/packages/"

const data_file = "packages.txt"

const sources_directory = "packages"

const hex_api_url = "https://repo.hex.pm/tarballs/"

const out_file = "results.txt"

pub type Package {
  Package(name: String, latest_version: String, downloads: Int)
}

fn package_decoder() -> decode.Decoder(Package) {
  use name <- decode.field("name", decode.string)
  use latest_version <- decode.field("latest-version", decode.string)
  use downloads <- decode.field(
    "releases",
    decode.list(decode.at(["downloads"], decode.int))
      |> decode.map(int.sum),
  )
  decode.success(Package(name:, latest_version:, downloads:))
}

fn api_response_decoder() -> decode.Decoder(List(String)) {
  decode.at(["data"], decode.list(decode.at(["name"], decode.string)))
}

pub fn main() -> Nil {
  let args = argv.load().arguments

  use <- bool.lazy_guard(list.contains(args, "--help"), print_help_text)

  let refresh_packages = list.contains(args, "--refresh-packages")
  let download_missing = list.contains(args, "--download-missing")
  let write_to_file = list.contains(args, "--write-to-file")

  let packages = case file.is_file(data_file) {
    Ok(True) if !refresh_packages -> {
      io.print("Reading packages from file...")
      let packages = read_data_from_file()
      io.println(" Done")
      packages
    }
    Ok(False) | Ok(True) -> {
      let packages = get_packages()
      let packages =
        list.sort(packages, fn(a, b) { int.compare(b.downloads, a.downloads) })
      io.print("Writing packages to file...")
      write_data_to_file(packages)
      io.println(" Done")
      packages
    }
    Error(_) -> panic
  }

  case file.is_directory(sources_directory) {
    Ok(True) if !download_missing -> Nil
    Ok(False) | Ok(True) -> download_packages(packages)
    Error(_) -> panic
  }

  let reports = list.reverse(scan_packages(packages))
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

  --download-missing  Normally, if a search has already been performed, `search`
    will not attempt to download packages and read from the `packages` directory.
    If `--download-missing` is passed, `search` will go through the package list
    again, and if the package is not found in the `packages` directory, it will be
    downloaded.

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

fn scan_packages(packages: List(Package)) -> List(Report) {
  let package_count = int.to_string(list.length(packages))

  use reports, package, i <- list.index_fold(packages, [])

  let package_directory = sources_directory <> "/" <> package.name

  // If the package index contains outdated information containing non-existent
  // packages, and therefore the directory for a particular package is missing,
  // we can just skip that package.
  use <- bool.lazy_guard(
    file.is_directory(package_directory) == Ok(False),
    return: fn() {
      io.println(
        "Package " <> package.name <> " missing from files, skipping...",
      )
      reports
    },
  )

  io.print("Scanning " <> package.name <> "...")

  let assert Ok(files) = file.get_files(package_directory)

  let reports =
    list.fold(files, reports, fn(reports, file) {
      case filepath.extension(file) {
        Ok("mjs") | Ok("js") | Ok("cjs") ->
          scan_javascript_file(file, package.name, reports)
        _ -> reports
      }
    })

  io.println(" Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")")

  reports
}

fn scan_javascript_file(
  file: String,
  package: String,
  reports: List(Report),
) -> List(Report) {
  let assert Ok(contents) = file.read(file)

  let #(tokens, _) =
    just.new(contents)
    |> just.ignore_whitespace
    |> just.ignore_comments
    |> just.tokenise

  let has_gleam_import = search_imports(tokens, file)

  case has_gleam_import {
    False -> reports
    True -> {
      let assert Ok(#(_, file_path_within_package)) =
        string.split_once(file, "src/")
      [Report(package:, file: file_path_within_package), ..reports]
    }
  }
}

fn search_imports(tokens: List(just.Token), file: String) -> Bool {
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
      case is_gleam_module(module, file) {
        True -> True
        False -> search_imports(tokens, file)
      }
    [just.Import, just.LeftBrace, ..tokens] ->
      case parse_import(tokens, []) {
        Ok(#(module, imports, tokens)) ->
          case
            // We also check if any of the imports start with capital letters. This
            // indicates we are importing a custom type and not just a function.
            is_gleam_module(module, file) && list.any(imports, is_capitalised)
          {
            True -> True
            False -> search_imports(tokens, file)
          }
        Error(tokens) -> search_imports(tokens, file)
      }
    [_, ..tokens] -> search_imports(tokens, file)
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

fn is_gleam_module(module: String, current_path: String) -> Bool {
  use <- bool.guard(filepath.base_name(module) == "gleam.mjs", True)
  use <- bool.guard(
    !{ string.starts_with(module, "./") || string.starts_with(module, "../") },
    False,
  )
  use <- bool.guard(!string.ends_with(module, ".mjs"), False)
  let assert Ok(path) =
    current_path
    |> filepath.directory_name
    |> filepath.join(string.replace(module, ".mjs", ".gleam"))
    |> filepath.expand
  file.is_file(path) == Ok(True)
}

@external(erlang, "search_ffi", "extract_all_files")
fn extract_files(contents: BitArray) -> Result(List(#(String, String)), Nil)

fn download_packages(packages: List(Package)) -> Nil {
  io.println("Downloading packages...")

  let package_count = int.to_string(list.length(packages))

  use package, i <- index_each(packages)

  let file_name = package.name <> "-" <> package.latest_version <> ".tar"
  let directory_path = sources_directory <> "/" <> package.name

  use <- bool.lazy_guard(file.is_directory(directory_path) == Ok(True), fn() {
    io.println(
      "Package " <> package.name <> " has already been downloaded, skipping...",
    )
  })

  io.print("Downloading " <> file_name <> "...")

  let assert Ok(request) = request.to(hex_api_url <> file_name)

  let assert Ok(response) = httpc.send_bits(request.set_body(request, <<>>))

  // Sometimes the package API contains old information so we try to download
  // non-existent Hex packages. In that case, we can just skip them.
  use <- bool.lazy_guard(response.status == 404, fn() {
    io.println("\nCould not find package " <> package.name <> ", skipping...")
  })

  assert response.status == 200

  io.println(" Done")
  io.print("Extracting files from " <> file_name <> "...")

  let assert Ok(files) = extract_files(response.body)

  io.println(" Done")
  io.print("Writing files to " <> directory_path <> "...")

  list.each(files, fn(file) {
    let #(path, contents) = file
    let file_path = directory_path <> "/" <> path
    let assert Ok(Nil) =
      file.create_directory_all(filepath.directory_name(file_path))
    let assert Ok(Nil) = file.write(contents:, to: file_path)
  })

  io.println(" Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")")
}

fn index_each(list: List(a), f: fn(a, Int) -> b) -> Nil {
  do_index_each(list, f, 0)
}

fn do_index_each(list: List(a), f: fn(a, Int) -> b, i: Int) -> Nil {
  case list {
    [] -> Nil
    [first, ..rest] -> {
      f(first, i)
      do_index_each(rest, f, i + 1)
    }
  }
}

fn write_data_to_file(packages: List(Package)) -> Nil {
  let assert Ok(Nil) =
    packages
    |> list.map(fn(package) {
      package.name
      <> "-"
      <> package.latest_version
      <> ": "
      <> int.to_string(package.downloads)
    })
    |> string.join("\n")
    |> file.write(to: data_file)

  Nil
}

fn read_data_from_file() -> List(Package) {
  let assert Ok(contents) = file.read(data_file)

  use line <- list.map(string.split(contents, "\n"))
  let assert Ok(#(name, downloads)) = string.split_once(line, ": ")
  let assert Ok(#(name, latest_version)) = string.split_once(name, "-")
  let assert Ok(downloads) = int.parse(downloads)
  Package(name:, latest_version:, downloads:)
}

fn get_packages() -> List(Package) {
  io.print("Fetching package list...")

  let assert Ok(request) = request.to(packages_api_url)
  let assert Ok(response) = httpc.send(request)

  assert response.status == 200
  let assert Ok(package_names) =
    json.parse(response.body, api_response_decoder())

  let package_count = int.to_string(list.length(package_names))

  io.println(" Done")

  use name, i <- list.index_map(package_names)

  io.print("Getting info for " <> name <> "...")

  let assert Ok(request) = request.to(packages_api_url <> name)
  let assert Ok(response) = httpc.send(request)

  assert response.status == 200
  let assert Ok(package) =
    json.parse(response.body, decode.at(["data"], package_decoder()))

  io.println(" Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")")

  package
}
