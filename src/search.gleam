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

const tarballs_directory = "packages/tarballs"

const source_zips_directory = "packages/source_zips"

const sources_directory = "packages/sources"

const hex_api_url = "https://repo.hex.pm/tarballs/"

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
  let packages = case file.is_file(data_file) {
    Ok(True) -> {
      io.print("Reading packages from file...")
      let packages = read_data_from_file()
      io.println(" Done")
      packages
    }
    Ok(False) -> {
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

  case file.is_directory(tarballs_directory) {
    Ok(True) -> Nil
    Ok(False) -> download_packages(packages)
    Error(_) -> panic
  }

  case file.is_directory(source_zips_directory) {
    Ok(True) -> Nil
    Ok(False) -> decompress_packages(packages)
    Error(_) -> panic
  }

  case file.is_directory(sources_directory) {
    Ok(True) -> Nil
    Ok(False) -> extract_sources(packages)
    Error(_) -> panic
  }

  let reports = list.reverse(scan_packages(packages))
  print_reports(reports)

  Nil
}

fn print_reports(reports: List(Report)) -> Nil {
  let packages = list.map(reports, fn(report) { report.package }) |> list.unique

  io.println(
    "Found "
    <> int.to_string(list.length(reports))
    <> " JavaScript files in "
    <> int.to_string(list.length(packages))
    <> " packages.\n",
  )

  io.println("The following packages use JavaScript FFI:\n")

  list.fold(reports, #("", []), fn(current, report) {
    let #(current_name, current_files) = current
    case report.package == current_name {
      True -> #(current_name, [report.file, ..current_files])
      False if current_name == "" -> #(report.package, [report.file])
      False -> {
        io.println("- " <> current_name <> ":")
        list.each(current_files, fn(file) { io.println("  - " <> file) })

        #(report.package, [report.file])
      }
    }
  })

  Nil
}

type Report {
  Report(package: String, file: String)
}

fn scan_packages(packages: List(Package)) -> List(Report) {
  let package_count = int.to_string(list.length(packages))

  use reports, package, i <- list.index_fold(packages, [])

  io.print("Scanning " <> package.name <> "...")

  let assert Ok(files) =
    file.get_files(sources_directory <> "/" <> package.name)

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
    True -> [Report(package:, file:), ..reports]
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

fn extract_sources(packages: List(Package)) -> Nil {
  let assert Ok(Nil) = file.create_directory_all(source_zips_directory)

  let package_count = int.to_string(list.length(packages))

  use package, i <- index_each(packages)

  let tarball_name = package.name <> "-" <> package.latest_version <> ".tar.gz"

  io.print("Extracting source files from " <> tarball_name <> "...")

  let assert Ok(contents) =
    file.read_bits(source_zips_directory <> "/" <> tarball_name)

  let assert Ok(decompressed) = decompress_gzip(contents)

  let assert Ok(files) = extract_files(decompressed)

  let directory_path = sources_directory <> "/" <> package.name

  io.print(" Done. Writing files to " <> directory_path <> "...")

  list.each(files, fn(file) {
    let #(path, contents) = file
    let file_path = directory_path <> "/" <> path
    let assert Ok(Nil) =
      file.create_directory_all(filepath.directory_name(file_path))
    let assert Ok(Nil) = file.write(contents:, to: file_path)
  })

  io.println(" Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")")
}

fn decompress_packages(packages: List(Package)) -> Nil {
  let assert Ok(Nil) = file.create_directory_all(source_zips_directory)

  let package_count = int.to_string(list.length(packages))

  use package, i <- index_each(packages)

  let tarball_name = package.name <> "-" <> package.latest_version <> ".tar"

  io.print("Extracting source zip file from " <> tarball_name <> "...")

  let assert Ok(contents) =
    file.read_bits(tarballs_directory <> "/" <> tarball_name)

  let assert Ok(contents) = extract_contents(contents)

  let zip_path = source_zips_directory <> "/" <> tarball_name <> ".gz"

  io.print(" Done. Writing contents to " <> zip_path <> "...")

  let assert Ok(Nil) = file.write_bits(contents, to: zip_path)

  io.println(" Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")")
}

@external(erlang, "search_ffi", "extract_contents_zip")
fn extract_contents(contents: BitArray) -> Result(BitArray, Nil)

@external(erlang, "search_ffi", "decompress_gzip")
fn decompress_gzip(contents: BitArray) -> Result(BitArray, Nil)

@external(erlang, "search_ffi", "extract_all_files")
fn extract_files(contents: BitArray) -> Result(List(#(String, String)), Nil)

fn download_packages(packages: List(Package)) -> Nil {
  io.println("Downloading tarballs...")

  let assert Ok(Nil) = file.create_directory_all(tarballs_directory)
  let package_count = int.to_string(list.length(packages))

  use package, i <- index_each(packages)

  let file_name = package.name <> "-" <> package.latest_version <> ".tar"
  let file_path = tarballs_directory <> "/" <> file_name

  use <- bool.lazy_guard(file.is_file(file_path) == Ok(True), fn() {
    io.println("Skipping " <> file_name <> ", it is already downloaded")
  })

  io.print("Downloading " <> file_name <> "...")

  let assert Ok(request) = request.to(hex_api_url <> file_name)

  let assert Ok(response) = httpc.send_bits(request.set_body(request, <<>>))

  assert response.status == 200

  let assert Ok(Nil) = file.write_bits(file_path, response.body)

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
