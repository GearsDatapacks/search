import gleam/bool
import gleam/dynamic/decode
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import simplifile as file

const packages_api_url = "https://packages.gleam.run/api/packages/"

const data_file = "packages.txt"

const tarballs_directory = "packages/tarballs"

const source_zips_directory = "packages/source_zips"

const hex_api_url = "https://repo.hex.pm/tarballs/"

pub type Package {
  Package(name: String, latest_version: String, downloads: Int)
}

fn package_decoder() -> decode.Decoder(Package) {
  use name <- decode.field("name", decode.string)
  use latest_version <- decode.field("latest-version", decode.string)
  use downloads <- decode.field(
    "releases",
    decode.list(decode.at(
      [
        "downloads",
      ],
      decode.int,
    ))
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

  Nil
}

fn decompress_packages(packages: List(Package)) -> Nil {
  let assert Ok(Nil) = file.create_directory_all(source_zips_directory)

  let package_count = int.to_string(list.length(packages))

  use package, i <- index_each(packages)

  let tarball_name = package.name <> "-" <> package.latest_version <> ".tar"

  io.print("Extracting source zip file from " <> tarball_name <> "...")

  let assert Ok(contents) =
    file.read_bits(tarballs_directory <> "/" <> tarball_name)

  let assert Ok(contents) = decompress(contents)

  let zip_path = source_zips_directory <> "/" <> tarball_name <> ".gz"

  io.print(" Done. Writing contents to " <> zip_path <> "...")

  let assert Ok(Nil) = file.write_bits(contents, to: zip_path)

  io.println(" Done (" <> int.to_string(i + 1) <> "/" <> package_count <> ")")
}

@external(erlang, "search_ffi", "decompress")
fn decompress(contents: BitArray) -> Result(BitArray, Nil)

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
