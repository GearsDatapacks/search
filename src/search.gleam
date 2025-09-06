import gleam/dynamic/decode
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import simplifile as file

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

  Nil
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

const packages_api_url = "https://packages.gleam.run/api/packages/"

const data_file = "packages.txt"

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
