import gleam/bytes_tree
import gleam/erlang/process
import gleam/http
import gleam/http/request.{Request as RequestValue}
import gleam/http/response.{
  type Response as HttpResponse, Response as ResponseValue,
}
import gleam/httpc
import gleam/list
import gleam/result.{map, map_error, try}
import gleam/string_tree.{type StringTree}
import mist
import wisp.{type Request, type Response}
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    wisp_mist.handler(handle_request, secret_key_base)
    |> mist.new
    |> mist.bind("0.0.0.0")
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}

fn middleware(req: Request, handle_request: fn(Request) -> Response) -> Response {
  let req = wisp.method_override(req)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)

  handle_request(req)
}

/// App Error
///
type AppErr {
  ParamNotFound(value: String)
  FailedToClientRequestInit(target: String)
  FailedToClientRequestSend(target: String)
  FailedToServerRequestReadBody
}

/// App Error handler
///
fn handle_app_err(err: AppErr) -> Response {
  case err {
    ParamNotFound(s) ->
      string_tree.from_string("Param not found:" <> s)
      |> plain_response(400)
    FailedToClientRequestInit(s) ->
      string_tree.from_string("Failed to init client request, target:" <> s)
      |> plain_response(500)
    FailedToClientRequestSend(s) ->
      string_tree.from_string("Failed to send client request, target:" <> s)
      |> plain_response(500)
    FailedToServerRequestReadBody ->
      string_tree.from_string("Failed to read body of server request")
      |> plain_response(500)
  }
}

/// server request handler
///
fn handle_request(req: Request) -> Response {
  use req <- middleware(req)

  case wisp.path_segments(req) {
    // This matches `/proxy`.
    ["proxy"] -> api_proxy(req) |> recover(handle_app_err)

    // This matches all other paths.
    _ -> wisp.not_found()
  }
}

/// proxy route handler
///
fn api_proxy(req: Request) -> Result(Response, AppErr) {
  fetch_target(req) |> map(from_target_response)
}

/// fetch target
/// send target client request and receive target client response
///
fn fetch_target(req: Request) -> Result(HttpResponse(BitArray), AppErr) {
  // get target from request query params
  use query <- try(
    request.get_query(req) |> map_error(fn(_) { ParamNotFound("target") }),
  )
  use target <- try(
    list.key_find(query, "target")
    |> map_error(fn(_) { ParamNotFound("target") }),
  )

  wisp.log_info(
    "Fetching" <> " [" <> http.method_to_string(req.method) <> "] " <> target,
  )

  // init target request
  use target_req <- try(
    request.to(target) |> map_error(fn(_) { FailedToClientRequestInit(target) }),
  )
  // target request set body
  use req_body <- try(
    wisp.read_body_to_bitstring(req)
    |> map_error(fn(_) { FailedToServerRequestReadBody }),
  )
  let target_req =
    request.set_body(target_req, req_body)
    // target request set method
    |> request.set_method(req.method)

  // target request set headers
  let target_req = RequestValue(..target_req, headers: req.headers)

  // send target request
  httpc.send_bits(target_req)
  |> map_error(fn(_) { FailedToClientRequestSend(target) })
}

/// convert target client response into server response
///
fn from_target_response(target_res: HttpResponse(BitArray)) -> Response {
  let res =
    wisp.response(target_res.status)
    |> wisp.set_body(
      target_res.body
      |> bytes_tree.from_bit_array
      |> wisp.Bytes,
    )
  ResponseValue(..res, headers: target_res.headers)
}

fn recover(r: Result(a, b), recover_fn: fn(b) -> a) -> a {
  case r {
    Ok(v) -> v
    Error(v) -> recover_fn(v)
  }
}

/// plain response
///
fn plain_response(body: StringTree, status: Int) -> Response {
  wisp.response(status)
  |> wisp.string_tree_body(body)
  |> wisp.set_header("content-type", "text/plain; charset=utf-8")
}
