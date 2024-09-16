package proxy_server

import "core:bytes"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:net"
import "core:strings"

import http "./odin-http"
import client "./odin-http/client"

main :: proc() {
	track(run)
}

track :: proc(code: proc()) {
	track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&track, context.allocator)
	context.allocator = mem.tracking_allocator(&track)

	defer {
		if len(track.allocation_map) > 0 {
			fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
			for _, entry in track.allocation_map {
				fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
			}
		}
		if len(track.bad_free_array) > 0 {
			fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
			for entry in track.bad_free_array {
				fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
			}
		}
		fmt.printf("Peak memory allocated: %v\n", track.peak_memory_allocated)
		mem.tracking_allocator_destroy(&track)
	}

	code()
}

run :: proc() {
	context.logger = log.create_console_logger()
	context.logger.options = log.Options{.Level, .Date, .Time, .Terminal_Color, .Thread_Id}
	context.logger.lowest_level = .Info

	svr: http.Server
	// Register a graceful shutdown when the program receives a SIGINT signal.
	http.server_shutdown_on_interrupt(&svr)

	// register routers
	router: http.Router
	http.router_init(&router)
	defer http.router_destroy(&router)

	http.route_all(&router, `(.*)`, http.handler(api_proxy))

	handler := http.router_handler(&router)

	// start server
	ep := net.Endpoint {
		address = net.IP4_Loopback,
		port    = 8000,
	}
	log.infof("Proxy server is listening on %v", ep)

	if err := http.listen_and_serve(&svr, handler, ep); err != nil {
		log.errorf("Server stopped: %s", err)
	}
}

RequestContext :: struct {
	req: ^http.Request,
	res: ^http.Response,
}

api_proxy :: proc(req: ^http.Request, res: ^http.Response) {
	log.debug("[api_proxy]")
	log.debugf("[api_proxy] url: %v", req.url)

	// check source request whether exists param: target
	_, ok := get_proxy_target(req)
	if !ok {
		http.respond_plain(res, fmt.aprint("Invalid param: target"), .Bad_Request)
		return
	}

	// async read source requset body and do proxy exchange
	req_ctx := new_clone(RequestContext{req = req, res = res})
	http.body(
		req,
		user_data = req_ctx,
		cb = proc(req_ctx: rawptr, body: http.Body, err: http.Body_Error) {
			req_ctx := cast(^RequestContext)req_ctx
			defer free(req_ctx)

			req := req_ctx.req
			res := req_ctx.res

			if err != nil {
				http.respond(res, http.body_error_status(err))
				return
			}

			target := get_proxy_target(req)
			method := req.line.?.method
			log.infof("[api_proxy] Do request: [%v] %v", method, target)

			// target client request init and send
			client_res, err := client_request(
				target = target,
				method = method,
				headers = req.headers,
				body = body,
			)
			if err != nil {
				http.respond_plain(
					res,
					fmt.aprintf("Request failed: %s", err),
					.Internal_Server_Error,
				)
				return
			}
			defer client.response_destroy(&client_res)

			// target client response read body
			client_res_body, allocation, berr := client.response_body(&client_res)
			if berr != nil {
				http.respond_plain(
					res,
					fmt.aprintf("Error retrieving response body: %s", berr),
					.Internal_Server_Error,
				)
				return
			}
			defer client.body_destroy(client_res_body, allocation)

			body_string := ""
			switch b in client_res_body {
			case client.Body_Plain:
				body_string = b
			case client.Body_Url_Encoded:
			case client.Body_Error:
			}

			// respond to source
			server_respond(
				res,
				status = client_res.status,
				headers = client_res.headers,
				body = body_string,
			)
		},
	)
}

query_get :: proc(query: ^string, key: string) -> (val: string, ok: bool) #optional_ok {
	for entry in #force_inline http.query_iter(query) {
		if entry.key == key {
			return entry.value, true
		}
	}
	return
}

get_proxy_target :: proc(req: ^http.Request) -> (target: string, ok: bool) #optional_ok {
	path := req.url.raw
	index := strings.index(path, "?")
	query := ""
	if index >= 0 {
		query = strings.cut(path, index + 1, len(path) - index - 1)
		target = query_get(&query, "target") or_return
		return target, true
	} else {
		return target, false
	}
}

headers_clone_from :: proc(dst: ^http.Headers, src: http.Headers) {
	for k, v in src._kv {
		http.headers_set(dst, k, v)
	}
}

client_request :: proc(
	target: string,
	method: http.Method,
	headers: http.Headers,
	body: string,
) -> (
	client.Response,
	client.Error,
) {
	// init request
	client_req: client.Request
	client.request_init(&client_req, method)
	defer client.request_destroy(&client_req)
	// set headers
	headers_clone_from(&client_req.headers, headers)
	// set body
	bytes.buffer_init_string(&client_req.body, body)
	// send request
	return client.request(&client_req, target)
}

server_respond :: proc(
	res: ^http.Response,
	status: http.Status,
	headers: http.Headers,
	body: string,
) {
	headers_clone_from(&res.headers, headers)
	http.body_set_str(res, body)
	http.respond(res, status)
}
