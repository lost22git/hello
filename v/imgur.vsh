#!/usr/bin/env -S v -d trace_http_request -d trace_http_response run

import net.http
import net.http.mime
import os

fn to_filedata(file_path string) !http.FileData {
	filename := os.file_name(file_path)
	mt := mime.get_mime_type(os.file_ext(file_path).all_after_first('.'))
	content_type := mime.get_content_type(mt)
	content := os.read_file(file_path)!

	return http.FileData{
		filename:     filename
		content_type: content_type
		data:         content
	}
}

fn upload(file_path string) ! {
	url := 'https://api.imgur.com/3/image?client_id=546c25a59c58ad7'

	filedata := to_filedata(file_path)!

	header := http.new_header_from_map({
		.accept:     '*/*'
		.referer:    'https://imgur.com/'
		.user_agent: 'curl/8.14.1'
	})

	resp := http.post_multipart_form(url,
		header: header
		files:  {
			'image': [filedata]
		}
	)!

	println(resp)
}

println('Begin...')
file_path := os.abs_path('./棋魂.jpg')
println('file path: ${file_path}')
upload(file_path)!

// FIXME: error since got response:
// ```
// > POST /3/image?client_id=546c25a59c58ad7 HTTP/1.1
// Host: api.imgur.com
// Content-Length: 2026
// Accept: */*
// Referer: https://imgur.com/
// User-Agent: curl/8.14.1
// Content-Type: multipart/form-data; boundary="01JY91EEK532RNF87H8TQNW5RR"
// Connection: close
//
//
// --01JY91EEK532RNF87H8TQNW5RR
// Content-Disposition: form-data; name="image"; filename="棋魂.jpg"
// Content-Type: image/jpeg; charset=utf-8
//
// ...binary data
// --01JY91EEK532RNF87H8TQNW5RR--
// <
// V panic: result not set (response does not start with HTTP/, line: ``)
// v hash: 36bef92
// ```
