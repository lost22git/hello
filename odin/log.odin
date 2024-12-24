///usr/bin/env odin run "$0" -file "$@" ; exit $?

package main

import "core:fmt"
import "core:log"
import "core:strings"

main :: proc() {
	context.logger = log.create_console_logger()

	log_some_msg()

	print_msg("change log level")
	context.logger.lowest_level = log.Level.Error

	log_some_msg()

	print_msg("add Thread_Id option")
	context.logger.options |= log.Options{log.Option.Thread_Id}

	log_some_msg()
}

print_msg :: proc(msg: string) {
	fmt.println(strings.repeat("-", 77), msg)
}

log_some_msg :: proc() {
	log.debug("hello", "odin")
	log.info("hello", "odin")
	log.warn("hello", "odin")
	log.error("hello", "odin")
	log.fatal("hello", "odin")
}
