#!/usr/bin/env -S v run

import term
import time

fn main() {
	// communicate with light threads via channel
	ch := chan int{}
	// light thread
	go show(
		ch:     ch
		total:  100
		cell:   '░'
		prefix: fn (total int, current int) string {
			return '|${current}/${total}|'
		}
	)

	for {
		time.sleep(1000 * time.millisecond)
		ch <- 10 or { break }
	}
}

pub type PrefixFn = fn (total int, current int) string

// sum type (type union)
pub type Prefix = string | PrefixFn

@[params]
pub struct Params {
	ch               chan int @[required]
	cell             string = '▁' //  '▁' '░'
	prefix           Prefix
	refresh_duration time.Duration = 100 * time.millisecond
	init             int
	total            int @[required]
}

// use params struct as function param for named params
pub fn show(params Params) {
	defer {
		params.ch.close()
		term.show_cursor()
	}
	term.hide_cursor()

	ch := params.ch
	cell := params.cell
	prefix := params.prefix
	refresh_duration := params.refresh_duration

	total := params.total
	mut current := params.init

	show0(prefix, cell, total, current)

	for {
		// select from multiple channels
		select {
			delta := <-ch {
				current += delta
			}
			i64(refresh_duration) {}
		}

		if current > total {
			current = total
			break
		}

		show0(prefix, cell, total, current)
	}

	show0(prefix, cell, total, current)
	println('')
}

fn show0(prefix_provider Prefix, cell string, total int, current int) {
	term_cols, _ := term.get_terminal_size()

	prefix := match prefix_provider {
		string {
			prefix_provider
		}
		PrefixFn {
			prefix_provider(total, current)
		}
	}

	total_cols := term_cols - prefix.len
	current_cols := current * total_cols / total

	print('\r')
	print(prefix)
	print(term.rgb(0, 0xFF, 0, cell.repeat(current_cols)))
	print(term.rgb(0xFF, 0xFF, 0xFF, cell.repeat(total_cols - current_cols)))
}
