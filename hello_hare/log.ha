///usr/bin/env hare run "$0" "$@" ; exit $?

use fmt;
use io;
use log;
use os;
use time::date;

export fn main() void = {
	let sink = new(os::stdout);
	log::setlogger(&sink);

	log::printfln("你好, {} ;)", LEVEL_DBG, "Hare");
	log::printfln("你好, {} ;)", LEVEL_INF, "Hare");
	log::printfln("你好, {} ;)", LEVEL_WAR, "Hare");
	log::printfln("你好, {} ;)", LEVEL_ERR, "Hare");

	setlevel(&sink, level::DBG);

	log::println(LEVEL_DBG, "你好", "Hare");
	log::println(LEVEL_INF, "你好", "Hare");
	log::println(LEVEL_WAR, "你好", "Hare");
	log::println(LEVEL_ERR, "你好", "Hare");
};

////////////
// logger //
////////////

type mylogger = struct {
	log::logger,
	level: level,
	levelfmt: *fn(sink: *log::logger, lvl: level) void,
	levelfilter: *fn(sink: *mylogger, lvl: level) bool,
	sink: io::handle,
};


fn new(sink: io::handle) mylogger = {
	return mylogger {
		println = &log_println,
		printfln = &log_printfln,
		level = level::INF,
		levelfmt = &level_fmt,
		levelfilter = &levelfilter,
		sink = sink,
	};
};

fn setlevel(sink: *mylogger, lvl: level) void = {
	sink.level = lvl;
};

fn levelfilter(sink: *mylogger, lvl: level) bool = {
	return lvl >= sink.level;
};

fn log_println(sink: *log::logger, fields: fmt::formattable...) void = {
	const sink = sink: *mylogger;
	assert(sink.println == &log_println);

	const lvl = level_parse(fields[0]: str);

	const loggable =  match (lvl) {
	case let l: level => yield sink.levelfilter(sink, l);
	case invalid => yield true;
	};

	if(!loggable) return;

	// time field
	const now = date::now();
	date::format(sink.sink, date::RFC3339, &now): void;
	
	// level field
	const rest_fields = match(lvl) {
	case let l: level => 
		fmt::fprint(sink.sink, " ["): void;
		sink.levelfmt(sink, l): void;
		fmt::fprint(sink.sink, "] "): void;
		yield fields[1..];
	case invalid => 
		fmt::fprint(sink.sink, " "): void;
		yield fields;
	};

	// rest fields
	fmt::fprintln(sink.sink, rest_fields...): void;
};

fn log_printfln(sink: *log::logger, fmt: str, fields: fmt::field...) void = {
	const sink = sink: *mylogger;
	assert(sink.printfln == &log_printfln);

	const lvl = level_parse(fields[0]: str);

	const loggable =  match (lvl) {
	case let l: level => yield sink.levelfilter(sink, l);
	case invalid => yield true;
	};

	if(!loggable) return;

	// time field
	const now = date::now();
	date::format(sink.sink, date::RFC3339, &now): void;

	// level field
	const rest_fields = match(lvl) {
	case let l: level =>
		fmt::fprint(sink.sink, " ["): void;
		sink.levelfmt(sink, l): void;
		fmt::fprint(sink.sink, "] "): void;
		yield fields[1..];
	case invalid => 
		fmt::fprint(sink.sink, " "): void;
		yield fields;
	};

	// rest fields
	fmt::fprintfln(sink.sink, fmt, rest_fields...): void;
};



///////////
// level //
///////////

type level = enum i8 {
	DBG = 0,
	INF = 10,
	WAR = 20,
	ERR = 30,
};

def LEVEL_DBG = "DBG";
def LEVEL_INF = "INF";
def LEVEL_WAR = "WAR";
def LEVEL_ERR = "ERR";

type invalid = !void;

fn level_name(lvl: level) str = {
	return switch(lvl) {
		case level::DBG => yield LEVEL_DBG;
		case level::INF => yield LEVEL_INF;
		case level::WAR => yield LEVEL_WAR;
		case level::ERR => yield LEVEL_ERR;
	};
};

fn level_parse(lvl: str) (level | invalid) = {
	return switch(lvl) {
		case LEVEL_DBG => yield level::DBG;
		case LEVEL_INF => yield level::INF;
		case LEVEL_WAR => yield level::WAR;
		case LEVEL_ERR => yield level::ERR;
		case => yield invalid;
	};
};

fn level_fmt(sink: *log::logger ,lvl: level) void = {
	const sink = sink: *mylogger;
	assert(sink.println == &log_println);

	const csi = "\x1b[";
	const reset = "\x1b[m";
	const code = switch(lvl) {
	case level::DBG => yield 34;
	case level::INF => yield 32;
	case level::WAR => yield 33;
	case level::ERR => yield 31;
	};

	fmt::fprint(sink.sink, csi): void;
	fmt::fprint(sink.sink, "1;"): void; // bold
	fmt::fprint(sink.sink, code): void; // color
	fmt::fprint(sink.sink, "m"): void;
	fmt::fprint(sink.sink, level_name(lvl)): void; // level
	fmt::fprint(sink.sink, reset): void; // reset
};
