table.unpack = table.unpack or unpack

local log = {}

local ansi_style = {
	RESET = "\27[m",
	BOLD_RED = "\27[1;31m",
	BOLD_GREEN = "\27[1;32m",
	BOLD_YELLOW = "\27[1;33m",
	BOLD_BLUE = "\27[1;34m",
}

log.levels = {
	DBG = { "DBG", 1, ansi_style.BOLD_BLUE },
	INF = { "INF", 2, ansi_style.BOLD_GREEN },
	WAR = { "WAR", 3, ansi_style.BOLD_YELLOW },
	ERR = { "ERR", 4, ansi_style.BOLD_RED },
}

log.min_level = log.levels.INF
log.date_format = "!%Y-%m-%d %H:%M:%S" -- UTC TIME

local function styled_level(level)
	local result = level[1]
	result = string.upper(string.sub(result, 1, 1))
	result = level[3] .. result .. ansi_style.RESET
	return result
end

function log.info(...)
	log.logit(log.levels.INF, ...)
end

function log.debug(...)
	log.logit(log.levels.DBG, ...)
end

function log.warn(...)
	log.logit(log.levels.WAR, ...)
end

function log.err(...)
	log.logit(log.levels.ERR, ...)
end

function log.loggable(level)
	return log.min_level[2] <= level[2]
end

function log.logit(level, ...)
	if not log.loggable(level) then
		return
	end
	local level_str = styled_level(level)
	local unix_now = os.date(log.date_format)
	print("[" .. unix_now .. "]" .. " " .. "[" .. level_str .. "]", ...)
end

-------- Usage -------------------------

log.min_level = log.levels.DBG

log.debug("hello", "lua")
log.info("hello", "lua")
log.warn("hello", "lua")
log.err("hello", "lua")
