#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "Time::Location"

p! Time::Location::UTC
p! Time::Location.local
p! Time::Location.load("Asia/Shanghai")
p! Time::Location.fixed(8 * 60 * 60)

__ "Time now"

p! Time.utc
p! Time.local
p! Time.local Time::Location.load("Asia/Shanghai")

__ "Time new"

p! Time.utc(2222, 2, 2, 2, 2, 2)

__ "Time zone conversion"

p! Time.utc.in Time::Location.load("Asia/Shanghai")
p! Time.utc.to_utc
p! Time.utc.to_local
p! Time.utc.to_local_in Time::Location.load("Asia/Shanghai")

__ "Time <=> String"

p! Time.parse!("2222-02-02 02:02:02 +00:00", "%Y-%m-%d %H:%M:%S %z")

# 如果字符串中不提供 timezone, 则选择参数的 timezone
p! Time.parse("2222-02-02 02:02:02 +00:00", "%Y-%m-%d %H:%M:%S %z", Time::Location.load("Asia/Shanghai"))
p! Time.parse("2222-02-02 02:02:02", "%Y-%m-%d %H:%M:%S", Time::Location.load("Asia/Shanghai"))

# 如果字符串中不提供 timezone, 则选择 local
p! Time.parse_local("2222-02-02 02:02:02 +00:00", "%Y-%m-%d %H:%M:%S %z")
p! Time.parse_local("2222-02-02 02:02:02", "%Y-%m-%d %H:%M:%S")

# 如果字符串中不提供 timezone, 则选择 utc
p! Time.parse_utc("2222-02-02 02:02:02 +00:00", "%Y-%m-%d %H:%M:%S %z")
p! Time.parse_utc("2222-02-02 02:02:02", "%Y-%m-%d %H:%M:%S")

# strict 版本的 iso8601
p! Time.parse_rfc3339("2222-02-02T02:02:02Z")
p! Time.parse_rfc3339("2222-02-02T02:02:02+08:00")

# 相比 rfc3339, iso8601 更为宽松, 允许 timezone 没有 `+` 和 `:`
p! Time.parse_iso8601("2222-02-02T02:02:02+08:00")
p! Time.parse_iso8601("2222-02-02T02:02:02+0800")

# to_rfc3339 输出的 timezone 永远是 UTC, 即: `Z`
p! Time.parse_iso8601("2222-02-02T02:02:02+0800").to_rfc3339

# 最佳实践: 使用 iso8601 解析, 使用 `"%Y-%m-%dT%H:%M:%S %z"` 输出带有 timezone 的格式
p! Time.parse_iso8601("2222-02-02T02:02:02+0800").to_s("%Y-%m-%dT%H:%M:%S %z")

__ "Time <=> unix timestamp"

# unix 秒 float
p! Time.utc.to_unix_f

# unix 秒 int
p! Time.utc.to_unix
p! Time.unix Time.utc.to_unix

# unix 毫秒 int
p! Time.utc.to_unix_ms
p! Time.unix_ms Time.utc.to_unix_ms

# unix 纳秒 int
p! Time.utc.to_unix_ns
p! Time.unix_ns Time.utc.to_unix_ns

__ "Time::Span"

p! Time::Span::MIN
p! Time::Span::MAX
p! Time::Span::ZERO

p! Time::Span::MAX.zero?
p! Time::Span::ZERO.zero?

p! Time::Span::MAX.positive?
p! Time::Span::MAX.negative?

p! Time::Span.new days: 2, hours: 2, minutes: 2, seconds: 2

p! 1.days

p! 1.days.total_hours

__ "Time::Span 和 Time"

p! Time.utc + 1.days
p! Time.utc(2222, 2, 2, 2, 2, 2) - Time.utc(2222, 2, 1, 2, 2, 2)

# 等同于 Time.local - 1.days
p! 1.days.ago

# 等同于 Time.local + 1.days
p! 1.days.from_now
