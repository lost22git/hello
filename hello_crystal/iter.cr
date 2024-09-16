#!/usr/bin/env crystal

# - `Iterable(T)#each() : Iterator(T)`
#
# - `Iterator(T)#next() : T`
#
# - `Iterator(T) include Enumerable(T)`
#
# - `Enumerable(T)` is **eager**，`Iterator(T)` is **lazy**

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "select/reject map each"

(1..10).each
  .select { |i| i < 5 }
  .reject { |i| i < 3 }
  .map { |i| "v-#{i}" }
  .each { |i| p i }

__ "skip and first"

(1..10).each
  .skip(3)
  .first(2)
  .each { |i| p i }

__ "min/max/minmax/sum/size/join"

p! (1..10).each
  .skip(3)
  .first(2)
  .min

p! (1..10).each
  .skip(3)
  .first(2)
  .max

p! (1..10).each
  .skip(3)
  .first(2)
  .minmax

p! (1..10).each
  .skip(3)
  .first(2)
  .sum

p! (1..10).each
  .skip(3)
  .first(2)
  .size

p! (1..10).each
  .skip(3)
  .first(2)
  .join ","

__ "reduce"

p! (1..10).each
  .skip(3)
  .first(2)
  .reduce(10) { |acc, i| acc + i }

__ "any?/all?/none?/one?"

p! (1..10).each.any? { |i| i > 0 }
p! (1..10).each.all? { |i| i > 0 }
p! (1..10).each.none? { |i| i > 0 }
p! (1..10).each.one? { |i| i > 0 }

__ "compact_map (aka. filter_map)"

(1..10).each
  .compact_map { |i| i.even? ? "even-#{i}" : nil }
  .each { |i| p i }

__ "step"

(1..10).each
  .step(3)
  .each { |i| p i }

__ "slice"

(1..10).each
  .slice(3)
  .each { |i| p i }

__ "slice_before"

(1..10).each
  .slice_before { |i| i.even? }
  .each { |i| p i }

__ "slice_after"

(1..10).each
  .slice_after { |i| i.even? }
  .each { |i| p i }

__ "slice_when (simulate silice_after)"

(1..10).each
  .slice_when { |last, current| last.even? }
  .each { |i| p i }

__ "slice_when (simulate silice_before)"

(1..10).each
  .slice_when { |last, current| current.even? }
  .each { |i| p i }

__ "in_groups_of"

(1..10).each
  .in_groups_of(3)
  .each { |i| p i }

__ "in_groups_of with default value"

(1..10).each
  .in_groups_of(3, 999)
  .each { |i| p i }

__ "cons (aka. window)"
(1..10).each
  .cons(3)
  .each { |i| p i }

__ "cons_pair"

(1..10).each
  .cons_pair
  .each { |i| p i }

__ "cycle"

(1..10).each
  .cycle(2)
  .each_slice(5)
  .each { |i| p i }

__ "with_index"

(1..10).each
  .with_index(10)
  .each { |i| p i }

__ "chain"

iter = (1..5).each
iter2 = (6..10).each
iter_iter = {iter, iter2}.each
p Iterator.chain(iter_iter).to_a

__ "zip"

iter = (1..5).each
iter2 = (6..10).each
p iter.zip(iter2).to_a
