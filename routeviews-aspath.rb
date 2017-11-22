#!/usr/bin/env ruby

routes = loop_routes = 0

prefix_hash = {}
asn = {}

longest = 0
sum = 0
prefix_count = 0

5.times { gets }
while l = gets
  #    Network            Next Hop            Metric LocPrf Weight Path
  # *  1.177.160.0/22     185.44.116.1             0      0      0 47872 15412 9848 38091 38091 38091 38091 38091 38091 9845 9697 i

  len = l.split.size - 7

  prefix_count += 1
  sum += len

  if len > longest
    puts "path length = #{len}"
    puts l

    longest = len
  end

  puts "avg = #{sum.to_f / prefix_count.to_f}" if prefix_count % 100000 == 0
end

puts "avg = #{sum.to_f / prefix_count.to_f}"
