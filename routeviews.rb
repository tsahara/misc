#!/usr/bin/env ruby

def check_loop(paths)
  lp = []
  len = paths.size
  paths.each_index { |i|
    as = paths[i]
    j = i + 1
    j += 1 while paths[j] == as
    j0 = j
    while j < len
      if paths[j] == as
        #puts "#{as}, #{paths.join(" ")}"
        lp << as
        break
      end
      j += 1
    end
  }
  lp
end

routes = loop_routes = 0

$first_prefix_only = true
prefix_hash = {}
asn = {}

5.times { gets }
while l = gets
  a = l.split

  raise "first column is not * : #{l.strip}" if a[0] != "*"

  #next if prefix_hash.has_key? a[1]
  #$stdout.write "\r#{a[1]}"
  #$stdout.flush
  prefix_hash[a[1]] = true

  a.shift(6)
  a.each { |as| asn[as] ||= false }
  as_list = check_loop(a)
  if as_list.size > 0
    loop_routes += 1
    as_list.each { |as| asn[as] = true }
  end
  #    Network            Next Hop            Metric LocPrf Weight Path
  # *  1.177.160.0/22     185.44.116.1             0      0      0 47872 15412 9848 38091 38091 38091 38091 38091 38091 9845 9697 i

  routes += 1
  if routes % 100000 == 0
    ratio = loop_routes.to_f / routes
    puts "#{loop_routes} / #{routes} routes = #{ratio}"
  end
end

ratio = loop_routes.to_f / routes
puts "#{loop_routes} / #{routes} routes = #{ratio}"

puts "Number of ASN = #{asn.size}"

looped = asn.select { |k,v| v }
puts "Looped ASN: #{looped.keys.size}"
