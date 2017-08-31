#!/usr/bin/env ruby

def check_loop(paths)
  lp = false
  len = paths.size
  paths.each_index { |i|
    as = paths[i]
    j = i + 1
    j += 1 while paths[j] == as
    while j < len
      if paths[j] == as
        puts "#{as} <- #{paths}"
        lp = true
      end
      j += 1
    end
  }
  lp
end

routes = loop_routes = 0

5.times { gets }
while l = gets
  routes += 1
  a = l.split
  if a[0] == "*"
    a.shift(6)
    if check_loop(a)
      loop_routes += 1
    end
    #    Network            Next Hop            Metric LocPrf Weight Path
    # *  1.177.160.0/22     185.44.116.1             0      0      0 47872 15412 9848 38091 38091 38091 38091 38091 38091 9845 9697 i

  else
    puts "first column is not * : #{l.strip}"
    exit
  end

  if routes % 1000 == 0
    ratio = loop_routes.to_f / routes
    puts "#{loop_routes} / #{routes} routes = #{ratio}"
  end
end
