#!/usr/bin/env ruby

def check_loop(paths)
  len = paths.size
  paths.each_index { |i|
    as = paths[i]
    j = i + 1
    j += 1 while paths[j] == as
    while j < len
      if paths[j] == as
        puts "*#{as}* <- #{paths}"
      end
      j += 1
    end
  }
end

5.times { gets }
while l = gets
  a = l.split
  if a[0] == "*"
    a.shift(6)
    check_loop a
    #    Network            Next Hop            Metric LocPrf Weight Path
    # *  1.177.160.0/22     185.44.116.1             0      0      0 47872 15412 9848 38091 38091 38091 38091 38091 38091 9845 9697 i

  else
    puts "first column is not * : #{l.strip}"
    exit
  end
end
