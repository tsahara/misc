#!/usr/bin/env ruby

require 'serialport'

def show line
  a = line.split(",")
  case a[0][3..5]
  when "GGA"
    fix = case a[6]
          when "0"
            "invalid"
          when "1"
            "GPS"
          when "2"
            "DGPS"
          else
            a[6]
          end

    pos = "%s%s/%s%s" % [a[3], a[2], a[5], a[4]]
    
    puts "GGA: satelites=#{a[7]}, fix=#{fix}, #{pos}"
  when "RMC"
    if a[2] == "A"
      /(\d\d)(\d\d)(\d\d)(\.\d+)?/ =~ a[1]
      t = "#{$1}:#{$2}:#{$3}#{$4}"

      pos = "%s%s/%s%s" % [a[4], a[3], a[6], a[5]]

      /(\d\d)(\d\d)(\d\d)/ =~ a[9]
      d = "20#{$1}/#{$2}/#{$3}"

      puts "RMC: date=#{d} #{t}, pos=#{pos}, speed=#{a[7]}"
    else
      puts "RMC: (data invalid)"
    end
  when "GSV"
    no = "#{a[2]}/#{a[1]}"

    sat = []
    ((a.length-4) / 4).times { |i| sat << a[4+i*4] }

    puts "GSV: #{no}, view=#{a[3]}, satelites=#{sat.join(",")}"
  else
    puts "  (#{a[0]})"
  end
end

nmea = SerialPort.new("/dev/tty.usbserial", { "baud" => 38400 })
while l = nmea.gets
  show l
end
