#!/usr/bin/env ruby

require 'openssl'


def base32decode(str)
  str = str.downcase
  alphabet = 'abcdefghijklmnopqrstuvwxyz234567'
  result = ""
  num = 0
  bitoffs = 0
  str.length.times { |idx|
    k = alphabet.index(str[idx])

    if bitoffs < 3
      num |= k << (3 - bitoffs)
      bitoffs += 5
    else
      num |= k >> (bitoffs - 3)
      result += [num].pack("C")
      rbits = (bitoffs + 5) - 8
      if rbits == 0
        num = 0
      else
        num = (k & ((1 << rbits) - 1)) << (8 - rbits)
      end
      bitoffs = rbits
    end
  }
  result
end

def hotp(k, c)
  hs = OpenSSL::HMAC.digest('sha1', k, [c].pack("Q>"))
  offset = hs[19].unpack("C")[0] & 0x0f
  snum = hs[offset, 4].unpack("N")[0] & 0x7fffffff
  return snum % 1000000
end

def totp(k)
  x = 30
  unixtime = (Time.now - Time.utc(1970, 1, 1)).to_i
  t = unixtime / x
  puts "unixtime = #{unixtime}"
  puts "t = #{t}"
  hotp(k, t)
end

p totp(base32decode("HXDMVJECJJWSRB3HWIZR4IFUGFTMXBOZ"))
