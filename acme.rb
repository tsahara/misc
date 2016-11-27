#!/usr/bin/env ruby

require 'base64'
require 'json'
require 'openssl'

ACME_URL = "https://acme-staging.api.letsencrypt.org/directory"

def base64url(s)
  if s.is_a? Integer
    Base64.urlsafe_encode64(s.to_s)
  else
    Base64.urlsafe_encode64(s).sub(/=+$/, '')
  end
end

def base64tobn(s)
  Base64.urlsafe_decode64(s).unpack("C*").inject { |n,k| n*256+k }
end

def jws(protected_header, paylaod, jwa)
  jws = {}
  jws["payload"]   = base64url(payload.to_json)
  jws["protected"] = base64url(protected_header.to_json)
  jws["signature"] = jwa.sign(jws["payload"] + "." + jws["payload"])
  jws.to_json
end

# RFC7515 A.1
if false
  ph = <<JSON.rstrip.gsub("\n", "\r\n")
{"typ":"JWT",
 "alg":"HS256"}
JSON

  payload = <<JSON.rstrip.gsub("\n", "\r\n")
{"iss":"joe",
 "exp":1300819380,
 "http://example.com/is_root":true}
JSON

  p base64url(payload)

  input = base64url(ph) + "." + base64url(payload)
  puts "input = #{input.inspect}"

  kstr = "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"
  k = Base64.urlsafe_decode64(kstr)
  d = OpenSSL::HMAC.digest("sha256", k, input)

  puts input + "." + base64url(d)
  exit

  payload = <<JSON.rstrip.gsub("\n", "\r\n")
{"iss":"joe",
 "exp":1300819380,
 "http://example.com/is_root":true}
JSON

  data = base64url(ph) + "." + base64url(payload)
  p OpenSSL::HMAC.digest("sha256", "key", data)

  p Base64.urlsafe_decode64("eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ")
end

# RFC7515 A.2
if true
  ph = '{"alg":"RS256"}'
  payload = <<JSON.rstrip.gsub("\n", "\r\n")
{"iss":"joe",
 "exp":1300819380,
 "http://example.com/is_root":true}
JSON
  input = base64url(ph) + "." + base64url(payload)

  jwk = {"kty"=>"RSA",
         "n"=>"ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx"+
             "HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs"+
             "D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH"+
             "SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV"+
             "MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8"+
             "NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ",
         "e"=>"AQAB",
         "d"=>"Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97I"+
             "jlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0"+
             "BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn"+
             "439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYT"+
             "CBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLh"+
             "BOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ",
         "p"=>"4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdi"+
             "YrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPG"+
             "BY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc",
         "q"=>"uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxa"+
             "ewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA"+
             "-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc",
         "dp"=>"BwKfV3Akq5_MFZDFZCnW-wzl-CCo83WoZvnLQwCTeDv8uzluRSnm71I3Q"+
             "CLdhrqE2e9YkxvuxdBfpT_PI7Yz-FOKnu1R6HsJeDCjn12Sk3vmAktV2zb"+
             "34MCdy7cpdTh_YVr7tss2u6vneTwrA86rZtu5Mbr1C1XsmvkxHQAdYo0",
         "dq"=>"h_96-mK1R_7glhsum81dZxjTnYynPbZpHziZjeeHcXYsXaaMwkOlODsWa"+
             "7I9xXDoRwbKgB719rrmI2oKr6N3Do9U0ajaHF-NKJnwgjMd2w9cjz3_-ky"+
             "NlxAr2v4IKhGNpmM5iIgOS1VZnOZ68m6_pbLBSp3nssTdlqvd0tIiTHU",
         "qi"=>"IYd7DHOhrWvxkwPQsRM2tOgrjbcrfvtQJipd-DlcxyVuuM9sQLdgjVk2o"+
             "y26F0EmpScGLq2MowX7fhd_QJQ3ydy5cY7YIBi87w93IKLEdfnbJtoOPLU"+
             "W0ITrJReOgo1cq9SbsxYawBgfp_gh6A5603k2-ZQwVK0JKSHuLFkuQ3U"
  }
  rsa = OpenSSL::PKey::RSA.new
  rsa.e = base64tobn(jwk["e"])
  rsa.d = base64tobn(jwk["d"])
  rsa.n = base64tobn(jwk["n"])
  rsa.p = base64tobn(jwk["p"])
  rsa.q = base64tobn(jwk["q"])
  rsa.dmp1 = base64tobn(jwk["dp"])
  rsa.dmq1 = base64tobn(jwk["dq"])
  rsa.iqmp = base64tobn(jwk["qi"])

  sign = base64url(rsa.sign("sha256", input))
  p sign

  exit
end

def get_directory
  #ACME_URL
  str = <<-JSON
  {
    "key-change": "https://acme-staging.api.letsencrypt.org/acme/key-change",
    "new-authz": "https://acme-staging.api.letsencrypt.org/acme/new-authz",
    "new-cert": "https://acme-staging.api.letsencrypt.org/acme/new-cert",
    "new-reg": "https://acme-staging.api.letsencrypt.org/acme/new-reg",
    "revoke-cert": "https://acme-staging.api.letsencrypt.org/acme/revoke-cert"
  }
  JSON
  JSON.parse(str)
end

$dir = get_directory
p $dir

# このペイロードを作るために JWS のスペックを読む
# ES256 = prime256v1
a = <<EOF
{
  "protected": base64url({
    "alg": "ES256",
    "jwk": {...},
    "nonce": "6S8IqOGY7eL2lsGoTZYifg",
    "url": "https://example.com/acme/new-reg"
  })
  "payload": base64url({
    "contact": [
      "mailto:cert-admin@example.com",
      "tel:+12025551212"
    ]
  }),
  "signature": "RZPOnYoPs1PhjszF...-nh6X1qtOFPB519I"
}
EOF
