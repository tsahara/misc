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
p base64url(0)
exit

def jws(protected_header, paylaod, jwa)
  jws = {}
  jws["payload"]   = base64url(payload.to_json)
  jws["protected"] = base64url(protected_header.to_json)
  jws["signature"] = jwa.sign(jws["payload"] + "." + jws["payload"])
  jws.to_json
end

if true
  BASE64URL_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "abcdefghijklmnopqrstuvwxyz" + "0123456789-_"

  ph = <<JSON.rstrip.gsub("\n", "\r\n")
{"typ":"JWT",
 "alg":"HS256"}
JSON
p base64url(ph)
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
