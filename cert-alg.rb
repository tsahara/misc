#!/usr/bin/env ruby

require "openssl"
require "socket"
require "resolv"
require "timeout"

# US 50
us = <<SITES.downcase
Google.com
Youtube.com
Amazon.com
Facebook.com
Yahoo.com
Reddit.com
Wikipedia.org
Ebay.com
Netflix.com
Bing.com
Office.com
Live.com
Twitch.tv
Myshopify.com
Livejasmin.com
Espn.com
Chase.com
Microsoftonline.com
Instagram.com
Cnn.com
Tmall.com
Linkedin.com
Twitter.com
Bongacams.com
Microsoft.com
Dropbox.com
Instructure.com
Apple.com
Nytimes.com
Pornhub.com
Adobe.com
Craigslist.org
Force.com
Imdb.com
Zillow.com
Walmart.com
Intuit.com
Wellsfargo.com
Qq.com
Sohu.com
Spotify.com
Aliexpress.com
Indeed.com
Paypal.com
Hulu.com
Msn.com
Imgur.com
Login.tmall.com
Salesforce.com
Stackoverflow.com
SITES

# Japan 50
japan = <<SITES.downcase
Google.com
Youtube.com
Yahoo.co.jp
Amazon.co.jp
Google.co.jp
Rakuten.co.jp
Facebook.com
Wikipedia.org
Tmall.com
Yahoo.com
Qq.com
Sohu.com
Google.com.sg
Amazon.com
Login.tmall.com
Fc2.com
Taobao.com
Twitter.com
Baidu.com
Nicovideo.jp
Jd.com
360.cn
Apple.com
Instagram.com
Microsoft.com
Live.com
Ameblo.jp
Netflix.com
Ebay.com
Lazada.sg
Office.com
Pages.tmall.com
Asos.com
Dmm.co.jp
Sina.com.cn
Adobe.com
Weibo.com
Pornhub.com
Dropbox.com
Mercari.com
Msn.com
Kakaku.com
Bing.com
Livedoor.jp
Qoo10.sg
Stackoverflow.com
Dbs.com.sg
Line.me
Dmm.com
Goo.ne.jp
SITES

china = <<SITES.downcase
Tmall.com
Baidu.com
Qq.com
Sohu.com
Login.tmall.com
Taobao.com
360.cn
Jd.com
Weibo.com
Sina.com.cn
Pages.tmall.com
Xinhuanet.com
Alipay.com
Csdn.net
Google.com.hk
Zhanqi.tv
Tianya.cn
Panda.tv
China.com.cn
Babytree.com
Mama.cn
Soso.com
Huanqiu.com
Yy.com
So.com
Detail.tmall.com
17ok.com
Hao123.com
Sogou.com
Jrj.com.cn
Bilibili.com
Yao.tmall.com
Google.cn
1688.com
Caijing.com.cn
Gome.com.cn
6.cn
Cnblogs.com
Iqiyi.com
Rednet.cn
Google.com
3c.tmall.com
Scol.com.cn
Gmw.cn
Jiameng.com
Aliyun.com
Zhihu.com
Eastday.com
Uniqlo.tmall.com
Food.tmall.com
SITES

sites = japan

sites.split("\n").each { |domain|
  host = "www." + domain
  begin
    addr = Resolv.getaddress(host)
  rescue Resolv::ResolvError
    host = domain
    addr = Resolv.getaddress(host)
  end

  pkey = nil
  begin
    Timeout.timeout(10) {
      sock = TCPSocket.new(addr, 443)
      ctx = OpenSSL::SSL::SSLContext.new
      ctx.min_version = :TLS1_2
      ctx.ciphers = 'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256'
      ssl = OpenSSL::SSL::SSLSocket.new(sock, ctx)
      ssl.hostname = host
      ssl.connect
      pkey = ssl.peer_cert.public_key
      sock.close
    }
  rescue
    pkey = nil
  end
  ssl.close rescue nil
  sock.close rescue nil

  printf "%-25s %-16s %-s\n", host, addr, pkey.class
}
