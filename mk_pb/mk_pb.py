#coding=utf-8
import nclogin_pb2
from socket import *
import urllib
import json

url = '''http://10.136.58.197:8086/sdk/platform/login/api/login?jsonData={channel:'xm',appid:'2882303761517480120',uid:'83458722',token:'IghChMuadhYi8n9G',serverIndex:'1'}'''
req = urllib.urlopen(url)
ret = req.read()
ret_dic = json.loads(ret)


login  = nclogin_pb2.NcLogin()
login.AccountId = "57b554c2e4b057a05e104548"
login.SessionId = ret_dic["resultCode"]["access_token"]
login.ServerId = 1
login_string =  login.SerializeToString()
f = open('nclogin','wb')
f.write(login_string)
f.close()

print login_string
