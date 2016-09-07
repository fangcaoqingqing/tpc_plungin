import urllib
url = '''http://10.136.58.197:8086/sdk/platform/login/api/login?jsonData={channel:'xm',appid:'2882303761517480120',uid:'83458722',token:'IghChMuadhYi8n9G',serverIndex:'1'}'''

req = urllib.urlopen(url)

print req.read()

