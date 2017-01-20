#!/usr/bin/env python3
from urllib.parse import urlencode
from urllib.request import Request, urlopen
import json
import sys

request = Request("%s/_api/ws/%s/commands" % (sys.argv[1], sys.argv[2]))
res = urlopen(request).read().decode()

commands = json.loads(res)["commands"]
count = len(commands)
i = 0

for c in commands:
    i += 1
    print("[%s/%s] Deleting document id%s.." % (i, count, c["id"]))
    del_req = Request("%s/_api/ws/%s/doc/%s" % (sys.argv[1],
                                                sys.argv[2],
                                                c["id"]))
    del_req.get_method = lambda: "DELETE"
    urlopen(del_req)
