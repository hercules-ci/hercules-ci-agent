"""HTTP server for the `hci curl` tests in cli.nix.

Prints the port it listens on. Answers each request with a JSON object of the
request's path and Authorization header. A request for /silent gets no answer:
the server prints that object instead and waits, so that the test can
interrupt curl while it waits.
"""

import http.server
import json
import time


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        request = {"path": self.path, "authorization": self.headers.get("Authorization")}
        body = json.dumps(request, separators=(",", ":"))
        if self.path == "/silent":
            print(body, flush=True)
            time.sleep(600)
            return
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body.encode())


server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
