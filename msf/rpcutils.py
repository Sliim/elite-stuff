import os
import msfrpc
import argparse
import curses

def login():
    try:
        client = msfrpc.Msfrpc({
            "host": os.environ.get('MSFRPC_HOST'),
            "port": os.environ.get('MSFRPC_PORT'),
            "uri": os.environ.get("MSFRPC_URI"),
            "ssl": os.environ.get("MSFRPC_SSL") == "true"
        })
        client.login(os.environ.get('MSFRPC_USER'), os.environ.get('MSFRPC_PASS'))
    except Exception:
        print "Cannot connect.. sorry."
        return False
    return client

def check_resp(resp):
    if "error" in resp:
        print "ERROR! %s: %s" % (resp["error_class"],
                                 resp["error_message"])
        return False
    return True

def parse_args():
    parser = argparse.ArgumentParser(description='MSF RPC Stuff!')
    parser.add_argument('item', help='Item to select in commands', metavar='I', type=str, nargs="?")
    parser.add_argument('-c','--current', help='Print results only for current workspace', required=False, action="store_true")
    parser.add_argument('-w','--workspace', help='Print results only for specific workspace', required=False)
    parser.add_argument('-f','--follow', help='Start a ncurses interface', required=False, action="store_true")
    parser.add_argument('-l','--list', help='Listing output (no extra line)', required=False, action="store_true")
    return parser.parse_args()

def ncurses(collector):
    w = curses.initscr()
    try:
        while True:
            try:
                w.erase()
                w.addstr(collector())
                time.sleep(3)
            except Exception:
                pass
            w.refresh()
    finally:
        curses.endwin()
