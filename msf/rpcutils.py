# -*- coding:utf-8 -*-

import os
import msfrpc
import argparse
import curses
import time
import cmd
import sys
from threading import Timer


def login():
    try:
        client = msfrpc.Msfrpc({
            "host": os.environ.get('MSFRPC_HOST'),
            "port": os.environ.get('MSFRPC_PORT'),
            "uri": os.environ.get("MSFRPC_URI"),
            "ssl": os.environ.get("MSFRPC_SSL") == "true"
        })
        client.login(os.environ.get('MSFRPC_USER'),
                     os.environ.get('MSFRPC_PASS'))
    except Exception:
        print 'Cannot connect.. sorry.'
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
    parser.add_argument('item', help='Item to select in commands',
                        metavar='I', type=str, nargs="?")
    parser.add_argument('-c', '--current', required=False, action="store_true",
                        help='Print results only for current workspace')
    parser.add_argument('-w', '--workspace', required=False,
                        help='Print results only for specific workspace')
    parser.add_argument('-f', '--follow', required=False, action="store_true",
                        help='Start a ncurses interface')
    parser.add_argument('-l', '--list', required=False, action="store_true",
                        help='Listing output (no extra line)')
    return parser.parse_args()


def parse_module_opts(options):
    opts = {}
    for o in options.split(";;"):
        o = o.split("=>")
        opts[o[0]] = o[1]


def ncurses(collector):
    w = curses.initscr()
    try:
        while True:
            try:
                w.erase()
                w.addstr(collector())
            except Exception:
                pass
            finally:
                w.refresh()
                time.sleep(3)
    finally:
        curses.endwin()


class MsfCmd(cmd.Cmd):
    def __init__(self):
        self.locked = False
        self.client = login()
        self.set_timer()
        cmd.Cmd.__init__(self)

    def read_data(self):
        return None

    def set_timer(self):
        self.refresh_timer = Timer(10.0, self.refresh)
        self.refresh_timer.start()

    def wait_lock(self):
        while True:
            if not self.locked:
                break
            time.sleep(2)

    def lock(self):
        self.locked = True

    def unlock(self):
        self.locked = False

    def refresh(self):
        self.wait_lock()
        self.lock()
        try:
            res = self.read_data()
            if res.get("data") and len(res.get("data")) > 0:
                print res.get("data")
                sys.stdout.write(self.prompt)
                sys.stdout.flush()
        except Exception as e:
            print e.message
        finally:
            self.unlock()
            self.set_timer()

    def onecmd(self, cmd):
        return False

    def do_help(self):
        self.onecmd("help")

    def do_exit(self, s):
        self.refresh_timer.cancel()
        return True
    do_EOF = do_exit
