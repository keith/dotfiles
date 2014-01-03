#!/usr/bin/env python

import os
import re
import subprocess
import sys


def get_env(var="EP"):
    return os.environ.get(var)


def get_keychain_pass(account=None, server=None):
    if not sys.platform == 'darwin':
        return get_env()

    home = os.environ.get('HOME')
    user = os.environ.get('USER')

    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
        'keychain': home + '/Library/Keychains/login.keychain',
        'user': user,
    }
    command = "sudo -u %(user) %(security)s -v %(command)s -g -a %(account)s -s %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)

