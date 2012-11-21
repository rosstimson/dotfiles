#!/usr/local/bin/python

# Simple wrapper for offlineimap to grab password stored in Pass - The Standard
# Unix Password Manager available at: http://zx2c4.com/projects/password-store
#
# Passwords are called by running the command 'pass mypassword', this just
# wraps that with Python as you cannot seem to put shell commands in 
# .offlineimaprc file.

import subprocess

def grab_secret(secret):
  return(subprocess.check_output(["pass", secret])).rstrip('\n')
