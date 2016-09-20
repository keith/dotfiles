#!/usr/bin/env python
#
# Change caps lock to escape on built in keyboards
#
# https://github.com/tekezo/Seil/issues/68#issuecomment-229260237
# Must be run with sudo, SIP must be disabled, requires restart
#

from xml.etree import ElementTree
import argparse
import os
import subprocess

# Find this product and the vendor ID by checking "System Information" -> USB -> Apple Internal Keyboard / Trackpad
PRODUCT_ID = 610
# Find this by cross referencing the vendor and product ID (in base 10) with
# what's in this plist. If you can't find it here check either of these:
#
# /System/Library/Extensions/AppleHIDKeyboard.kext/Contents/Info.plist
# /System/Library/Extensions/AppleTopCase.kext/Contents/PlugIns/AppleTopCaseHIDEventDriver.kext/Contents/Info.plist
#
PLIST_PATH = "/System/Library/Extensions/AppleUSBTopCase.kext/Contents/PlugIns/AppleUSBTCKeyEventDriver.kext/Contents/Info.plist"

HEADER = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">"""

plist = ElementTree.parse(PLIST_PATH)
driverdef = plist.findall(".//*[key='ProductID']/[integer='%d']"
                          % PRODUCT_ID)[0]

ElementTree.SubElement(driverdef, 'key').text = 'KeyboardUsageMap'

map_dict = ElementTree.SubElement(driverdef, 'dict')
ElementTree.SubElement(map_dict, 'key').text = '0x00070039'
ElementTree.SubElement(map_dict, 'integer').text = '0x00070029'

os.rename(PLIST_PATH, PLIST_PATH + '.bak')
with open(PLIST_PATH, 'w') as f:
    treestr = ElementTree.tostring(plist.getroot(),
                                   encoding='UTF-8').replace(' />', '/>')
    f.write("%s\n%s" % (HEADER, treestr))

subprocess.call("touch /System/Library/Extensions && sudo kextcache -update-volume /".split())
