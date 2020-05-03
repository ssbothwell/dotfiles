#!/usr/bin/env python
"""
ACPI Power Status Script **Deprecated**

"""
from math import floor

EMPTY = ""
QUARTER = ""
HALF = ""
THREEQUARTERS = ""
FULL = ""
BOLT = ""

def calc_percent(current, capacity):
    """ calculate the batteries charge """
    return floor((current/capacity) * 100)

def charge_status(status):
    """ Check charge status """
    return BOLT if status == "Charging" else ""

def battery_icon(percent):
    """ Return battery icon """
    if percent == 0:
        return EMPTY
    if percent < 26:
        return QUARTER
    if percent < 51:
        return HALF
    if percent < 75:
        return THREEQUARTERS
    return FULL


with open('/sys/class/power_supply/BAT1/energy_full', 'r') as F:
    CAPACITY_EXT = int(F.readline().strip())
with open('/sys/class/power_supply/BAT1/energy_now', 'r') as F:
    CURRENT_EXT = int(F.readline().strip())
with open('/sys/class/power_supply/BAT1/status', 'r') as F:
    STATUS_EXT = F.readline().strip()

with open('/sys/class/power_supply/BAT0/energy_full', 'r') as F:
    CAPACITY_INT = int(F.readline().strip())
with open('/sys/class/power_supply/BAT0/energy_now', 'r') as F:
    CURRENT_INT = int(F.readline().strip())
with open('/sys/class/power_supply/BAT0/status', 'r') as F:
    STATUS_INT = F.readline().strip()

PERCENT_EXT = calc_percent(CURRENT_EXT, CAPACITY_EXT)
PERCENT_INT = calc_percent(CURRENT_INT, CAPACITY_INT)
AC_EXT = charge_status(STATUS_EXT)
AC_INT = charge_status(STATUS_INT)
BATTERY_EXT = battery_icon(PERCENT_EXT)
BATTERY_INT = battery_icon(PERCENT_INT)

print(f"{AC_INT} {BATTERY_INT} {PERCENT_INT}% {AC_EXT} {BATTERY_EXT} {PERCENT_EXT}%")
