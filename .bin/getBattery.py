#!/usr/bin/env python
"""
ACPI Power Status Script
"""
import os
from math import floor

with open('/sys/class/power_supply/BAT1/energy_full', 'r') as F:
    capacity = int(F.readline().strip())
with open('/sys/class/power_supply/BAT1/energy_now', 'r') as F:
    current = int(F.readline().strip())
with open('/sys/class/power_supply/BAT1/status', 'r') as F:
    status = F.readline().strip()

empty           = ""
quarter         = ""
half            = ""
threequarters   = ""
full            = ""

bolt            = ""
percent = floor((current/capacity) * 100)

if status == "Charging": AC = bolt
else: AC = ""

if percent == 0:   battery = empty
elif percent < 26: battery = quarter
elif percent < 51: battery = half
elif percent < 75: battery = threequarters
else:              battery = full
print(f"{AC}{battery} {percent}%")
