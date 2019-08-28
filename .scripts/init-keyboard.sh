#!/bin/sh


# Generate a custom xkb file in /tmp/ with hyper keys and backslash remapped
# Copied from Ethan Schoonovers's script:
# https://github.com/altercation/dotfiles-tilingwm/blob/master/bin/wm/init-keyboard

# old symbols for reference:
#xkb_symbols   { include "pc+us+us:2+inet(evdev)+group(shifts_toggle)+level3(ralt_switch_multikey)+ctrl(nocaps)+typo(base):1+typo(base):2+custom(hypers)" };

# Dump config to a file:
XKBDIR=/tmp/xkb
[ -d ${XKBDIR}/symbols ] || mkdir -p ${XKBDIR}/{keymap,symbols}

cat > $XKBDIR/keymap/custom.xkb << EOF
xkb_keymap {
    xkb_keycodes  { include "evdev+aliases(qwerty)"	};
    xkb_types     { include "complete"	};
    xkb_compat    { include "complete"	};
    xkb_symbols   { include "pc+us+us:2+inet(evdev)+level3(ralt_switch_multikey)+ctrl(nocaps)+typo(base):1+typo(base):2+custom(hypers)" };
    xkb_geometry  { include "pc(pc104)"	};
};
EOF

cat > $XKBDIR/symbols/custom << EOF
default partial
xkb_symbols "hypers" {
    key  <TAB> { [ Hyper_L, Hyper_L ] };
    key <BKSL> { [ Hyper_R, Hyper_R ] };
    key <I252> { [ Tab,	ISO_Left_Tab ] };
    key <I253> { [ backslash, bar ] };
    modifier_map Mod4 { Super_L, Super_R, Hyper_L, Hyper_R };
};
EOF


# Reinitialize keyboard
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$XUSERID/bus"
notify-send -u low "Keyboard initialized"
xkbcomp -synch -w3 -I$XKBDIR $XKBDIR/keymap/custom.xkb $DISPLAY &>/dev/null
#(exec killall -q xcape) & # gets restarted by the xcape systemd user service
(exec /usr/bin/xcape -e "Hyper_L=Tab;Hyper_R=backslash;Control_L=Escape")
echo "keyboard initialized"
