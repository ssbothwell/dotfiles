#!/usr/bin/python
import os
import subprocess

def main():
    """ Main execution Function """

    submenus = {
        'xcape': ['start', 'stop', 'restart'],
        'screens': ['laptop', 'external']
    }
    single_items = ['initialize keyboard']
    menu = list(submenus.keys())

    menu = {
        'xcape': {
            'start': 'systemctl --user start xcape.service',
            'stop': 'systemctl --user stop xcape.service',
            'restart': 'systemctl --user restart xcape.service'
        },
        'screens': {
            'laptop': 'screenToggle laptop',
            'external': 'screenToggle external'
        },
        'xmonad': {
            'recompile': 'xmonad --recompile && xmonad --restart',
            'restart': 'xmonad --restart'
        },
        'initialize keyboard': 'init-keyboard.sh',
    }

    while True:
        menu_string = '\n'.join(menu)
        command = "echo -e '%s' | dmenu -no-case-sensitive true" % menu_string,
        p = subprocess.Popen(
            command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        for line in p.stdout.readlines():
            choice = line.decode('utf-8').strip()
        p.wait()

        if type(menu[choice]) == dict:
            menu = menu[choice]
        else:
            if menu[choice] is None:
                # execute choice
                subprocess.run(choice, shell=True)
                break
            else:
                # execute menu[choice]
                subprocess.run(menu[choice], shell=True)
                break


if __name__ == '__main__':
    print(main())
