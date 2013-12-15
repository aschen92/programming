# Aaron Schendel
# received help from Brett Peterson

#!/usr/bin/env python
# file: shell-shell.py

import os


# Modify this function to implement your shell
#   This version just prints out the command line arguments.
def execute(args):
    wait = True
    if args[-1] == '&':
        args = args[:-1]
        wait = False
     
    pid = os.fork()
   
    if pid == 0:
        os.system(''.join(args))

    else:
        if wait:
            os.wait()
    
    

def main():
    while True: # An infinite loop
        try:
            user = os.getlogin()
            commandLine = input("{0}% ".format(user))
        except:
            break  # Quit shell on interrupt or end-of-file
        if commandLine == "exit":
            break  # Quit shell on exit
        if commandLine == "" :
            continue # Reprint prompt on blank line.
        if commandLine.strip() != "":
            # split at spaces to create list of strings.
            args = commandLine.split() 
            # pass the list of strings to the execute command.
            execute(args)


main()

