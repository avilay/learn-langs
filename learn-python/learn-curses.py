import curses
import time


stdscr = curses.initscr()
curses.start_color()

stdscr.clear()

for i in range(0, 6):
    stdscr.addstr(0, 0, 'Value of i is {}'.format(i))
    stdscr.refresh()
    time.sleep(0.5)

stdscr.addstr(2, 1, 'This is a bold statement', curses.A_BOLD)
stdscr.addstr(4, 1, 'Look at me! Look at me!', curses.A_BLINK)
stdscr.refresh()
time.sleep(2)

stdscr.addstr(6, 2, 'Pretty text', curses.color_pair(1))
stdscr.refresh()
time.sleep(2)

curses.init_pair(1, curses.COLOR_RED, curses.COLOR_WHITE)
stdscr.addstr(8, 2, 'RED ALERT!', curses.color_pair(1))
stdscr.refresh()
time.sleep(2)

curses.endwin()
