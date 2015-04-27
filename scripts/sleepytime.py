#!/usr/bin/python2
import time

now = time.localtime()
(shour, smin) = (now.tm_hour, now.tm_min)

smin += 14
if smin > 60:
    shour += 1
    smin %= 60

hours = ['1.5 hrs', '3.0 hrs', '4.5 hrs', '6.0 hrs', '7.5 hrs', '9.0 hrs']
for sleepCycle in xrange(6):
    shour += 1
    smin += 30
    if smin > 60:
        shour += 1
        smin %= 60
    print "%s\t %02d:%02d" % (hours[sleepCycle], (shour % 24), smin)
