# I verify that I am the sole author of the source code contained in this file, 
# except where explicitly stated to the contrary.
#
# The following code is used to collect data for NFA based regular expression matcher.
#
# Python standard libraries re and timeit are used to conduct experiments.
#
# Author: Arshdeep Singh Pareek
# Date: April 9, 2021

import re
import timeit

for x in range(0, 150, 10):
    time = 0
    for y in range(5):
        
        regex = re.compile("(a*)*b")
        #regex = re.compile("((a?){" + str(x) + "})((a){" + str(x) + "})")
        #regex = re.compile("(a|aa)*")
        #regex = re.compile("(a+a+)+b")
        start = timeit.default_timer()
        print(regex.match(("a"*x)))
        stop = timeit.default_timer()

        time += (stop-start)

    print('Time: ', time/5)  