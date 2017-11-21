import random
shape_ = 1.5

for x in range(1,100):
    next_burstlen = int(random.paretovariate(shape_)+0.5)
    if (next_burstlen == 0):
        next_burstlen = 1
        pass
    print ('Burstlen{}: '.format(x), next_burstlen)


    next_idle_time = random.paretovariate(shape_)
    print ('Idletime{}: '.format(x),next_idle_time)
    pass
